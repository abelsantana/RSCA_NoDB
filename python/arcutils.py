import json, pandas as pd
from socketserver import ThreadingUDPServer
from arcgis.geometry import Point
from arcgis.features import Feature

# function to sync a pandas dataframe to a feature layer
def update_layer_from_dataframe(
    df, layer, mergecols, 
    uniqueidcol = 'ObjectId2', hasgeometry = True, latcol = 'latitude', longcol = 'longitude', deletemissing = False
):
    assert not df.empty, "cant update layer with empty dataframe"
    assert isinstance(mergecols, list), "Mergecols must be a list"

    layerdf = layer.query().sdf

    if not layerdf.empty:
        assert set(df.columns).issubset(set(layerdf.columns)), \
            "dataframe columns is not a subset of the columns in the feature layer's data."

        layerdf = layerdf.assign(layerflag = 'present')
        df = df.assign(newdataflag = 'present')

        merged = layerdf[[*mergecols, uniqueidcol, 'layerflag']].merge(df, on = mergecols, how = 'outer')

    else:
        merged = df
    
    if hasgeometry and not merged.empty:
        assert set([latcol, longcol]).issubset(set(merged.columns)), "Could not identify shape/lat/long columns for the new records"
        merged['SHAPE'] = merged.apply(
            lambda row: Point({"x" : row[longcol], "y" : row[latcol], "spatialReference" : {"wkid":4326}}), axis = 1
        )

    datecols = [c for c in merged.columns if 'date' in c]
    for col in datecols:
        try:
            merged[col] = merged[col].apply(lambda x: pd.Timestamp(x).strftime('%Y-%m-%d %H:%M:%S'))
        except Exception as e:
            print(f"Warning: Column {col} of datatype {str(merged[col].dtype)} was unable convert to a timestamp string")
            print(f"Error message: {e}")
    
    if not layerdf.empty:
        # in the feature layer but not the new data
        # remember deletes are basedon the objectid/unique id field
        deletes = merged[pd.isnull(merged.newdataflag)][uniqueidcol].tolist()

        merged.drop(merged[pd.isnull(merged.newdataflag)].index, inplace = True)

        # in the new data but not the feature layer
        addrecords = merged[pd.isnull(merged.layerflag)]
        addrecords.drop(['layerflag', 'newdataflag'], axis = 'columns', inplace = True)

        updaterecords = merged.drop(addrecords.index)
        updaterecords.drop(['layerflag', 'newdataflag'], axis = 'columns', inplace = True)

        updates = [
            Feature(
                attributes = {
                    k:v for k,v in rec.items() if k.upper() != 'SHAPE'
                },
                geometry = rec['SHAPE'].project_as({'latestWkid': 3857, 'wkid': 102100})
            ) 
            for rec in updaterecords.to_dict('records')
        ]
        adds = [
            Feature(
                attributes = {
                    k:v for k,v in rec.items() if k.upper() != 'SHAPE'
                },
                geometry = rec['SHAPE'].project_as({'latestWkid': 3857, 'wkid': 102100})
            ) 
            for rec in addrecords.to_dict('records')
        ]

    else:
        addrecords = merged
        updates = []
        deletes = []
        adds = [
            Feature(
                attributes = {
                    k:v for k,v in rec.items() if k.upper() != 'SHAPE'
                },
                geometry = rec['SHAPE'].project_as({'latestWkid': 3857, 'wkid': 102100})
            ) 
            for rec in addrecords.to_dict('records')
        ]

    print("updates")
    print(updates)
    print("adds")
    print(adds)
    print("deletes")
    print(deletes)

    result = layer.edit_features(adds = adds, updates = updates, deletes = deletes if deletemissing else [])
    print(result)
    return result


# The below code is from ESRI community, and it can serve as a routine for publishing a pandas dataframe to a hosted feature layer
# I am considering adding this as a method to the GeoDBDataFrame class
# https://community.esri.com/t5/arcgis-notebooks-questions/trying-to-create-a-feature-layer-from-a-pandas/td-p/162111
def publish_feature_layer(df, gis, title: str, description = '', tags = [], type = 'Feature Collection'):
    groups_stats_fc = gis.content.import_data(df)

    groups_stats_fc_dict = dict(groups_stats_fc.properties)
    groups_stats_fc_json = json.dumps({"featureCollection": {"layers": [groups_stats_fc_dict]}})

    groups_stats_item_properties = {
        'title'       : title,
        'description' : description ,
        'tags'        : ','.join(tags),
        'text'        : groups_stats_fc_json,
        'type'        : 'Feature Collection'
    }
    group_stats_item = gis.content.add(groups_stats_item_properties)
    group_stats_item.publish()