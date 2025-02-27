# imports
import os
import pandas as pd
from sqlalchemy import create_engine
from datetime import datetime
from arcgis.gis import GIS

from arcutils import update_layer_from_dataframe

# Connect to AGOL and SMC database
gis_username = os.environ.get('GIS_EDITOR_USERNAME')
gis_password = os.environ.get('GIS_EDITOR_PASSWORD')
gis = GIS("https://www.arcgis.com",gis_username,gis_password)
eng = create_engine(os.environ.get('SMC_DB_CONNECTION_STRING'))

# grab data from SMC database, most recent sampling event
qry = 'SELECT * FROM vw_rsca_loe_mod_sum'

modsum = pd.read_sql(qry, eng)
modsum.drop('objectid', axis = 'columns', inplace = True)

modsum.rename(columns = {'test_site':'masterid','test_csci_sampleid':'csci_sampleid'}, inplace = True)


# We know there is only one layer in that item
layer = gis.content.search(f"id:{os.environ.get('RSCA_MODSUM_LAYER_ID')}")[0].layers[0]

update_layer_from_dataframe(modsum, layer, mergecols=['masterid'])

today = datetime.today().strftime('%Y-%m-%d %H:%M:%S')
eng.execute(f"UPDATE rsca_latest_updates SET layer_latest_update = '{today}' WHERE appname = 'sgrrmp_rsca';")







