# This function is designed to import, clean, and assemble site/sample data for Rapid Screening Causual Assessment.
# This function creates final CSCI, Stressor, and Site/OE datasets and saves them locally as R Data files.
# Re-run this function to re-pull/re-fresh base datasets.
# Those datasets will be used and subsetted to create Comparator and Test Site datasets.
# Data is imported from SMC database, using in-house connection.
# Imported datasets are cleaned and wrangled to format required for subsequent RSCA functions.


#### WORKFLOW OVERVIEW ####

# 1) load libraries, connect to SMC database.
# 2) Use SQL to preform initial data queries
#       Datasets of interest:
#         + station information (lu_stations)
#         + California Stream Condition Index score (analysis_csci_core)
#         + Expected taxa and associated capture probabilities (analysis_csci_suppl1_oe)
#         + chemistry/water quality stressor data (unified_chemistry)
#         + Nutrient data (analysis_chem_nutrients)
#         + Index of Physical Integrity (IPI) scores for stressor data (analysis_phab_ipi)
#       Final Site Datasets:
#         + csci
#         + oe
#         + stressors
#         + stations
# 3) Clean each dataset individually.
#         General steps:
#           + retain masterid
#           + make sure any qa/qc data like duplicates, blanks are removed (this is first done in SQL, but confirm)
#           + all negative results to zero, all NDs to 0
#           + remove NA results
#           + confirm results/samples are suitable
# 4) Create final merged datasets, export for use in following scripts.


prep_smc_data <- function(con) {
  
  #### LOADING LIBRARIES, CONNECTING TO DATABASE ####
  
  # required packages 
  library(DBI) # needed to connect to database
  library(dbplyr) # needed to connect to database
  library(RPostgreSQL) # needed to connect to our database
  library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
  library(tidyverse) # loaded for data manipulation
  # library(lubridate)
  # library(naniar)
  library(cgwtools) #for dealing with RData files
  #library(pool)
  
  # database connection
  # con <- database_connection
  # con <- DBI::dbConnect(
  #   RPostgreSQL::PostgreSQL(),
  #   host = Sys.getenv('SMC_DB_HOST'),
  #   user = Sys.getenv('SMC_DB_USER'),
  #   password = Sys.getenv('SMC_DB_PASSWORD'),
  #   dbname = Sys.getenv('SMC_DB_NAME')
  # )
  #### STATION INFORMATION ####
  lustations_sql <- paste0("SELECT stationid, masterid, stationname, latitude, longitude, huc, county, psa_lu, comid FROM sde.lu_stations")
  # pull data, writing to tibble
  lustations_df <- tbl(con, sql(lustations_sql)) %>%
    as_tibble() %>% 
    # just in case, make sure no NA masterids
    filter(!is.na(masterid))
  #### Channel Engineering Information
  chansum_sql <- paste0("SELECT  masterid, channel_engineering_class  FROM sde.unified_channelengineering_summary")
  # pull data, writing to tibble
  chansum_df <- tbl(con, sql(chansum_sql)) %>%
    as_tibble() %>% 
    # just in case, make sure no NA masterids
    filter(!is.na(masterid)) %>% 
    rename(Class = channel_engineering_class, comp_site = masterid)
  
  #### CSCI DATA ####
  
  # create SQL for csci data pull
    csci_sql <- paste0("SELECT stationcode,sampleid,sampledate,collectionmethodcode,fieldreplicate,count,
    pcnt_ambiguous_individuals, pcnt_ambiguous_taxa,e, mean_o,oovere,oovere_percentile,
     mmi, mmi_percentile,csci,csci_percentile FROM sde.analysis_csci_core")
  # pull data, writing to tibble
  analysis_csci_df <- tbl(con, sql(csci_sql)) %>% 
    as_tibble() 
  # prep csci data for RSCA analysis
  # it is ok to have replicates for CSCI data, will be treated as unique sample events
  csci_clean_df <- analysis_csci_df %>% 
    #TODO 11/14/24 drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    inner_join(lustations_df %>% select(masterid,stationid), by = c("stationcode" = "stationid")) %>% #get masterid
    # create field that is T/F based on meeting conditions of a suitable sample
    # recent update: remove this data checking step, so that these sites can still pass through causual assessment
    # mutate(suitable_sample = ifelse(count >= 250 & pcnt_ambiguous_taxa <= 50 & pcnt_ambiguous_individuals <= 50, TRUE, FALSE)) %>% 
    # only retain suitable samples based on those criteria
    # filter(suitable_sample == TRUE) %>% 
    mutate(sampledate = as.Date(as.character(sampledate))) %>% 
    # select final required fields for functions
    select(stationcode,masterid,sampledate,collectionmethodcode,fieldreplicate,pcnt_ambiguous_individuals,pcnt_ambiguous_taxa,e, mean_o,oovere,oovere_percentile,
           mmi,mmi_percentile,csci,csci_percentile, count)
  # list of all sites with csci data, to use for data filtering
  csci_samples <- csci_clean_df %>% 
    distinct(masterid, sampledate) %>% 
    mutate(csci_check = 1)
  
  
  #### LAB CHEMISTRy DATA ####
  # TODO: 08/02/24 we updated the matrix filter to include samplewater
  # pulling chlorophylla and afdm from chemistry dataset
  lab_chem_sql <- paste0("SELECT stationcode, sampledate, matrixname, fieldreplicate, labreplicate, methodname, sampletypecode, analytename,
                         fractionname,unit, result,resqualcode, mdl, rl, qacode, record_origin FROM sde.unified_chemistry
                         WHERE analytename IN ('Chlorophyll a, Total', 'Chlorophyll a, Particulate', 'Chlorophyll a', 'Chlorophyll a, Not Recorded',
                         'Ash Free Dry Mass', 'Ash Free Dry Mass, Total', 'AFDM_Algae, Particulate', 'AFDM_Algae, Total') 
                         AND (matrixname LIKE '%benthic%' OR matrixname LIKE '%samplewater%')
                         AND LOWER(sampletypecode) NOT LIKE '%dup%' 
                         AND LOWER(sampletypecode) NOT LIKE '%qa%' 
                         AND LOWER(sampletypecode) NOT LIKE '%blank%' 
                         AND (LOWER(sampletypecode) LIKE '%grab%' 
                         OR LOWER(sampletypecode) LIKE '%integrated%' 
                         OR LOWER(sampletypecode) LIKE '%not recorded%' 
                         OR LOWER(sampletypecode) LIKE '%split%' 
                         OR LOWER(sampletypecode) LIKE '%fieldsp%')", sep = "")
  
  # running query, pulling data into environment
  lab_chem_df <- tbl(con, sql(lab_chem_sql)) %>% 
    as_tibble()
  lab_chem_clean <- lab_chem_df %>%     
    #TODO 11/14/24 drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    # get masterid, only want stations that exist in both tables so chose inner join
    inner_join(lustations_df %>% select(masterid,stationid), by = c("stationcode" = "stationid")) %>% 
    # remove blanks etc by masterids
    filter(masterid != "FBLANK", masterid != "F.BLK", masterid != "000NONPJ") %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples, by = c("masterid", "sampledate")) %>% 
    filter(csci_check == 1) %>% 
    # make all ND results 0 for now
    
    # TODO make sure result is a numeric value
    mutate(result = case_when(resqualcode == "ND" ~ 0,
                              TRUE ~ as.numeric(result))) %>%
    # conform sampledates as check
    # update fractionnames (will have to look in the analytename column, and if not look in the fractionname column)
    # kept this as a separate column for now as still have to validate this
    mutate(fractionname_v2 = case_when(str_detect(analytename,'Dissolved') ~ "dissolved",
                                       str_detect(fractionname, 'Dissolved') ~ "dissolved",
                                       
                                       str_detect(analytename,'Particulate') ~ "particulate",
                                       str_detect(fractionname, 'Particulate') ~ "particulate",
                                       
                                       str_detect(analytename, 'Total') ~ "total",
                                       str_detect(fractionname, 'Total') ~ "total",
                                       
                                       str_detect(analytename,'Not Recorded') ~ "not recorded",
                                       str_detect(fractionname, 'Not Recorded') ~ "not recorded",
                                       str_detect(fractionname, 'None') ~ "not recorded",
                                       TRUE ~ "error")) %>% 
    # conform analytenames
    mutate(analytename = case_when(analytename %in% c("Ash Free Dry Mass", "Ash Free Dry Mass, Total", "AFDM_Algae, Particulate", "AFDM_Algae, Total") ~ "AFDM_Algae",
                                   analytename %in% c("Chlorophyll a, Total", "Chlorophyll a, Particulate", "Chlorophyll a", "Chlorophyll a, Not Recorded") ~ "Chlorophyll a",
                                   
                                   TRUE ~ "Error")) %>% 
    
    # make all units, sampletypecodes, fractionname lowercase for conformity
    mutate(unit = str_to_lower(unit), sampletypecode = str_to_lower(sampletypecode), fractionname = str_to_lower(fractionname)) %>% 
    # confirm all benthic samples, and grab/integrated sampletypes (search for 'grab' or 'integrated' within the string, so also gets cases like 'field_grab')
    # make sure to keep benthic, and grab/integrated samples only (did this in query, but confirming)
    # Some data were reported with a samplewater matrix so we cannot use this filter
    #filter(str_detect(matrixname, 'benthic') & str_detect(sampletypecode, 'grab|integrated|split|not recorded|fieldsp')) %>% 
    # double check for dub or blank samples
    filter(!str_detect(sampletypecode, 'blank|dup|qa')) %>% 
    # make sure no NAs in character column
    mutate(resqualcode = ifelse(is.na(resqualcode), "NA", resqualcode)) %>% 
    #TODO 11/14/24 replace -88 mdl and rl values with NA replaced if else with case when and improved handling of NAs
    mutate(mdl = as.numeric(mdl),
           rl = as.numeric(rl),
           mdl = case_when(is.na(mdl)~NA_real_,
                           mdl < 0~ NA,
                           T~mdl),
           rl = case_when(is.na(rl)~NA_real_,
                          rl < 0~ NA,
                          T~rl)) %>%
    # TODO make sure that mdl and rl are numeric values
    mutate(mdl = ifelse(mdl < 0, NA, as.numeric(mdl)),
           rl = ifelse(rl < 0, NA, as.numeric(rl))) %>%
    # make all negative results zero
    mutate(result = ifelse(result < 0, 0, result)) %>% 
    # remove NA results
    filter(!is.na(result)) %>% 
    # now can make results and mdl/rl columns numeric
    mutate(result = as.numeric(result), mdl = as.numeric(mdl), rl = as.numeric(rl)) %>% 
    # retain just area units, not volume (for example, not "mg/m3")
    # all units per area have 'm2' in string, like 'g/m2' or 'mg/cm2'
    filter(str_detect(unit, 'm2')) %>% 
    # conform units: afdm in g/m2, chla in mg/m2
    # first convert everything to g/m2, then convert chla to mg/m2
    # have to convert both the result and the mdl/rl values also
    # will return '-9999999' if case_when statements don't capture all the possibilities 
    mutate(result = case_when(unit == "mg/m2" ~ result/1000,
                              unit == "ug/cm2" ~ result/100,
                              unit == "mg/cm2" ~ result * 10,
                              unit == "g/m2" ~ result,
                              TRUE ~ -999999),
           mdl = case_when(unit == "mg/m2" ~ mdl/1000,
                           unit == "ug/cm2" ~ mdl/100,
                           unit == "mg/cm2" ~ mdl * 10,
                           unit == "g/m2" ~ mdl,
                           TRUE ~ -999999),
           rl = case_when(unit == "mg/m2" ~ rl/1000,
                          unit == "ug/cm2" ~ rl/100,
                          unit == "mg/cm2" ~ rl * 10,
                          unit == "g/m2" ~ rl,
                          TRUE ~ -999999)) %>% 
    # TODO filter out results that do not equal to -999999
    filter(result != -999999) %>%
    mutate(unit = "g/m2") %>% 
    #convert chla to ug/cm2
    mutate(result = case_when(analytename == "Chlorophyll a" ~ result * 100,
                              TRUE ~ result),
           mdl = case_when(analytename == "Chlorophyll a" ~ mdl * 100,
                           TRUE ~ mdl),
           rl = case_when(analytename == "Chlorophyll a" ~ rl * 100,
                          TRUE ~ rl),
           unit = case_when(analytename == "Chlorophyll a" ~ "ug/cm2",
                            TRUE ~ unit)) %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate,methodname, analytename, result, unit, resqualcode, mdl, rl, qacode) %>% 
    # getting rid of true duplicates
    distinct(masterid, sampledate, fieldreplicate, labreplicate, analytename, result, .keep_all = TRUE) %>% 
    # want one row per masterid/sampledate/analytename
    # will calculate mean result per site/date/analyte
    # use mutate instead of summarize so other info isn't dropped
    # then retain a distinct row, so will choice one of the summarized resqualcodes and qacodes??
    group_by(masterid, sampledate, analytename) %>% 
    mutate(result = mean(result)) %>% 
    ungroup() %>% 
    distinct(masterid, sampledate, analytename, .keep_all = TRUE)
  
  # pulling chloride, sulfate, TDS
  # mostly because dealing with different matrixnames and units
  # total nitrogen, total phosphorus separately, from analysis table
  lab_chem_2_sql <- paste0("SELECT stationcode, sampledate, matrixname,fieldreplicate, labreplicate, methodname,sampletypecode,analytename,
                           fractionname,unit, result,resqualcode, mdl, rl, qacode, record_origin FROM sde.unified_chemistry
                           WHERE analytename IN ('Chloride', 'Chloride, Dissolved', 'Chloride, Not Recorded', 'Chloride, Total',
                           'Sulfate', 'Sulfate, Dissolved', 'Sulfate, Not Recorded', 'Sulfate, Total',
                           'Dissolved Solids', 'Total Dissolved Solids, Dissolved', 'Total Dissolved Solids, Total', 'Total Dissolved Solids',
                           'Total Dissolved Solids, Fixed') 
                           AND matrixname LIKE '%samplewater%' 
                           AND LOWER(sampletypecode) NOT LIKE '%dup%' 
                           AND LOWER(sampletypecode) NOT LIKE '%qa%' 
                           AND LOWER(sampletypecode) NOT LIKE '%blank%' 
                           AND (LOWER(sampletypecode) LIKE '%grab%' 
                           OR LOWER(sampletypecode) LIKE '%integrated%' 
                           OR LOWER(sampletypecode) LIKE '%not recorded%' 
                           OR LOWER(sampletypecode) LIKE '%split%' 
                           OR LOWER(sampletypecode) LIKE '%fieldsp%')", sep = "")
  # running query, pulling data into environment
  lab_chem_2_df <- tbl(con, sql(lab_chem_2_sql)) %>% 
    as_tibble()
  lab_chem_clean_2 <- lab_chem_2_df %>% 
    #TODO drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    # get masterid, only want stations that exist in both tables so chose inner join
    inner_join(lustations_df %>% select(masterid,stationid), by = c("stationcode" = "stationid")) %>% 
    # remove blanks etc by masterids
    filter(masterid != "FBLANK", masterid != "F.BLK", masterid != "000NONPJ") %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples, by = c("masterid", "sampledate")) %>% 
    filter(csci_check == 1) %>% 
    #TODO 11/14/24 replaced an if else with a case when because when resqualcode was NA we lost all results
    mutate(result = case_when(is.na(resqualcode)~as.numeric(result), 
                              resqualcode== "ND"~ 0,
                              T~as.numeric(result))) %>%
    # conform sampledates as check
    # update fractionnames (will have to look in the analytename column, and if not look in the fractionname column)
    # kept this as a separate column for now as still have to validate this
    mutate(fractionname_v2 = case_when(str_detect(analytename,'Dissolved') ~ "dissolved",
                                       str_detect(fractionname, 'Dissolved') ~ "dissolved",
                                       
                                       str_detect(analytename,'Particulate') ~ "particulate",
                                       str_detect(fractionname, 'Particulate') ~ "particulate",
                                       
                                       str_detect(analytename, 'Total') ~ "total",
                                       str_detect(fractionname, 'Total') ~ "total",
                                       
                                       str_detect(analytename,'Not Recorded') ~ "not recorded",
                                       str_detect(fractionname, 'Not Recorded') ~ "not recorded",
                                       str_detect(fractionname, 'None') ~ "not recorded",
                                       TRUE ~ "error")) %>% 
    # conform analytenames
    mutate(analytename = case_when(analytename %in% c("Chloride", "Chloride, Dissolved", "Chloride, Not Recorded", "Chloride, Total") ~ "Chloride",
                                   analytename %in% c("Sulfate", "Sulfate, Dissolved", "Sulfate, Not Recorded", "Sulfate, Total") ~ "Sulfate",
                                   analytename %in% c("Dissolved Solids", "Total Dissolved Solids, Dissolved", "Total Dissolved Solids, Total", "Total Dissolved Solids",
                                                      "Total Dissolved Solids, Fixed") ~ "Total Dissolved Solids",
                                   
                                   TRUE ~ "Error")) %>% 
    
    # make all units, sampletypecodes, fractionname lowercase for conformity
    mutate(unit = str_to_lower(unit), sampletypecode = str_to_lower(sampletypecode), fractionname = str_to_lower(fractionname)) %>% 
    # confirm all benthic samples, and grab/integrated sampletypes (search for 'grab' or 'integrated' within the string, so also gets cases like 'field_grab')
    # make sure to keep benthic, and grab/integrated samples only (did this in query, but confirming)
    filter(str_detect(matrixname, 'samplewater') & str_detect(sampletypecode, 'grab|integrated|split|not recorded|fieldsp')) %>% 
    # double check for dub or blank samples
    filter(!str_detect(sampletypecode, 'blank|dup|qa')) %>% 
    # make sure no NAs in character column
    mutate(resqualcode = ifelse(is.na(resqualcode), "NA", resqualcode)) %>% 
    # replace -88 mdl and rl values with NA
    mutate(mdl = ifelse(mdl < 0, NA, mdl),
           rl = ifelse(rl < 0, NA, rl)) %>%
    # make all negative results zero
    mutate(result = ifelse(result < 0, 0, result)) %>% 
    # remove NA results
    filter(!is.na(result)) %>% 
    # now can make results and mdl/rl columns numeric
    mutate(result = as.numeric(result), mdl = as.numeric(mdl), rl = as.numeric(rl)) %>% 
    # conform units
    filter(unit == "mg/l") %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, analytename, methodname,result, unit, resqualcode, mdl, rl, qacode) %>% 
    # getting rid of true duplicates
    distinct(masterid, sampledate, fieldreplicate, labreplicate, analytename, result, .keep_all = TRUE) %>% 
    # want one row per masterid/sampledate/analytename
    # will calculate mean result per site/date/analyte
    # use mutate instead of summarize so other info isn't dropped
    # then retain a distinct row, so will choice one of the summarized resqualcodes and qacodes??
    group_by(masterid, sampledate, analytename) %>% 
    mutate(result = mean(result)) %>% 
    ungroup() %>% 
    distinct(masterid, sampledate, analytename, .keep_all = TRUE)
  
  #### FIELD CHEMISTRY ####
  
  # pulling water quality/field chemistry data separately
  # (mostly field measurements, may get a few lab measurements) 
  # this looks for field chemistry data in the chemistry table
  field_chem_sql <- paste0("SELECT stationcode, sampledate, matrixname,fieldreplicate, labreplicate, methodname,sampletypecode,analytename,
                           fractionname,unit, result,resqualcode, mdl, rl, qacode, record_origin 
                           FROM sde.unified_chemistry
                           WHERE analytename IN ('SpecificConductivity, Not Recorded', 'SpecificConductivity', 'SpecificConductivity, Dissolved',
                           'ElectricalConductivity, Total','SpecificConductivity, Total', 'ElectricalConductivity',
                           'Oxygen, Dissolved, Not Recorded', 'Oxygen, Dissolved, Dissolved', 'Oxygen, Dissolved, Total',
                           'Temperature', 'Temperature, Total')
                           AND matrixname LIKE '%samplewater%' 
                           AND LOWER(sampletypecode) NOT LIKE '%dup%' 
                           AND LOWER(sampletypecode) NOT LIKE '%qa%' 
                           AND LOWER(sampletypecode) NOT LIKE '%blank%' 
                           AND (LOWER(sampletypecode) LIKE '%grab%' 
                           OR LOWER(sampletypecode) LIKE '%integrated%' 
                           OR LOWER(sampletypecode) LIKE '%not recorded%' 
                           OR LOWER(sampletypecode) LIKE '%split%' 
                           OR LOWER(sampletypecode) LIKE '%fieldsp%')", sep = "")
  # running query, pulling data into environment
  field_chem_df <- tbl(con, sql(field_chem_sql)) %>% 
    as_tibble()
  field_chem_clean <- field_chem_df %>% 
    #TODO drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    # get masterid, only want stations that exist in both tables so chose inner join
    inner_join(lustations_df %>% select(masterid,stationid), by = c("stationcode" = "stationid")) %>% 
    # remove blanks etc by masterids
    filter(masterid != "FBLANK", masterid != "F.BLK", masterid != "000NONPJ") %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples, by = c("masterid", "sampledate")) %>% 
    filter(csci_check == 1) %>% 
    # make all ND results 0 for now
    mutate(result = ifelse(resqualcode == "ND", 0, result)) %>% 
    # update fractionnames (will have to look in the analytename column, and if not look in the fractionname column)
    # kept this as a separate column for now as still have to validate this
    mutate(fractionname_v2 = case_when(str_detect(analytename,'Dissolved') ~ "dissolved",
                                       str_detect(fractionname, 'Dissolved') ~ "dissolved",
                                       
                                       str_detect(analytename,'Particulate') ~ "particulate",
                                       str_detect(fractionname, 'Particulate') ~ "particulate",
                                       
                                       str_detect(analytename, 'Total') ~ "total",
                                       str_detect(fractionname, 'Total') ~ "total",
                                       
                                       str_detect(analytename,'Not Recorded') ~ "not recorded",
                                       str_detect(fractionname, 'Not Recorded') ~ "not recorded",
                                       str_detect(fractionname, 'None') ~ "not recorded",
                                       TRUE ~ "error")) %>% 
    # conform analytenames
    mutate(analytename = case_when(analytename %in% c("SpecificConductivity, Not Recorded", "SpecificConductivity", "SpecificConductivity, Dissolved", "ElectricalConductivity, Total","SpecificConductivity, Total", "ElectricalConductivity") ~ "SpecificConductivity",
                                   
                                   analytename %in% c("Oxygen, Dissolved, Not Recorded", "Oxygen, Dissolved, Dissolved", "Oxygen, Dissolved, Total") ~ "Dissolved Oxygen",
                                   
                                   analytename %in% c("Temperature", "Temperature, Total") ~ "Temperature")) %>% 
    
    # make all units, sampletypecodes, fractionname lowercase for conformity
    mutate(unit = str_to_lower(unit), sampletypecode = str_to_lower(sampletypecode), fractionname = str_to_lower(fractionname)) %>% 
    # make sure to keep samplewater, and grab/integrated samples only (did this in query, but confirming)
    filter(str_detect(matrixname, 'samplewater') & str_detect(sampletypecode, 'grab|integrated|split|not recorded|fieldsp')) %>% 
    # double check for dub or blank samples
    filter(!str_detect(sampletypecode, 'blank|dup|qa')) %>% 
    # make sure no NAs in character column
    mutate(resqualcode = ifelse(is.na(resqualcode), "NA", resqualcode)) %>% 
    # replace -88 mdl and rl values with NA
    mutate(mdl = ifelse(mdl < 0, NA, mdl),
           rl = ifelse(rl < 0, NA, rl)) %>%
    # make all negative results zero
    mutate(result = ifelse(result < 0, 0, result)) %>% 
    # remove NA results
    filter(!is.na(result)) %>% 
    # now can make results and mdl/rl columns numeric
    mutate(result = as.numeric(result), mdl = as.numeric(mdl), rl = as.numeric(rl)) %>% 
    # conform units
    # dealing with equivalent units first
    mutate(unit = case_when(unit ==  "umhos/cm" ~ "us/cm",
                            unit == "nr" ~ "none",
                            TRUE ~ unit)) %>% 
    # making other conversions
    # need to convert result, and mdl and rls
    mutate(result = case_when(unit == "ms/cm" ~ result * 1000, #conductivity in us/cm
                              TRUE ~ result),
           mdl = case_when(unit == "ms/cm" ~ mdl * 1000, #conductivity in us/cm
                           TRUE ~ mdl),
           rl = case_when(unit == "ms/cm" ~ rl * 1000, #conductivity in us/cm
                          TRUE ~ rl)) %>% 
    # now renaming units that were converted
    mutate(unit = case_when(unit == "ms/cm" ~ "us/cm",
                            TRUE ~ unit)) %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, analytename, methodname,result, unit, resqualcode, mdl, rl, qacode) %>% 
    # getting rid of true duplicates
    distinct(masterid, sampledate, fieldreplicate, labreplicate, analytename, result, .keep_all = TRUE) %>% 
    # want one row per masterid/sampledate/analytename
    # will calculate mean result per site/date/analyte
    # use mutate instead of summarize so other info isn't dropped
    # then retain a distinct row, so will choice one of the summarized resqualcodes and qacodes??
    group_by(masterid, sampledate, analytename) %>% 
    mutate(result = mean(result)) %>% 
    ungroup() %>% 
    distinct(masterid, sampledate, analytename, .keep_all = TRUE)
  # ALSO PULL FROM UNIFIED PHAB, as some data not in unified chem and vise vera
  # no air temperature, so want just samplewater matrix
  field_chem_2_sql <- paste0("SELECT stationcode, sampledate, matrixname,replicate,methodname,collectiondepth,analytename,
                           fractionname,unitname, result,resqualcode, qacode, sampleagencycode 
                           FROM sde.unified_phab
                           WHERE analytename IN ('SpecificConductivity',
                           'Oxygen, Dissolved',
                           'Temperature') AND matrixname LIKE '%samplewater%' ", sep = "")
  # running query, pulling data into environment
  field_chem_2_df <- tbl(con, sql(field_chem_2_sql)) %>% 
    as_tibble()
  # data cleaning
  field_chem_clean_2 <- field_chem_2_df %>% 
    #TODO drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    # get masterid, only want stations that exist in both tables so chose inner join
    inner_join(lustations_df %>% select(masterid,stationid), by = c("stationcode" = "stationid")) %>% 
    # remove blanks etc by masterids
    filter(masterid != "FBLANK", masterid != "F.BLK", masterid != "000NONPJ") %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples, by = c("masterid", "sampledate")) %>% 
    filter(csci_check == 1) %>% 
    # rename dissolved oxygen
    mutate(analytename = case_when(analytename == "Oxygen, Dissolved" ~ "Dissolved Oxygen",
                                   TRUE ~ analytename)) %>% 
    # make all units, sampletypecodes, fractionname lowercase for conformity
    mutate(unitname = str_to_lower(unitname), fractionname = str_to_lower(fractionname)) %>% 
    # make all negative results zero
    mutate(result = ifelse(result < 0, 0, result)) %>% 
    mutate(result = as.numeric(result)) %>% 
    # make filler columns
    mutate(labreplicate = 1, mdl = NA, rl =NA) %>% 
    rename(unit = unitname, fieldreplicate = replicate) %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, analytename, methodname,result, unit, resqualcode, mdl, rl, qacode) %>% 
    # getting rid of true duplicates, not likely but just in case
    distinct(masterid, sampledate, fieldreplicate, labreplicate, analytename, result, .keep_all = TRUE)
  
  # find any stations/samples unique to unified_phab
  # want to add those to unified_chem field data
  # have to use masterid and sampledate as part of anijoin, retaining only unique unified phab samples
  # anti-join dropped samples not in PHAB but that were in Chem so we changed to bind_rows
  field_chem_add <- field_chem_clean_2 %>% 
    anti_join(field_chem_clean, by = c("masterid", "sampledate"))
  #   bind_rows(field_chem_clean) %>%
  #   distinct()
  
  #### NUTRIENT ANALYSIS CHEMISTRY ####
  
  # pulling TN and TP data from the analysis table
  lab_chem_3_sql <- paste0("SELECT * FROM sde.analysis_chem_nutrients_0")
  # running query, pulling data into environment
  lab_chem_3_df <- tbl(con, sql(lab_chem_3_sql)) %>% 
    as_tibble()
  
  lab_chem_clean_3_tn <- lab_chem_3_df %>% 
    #TODO drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples, by = c("masterid", "sampledate")) %>% 
    filter(csci_check == 1) %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, total_n_mgl, total_n_mgl_mdl, total_n_mgl_rl, total_n_mgl_method) %>% 
    filter(total_n_mgl_method == "reported" | total_n_mgl_method == "calculated") %>% 
    rename(result = total_n_mgl, mdl = total_n_mgl_mdl, rl = total_n_mgl_rl, methodname = total_n_mgl_method) %>% 
    mutate(unit = "mg/l", analytename = "Nitrogen,Total", resqualcode = "=", qacode = "None") %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, analytename, methodname, result, unit, resqualcode, mdl, rl, qacode) %>% 
    # want one row per masterid/sampledate/analytename
    # will calculate mean result per site/date/analyte
    group_by(masterid, sampledate, analytename) %>% 
    mutate(result = mean(result)) %>% 
    ungroup() %>% 
    distinct(masterid, sampledate, analytename, .keep_all = TRUE)
  
  
  lab_chem_clean_3_tp <- lab_chem_3_df %>% 
    #TODO drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples, by = c("masterid", "sampledate")) %>% 
    filter(csci_check == 1) %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, total_p_mgl, total_p_mgl_mdl, total_p_mgl_rl, total_p_mgl_method) %>% 
    # changed this because Phos Data is not always reported correctly in the analysis table
     #filter(total_p_mgl_method == "reported" | total_p_mgl_method == "calculated") %>% 
    #filter(total_p_mgl_method == "calculated") #%>% 
    rename(result = total_p_mgl, mdl = total_p_mgl_mdl, rl = total_p_mgl_rl, methodname = total_p_mgl_method) %>% 
    mutate(unit = "mg/l", analytename = "Phosphorus as P", resqualcode = "=", qacode = "None") %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, analytename, methodname, result, unit, resqualcode, mdl, rl, qacode) %>% 
    # want one row per masterid/sampledate/analytename
    # will calculate mean result per site/date/analyte
    group_by(masterid, sampledate, analytename) %>% 
    mutate(result = mean(result)) %>% 
    ungroup() %>% 
    distinct(masterid, sampledate, analytename, .keep_all = TRUE)
  
  
  lab_chem_clean_3 <- bind_rows(lab_chem_clean_3_tn, lab_chem_clean_3_tp)
  
  #### PHAB METRICS ####
  # analysis_phab_metrics_df_sql <- paste0("SELECT * FROM sde.analysis_phab_metrics")
  
  analysis_phab_metrics_df <- tbl(con, "analysis_phabmetrics") %>% 
    as_tibble()
  phab_clean <- analysis_phab_metrics_df %>% 
    #TODO drop time from datetime
    mutate(sampledate=as.Date(sampledate)) %>% 
    # get masterid, only want stations that exist in both tables so chose inner join
    inner_join(lustations_df %>% select(masterid,stationid), by = c("stationcode" = "stationid")) %>% 
    # remove blanks etc by masterids
    filter(masterid != "FBLANK", masterid != "F.BLK", masterid != "000NONPJ") %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples, by = c("masterid", "sampledate")) %>% 
    filter(csci_check == 1) %>% 
    select(masterid, sampledate, variable, result, unit) %>% 
    filter(variable %in% c("XCMG", "PCT_SAFN", "H_SubNat", "H_AqHab", "Ev_FlowHab")) %>% 
    rename(analytename = variable) %>% 
    mutate(fieldreplicate = 1,
           labreplicate = "",
           resqualcode = "=",
           mdl = as.numeric(NA), 
           rl = as.numeric(NA),
           qacode = "None",
           methodname = "NA") %>% #column conformity with other chem data
    mutate(labreplicate = as.numeric(labreplicate), result = as.numeric(result)) %>% 
    # getting rid of true duplicates
    distinct(masterid,sampledate, analytename, result, .keep_all = TRUE) %>% 
    # want one row per masterid/sampledate/analytename
    # will calculate mean result per site/date/analyte
    # use mutate instead of summarize so other info isn't dropped
    # then retain a distinct row, so will choice one of the summarized resqualcodes and qacodes??
    group_by(masterid, sampledate, analytename) %>% 
    mutate(result = mean(result)) %>% 
    ungroup() %>% 
    select(masterid, sampledate, fieldreplicate, labreplicate, methodname,analytename, result, unit, resqualcode, mdl, rl, qacode) %>% 
    distinct(masterid, sampledate, analytename, result, .keep_all = TRUE)
  
  
  ####  OE Data ####
  
  analysis_csci_oe_df <- tbl(con, sql("SELECT stationcode, sampleid, otu, captureprob, meanobserved FROM sde.analysis_csci_suppl1_oe")) %>% 
    as_tibble()
  
  # Assemble the expected taxa lists and associated capture probabilities
  
  oe_clean <- analysis_csci_oe_df %>% 
    # get masterid, only want stations that exist in both tables so chose inner join
    inner_join(lustations_df %>% select(masterid,stationid), by = c("stationcode" = "stationid")) %>% 
    # remove blanks etc by masterids
    filter(masterid != "FBLANK", masterid != "F.BLK", masterid != "000NONPJ") %>% 
    # checking which stations and sampledates have csci data
    left_join(csci_samples %>% distinct(masterid, .keep_all = TRUE), by = c("masterid")) %>% 
    filter(csci_check == 1) %>% 
    select(masterid, otu,captureprob,meanobserved) %>% 
    filter(otu != "Unambiguous_NotAtRefCal") %>% #remove entries without taxa
    mutate(captureprob = ifelse(is.na(captureprob), 0, captureprob)) %>%   #make sure any NA entry is zero (prob could equivalently just delete these rows)
    # retain unique rows per masterid/otu/captureprob
    distinct(masterid, otu, captureprob,.keep_all = TRUE) %>% 
    # add latitude/longitude/county information, want just one row per masterid from lustations dataset
    left_join(lustations_df %>% select(masterid, latitude,longitude,county) %>% distinct(masterid, .keep_all = TRUE), by = "masterid")
  
  #### FINAL CSCI DATASET ####
  
  # use csci_clean, but deal with potential sampling repeats or data entry issues carefully
  # so decided to separate the samples:
  # keep a set of no problem data, and another set where we have multiple sets of masterid/sampledate/replicate/collectionmethods
  
  
  csci_noreps <- csci_clean_df %>% 
    group_by(masterid, sampledate, fieldreplicate, collectionmethodcode) %>% 
    mutate(sample_count = n()) %>% 
    filter(sample_count == 1) %>% 
    ungroup()
  csci_reps <- csci_clean_df %>% 
    group_by(masterid, sampledate, fieldreplicate, collectionmethodcode) %>% 
    mutate(sample_count = n()) %>% 
    filter(sample_count > 1) %>% 
    ungroup() %>% 
    # remove cases when stationcode includes SMCR8, as those seem to be cases when bugs re-scored/improved and submitted again 
    filter(!str_detect(stationcode, 'SMCR8')) %>% 
    # also remove true duplicate/same csci scores
    distinct(masterid, sampledate, collectionmethodcode, fieldreplicate, csci, .keep_all = TRUE) %>% 
    # that leaves mostly sites that have been synonimized (two different stationcodes for one masterid)
    # for these, just keep the max csci score (the scores should be pretty close/ within 0.1)
    group_by(masterid,sampledate, fieldreplicate, collectionmethodcode) %>% 
    top_n(1, csci) %>%  # select the one row per group with the highest csci score
    ungroup()
  # for final dataset, join the two datasets back together
  csci_base_df <- bind_rows(csci_noreps, csci_reps) %>% 
    select(-sample_count, -stationcode) ## edited by Rachel 
  
  # save(csci_base_df, file = "Base_Files/Base_Data.RData")
  
  #### FINAL STRESSOR DATASET ####
  
  # we had previously decided to average stressor results per masterid/date/analyte
  # so field/lab replicate information no longer necessary
  # also fill in units to match the rsca_module_direction assignments (made lowercase before just so easier to deal with)
  
  # add outlier filtering/removing
  # temp 40 degrees, DO 20 mg/l
  
  stressor_base_df <- lab_chem_clean %>% 
    bind_rows(lab_chem_clean_2) %>% 
    bind_rows(lab_chem_clean_3) %>% 
    bind_rows(field_chem_clean) %>% 
    bind_rows(field_chem_add) %>% 
    bind_rows(phab_clean) %>% 
    select(masterid, sampledate,analytename, unit, result, resqualcode, mdl, rl, qacode) %>% 
    mutate(unit = case_when(analytename %in% c('Chloride', 'Sulfate', 'Total Dissolved Solids', 'Dissolved Oxygen',
                                               'Nitrogen,Total', 'Phosphorus as P') ~ "mg/L",
                            analytename == "SpecificConductivity" ~ "uS/cm",
                            analytename == "AFDM_Algae" ~ "g/m2",
                            analytename == "Chlorophyll a" ~ "ug/cm2",
                            analytename == "Temperature" ~ "deg C",
                            analytename %in% c('Ev_FlowHab', 'H_AqHab', 'H_SubNat') ~"none",
                            analytename %in% c('PCT_SAFN', 'XCMG') ~ "%")) %>% 
    # making an outlier note, values based on Rafi's recommendation
    mutate(outlier = case_when(analytename == "Temperature" & result > 40 ~ "yes",
                               analytename == "Dissolved Oxygen" & result > 20 ~ "yes",
                               analytename == "Phosphorus as P" & result > 3 ~ "yes",
                               analytename == "SpecificConductivity" & result > 20000 ~ "yes",
                               analytename == "SpecificConductivity" & result < 10 ~ "yes",
                               TRUE ~ "no")) %>% 
    filter(outlier == "no") %>% 
    select(-outlier)
  # want stressor dataset to be full, so NAs for any analyte data that we are missing
  # to do so, add csci data so we can define unique sampling events, then fill out using crossing() and analytenames in module list
  # to get analyte and module assignments
  load("Base_Files/RSCA_Module_Direction_Assignments.RData")
  
  stressor_csci <- stressor_base_df %>% 
    # right join, so stressor data populates for each csci sampling event 
    right_join(csci_base_df %>% select(masterid,sampledate,fieldreplicate,collectionmethodcode,csci), by = c("masterid", "sampledate"))
  # # making sure we have all unique data/no duplicates
  # distinct(masterid, sampledate, fieldreplicate, collectionmethodcode, csci)
  
  # TODO a better way to ensure no duplicates
  stressor_csci %>%
    group_by(masterid, sampledate, collectionmethodcode, fieldreplicate, analytename) %>%
    tally() %>%
    filter(n>1)
  stressor_csci_base_df <- stressor_csci %>% 
    # retain unique sampling/csci events, so can build out data skeleton with all analyte combos
    distinct(masterid, sampledate, fieldreplicate,collectionmethodcode,csci) %>% 
    # creating full test sample x stressor data matrix. use list of analytes from module direction dataset
    crossing(., select(rsca_module_direction, analytename)) %>% 
    # adding back whatever stressor data we have
    # make sure that row number doesn't increase here. if it does, then we don't have uniquely defined stressor data (by masterid/sampledate/analytename)
    left_join(stressor_csci, by = c("masterid", "sampledate", "fieldreplicate", "collectionmethodcode", "csci", "analytename")) %>% 
    # make sure no NAs in character column
    mutate(resqualcode = ifelse(is.na(resqualcode), "NA", resqualcode)) %>% 
    mutate(qacode = ifelse(is.na(qacode), "None", qacode)) %>% 
    # fill in missing units
    mutate(unit = case_when(analytename %in% c('Chloride', 'Sulfate', 'Total Dissolved Solids', 'Dissolved Oxygen',
                                               'Nitrogen,Total', 'Phosphorus as P') ~ "mg/L",
                            analytename == "SpecificConductivity" ~ "uS/cm",
                            analytename == "AFDM_Algae" ~ "g/m2",
                            analytename == "Chlorophyll a" ~ "ug/cm2",
                            analytename == "Temperature" ~ "deg C",
                            analytename %in% c('Ev_FlowHab', 'H_AqHab', 'H_SubNat') ~ "none",
                            analytename %in% c('PCT_SAFN', 'XCMG') ~ "%")) 
  
  # resave(stressor_csci_base_df, file = "Base_Files/Base_Data.RData")
  #### FINAL OE DATASET ####
  
  # get otu/captur prob data, add MaxOfCSCI scores (by masterid)
  
  oe_base_df <- oe_clean 
  
  # resave(oe_base_df, file = "Base_Files/Base_Data.RData")
  
  #### FINAL STATION DATASET ####
  station_base_df <- oe_clean %>% 
    distinct(masterid, latitude,longitude,county) %>% 
    left_join(lustations_df %>% select(masterid, latitude, longitude, county, huc, comid, stationname) %>% 
                distinct(masterid,latitude,longitude,county,.keep_all = TRUE))
  
  # resave(station_base_df, file = "Base_Files/Base_Data.RData")
  
  ### added Dec 2023 after updating packages, "resave" was not working in the function
  save(csci_base_df, stressor_csci_base_df, oe_base_df, station_base_df, chansum_df, file = "Base_Files/Base_Data.RData")
  
  #### DATABASE DISCONNECTION ####
  
  dbDisconnect(con)  # close the connection, after retrieving the data.
  
}





