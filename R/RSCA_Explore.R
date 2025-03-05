# Location to run data assembly and analysis scripts for Rapid Screening Causal Assessment.

# Very Similar to 0.2_Core.R, but this is for exploring results/generating static datasets and graphs
# so this script is not connected to/called by smc database script (0.0_main.R)


#### ENTER YOUR TEST SITES AND YOUR EXPORT PATH/DIRECTORY ####

# ***if you are just running the functions, not making changes, this section is the only one you need to update before running the script
# ***also need to load a connection to database, and name it 'con' (cannot share that connection on GitHub)
# ***can run batches of test sites, but would recommend limiting the batch size. for example, 1000 test sites took 5 hours to run.

# update this path with local directory
my_path <- "A:/David/RSCA_exports"

# assign list of test sites to 'my_input_test_sites'

# SMC sites, all
# is there a different way to pull this list? for now just filtering by hucs
smc_station <- station_base_df %>% 
  # list of hucs in SMC region, based on SMC_County shapefile and calwater_SWAMP3Code shapefiles for now
  filter(huc %in% c(401, 402, 403, 404, 405, 407, 408, 409, 410,411, 412, 845, 481,801, 802, 901, 902, 903, 
                    904, 905, 906, 907, 908, 909, 910, 911))

my_input_test_sites <- unique(smc_station$masterid[1001:1553])
# first run 1:1000
# start 11, end 3:30
# export started 4:32 pm, ended 5:03 pm

# second run 1000 to 1553

#### REQUIRED LIBRARIES ####

library(tidyverse)
library(lubridate)
library(vegan)
library(purrr)
library(DBI) # needed to connect to database
library(dbplyr) # needed to connect to database
library(RPostgreSQL) # needed to connect to our database
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear


#### LOAD DATA ASSEMBLY & RSCA FUNCTIONS ####

# load Functions, by running each function script
source("R/1.0_Test_CSCI_check.R")
source("R/2.0_Comparator_Site_Selection.R")
source("R/3.0_LOE_Data_Prep.R")
source("R/4.1_Spatial_Temporal_LOE.R")
source("R/4.2_Ref_Condition_Comp_LOE.R")
source("R/4.3_Stressor_Response_LOE.R") #using log functions, not linear
source("R/5.0_LOE_summarize.R")
source("R/6.0_Data_Inventory.R")


#### LOAD BASE DATA, set function thresholds ####

# this loads CSCI, station, stressor, and OE data to your environment, which are in RData file format
# NOTE: if want to re-save/update base datasets, re-run the 'prep_smc_data' function from 0.1_SMC_Data_Prepping.R
# source("R/0.1_SMC_Data_Prepping.R")
# prep_smc_data(database_connection = con)

load("Base_Files/Base_Data.RData")
load("Base_Files/RSCA_Module_Direction_Assignments.RData")

# also import scape data
# from SMC database connection, information not posted on github
scape <- tbl(con, sql("SELECT * FROM sde.scape_strm_constraints")) %>% 
  as_tibble()
# scape <- read_csv("Data/scape_strm_constraints.csv")

#### EXECUTE FUNCTIONS ####

# Run desired analysis, on generated test site data (generated in Test&Comp_Data_Assembly.R)
# wrapped all functions into one core workflow, can iterate/map that across input test sites


CORE_fun <- function(TestID, MinCaptureProb=0.5, MaxBC=0.1){
  
  CSCI_check_fun_out <- CSCI_check_fun(TestID = TestID, CSCIthreshold = "default") #scape or "default"
  
  # If no data, don't run RSCA analysis
  if (CSCI_check_fun_out$csci_check[[1]] == "no"){
    
    print(CSCI_check_fun_out)
    
    # Else run RSCA analysis
  } else {
    
    
    Comp_Select_fun_out <- Comp_Select_fun(TestID = TestID, Min.CaptureProb = MinCaptureProb, Max.BC = MaxBC)
    
    ST_dat_fun_out <- ST_dat_fun(TestID = TestID, CompSites = Comp_Select_fun_out)
    ST_fun_out <- ST_fun(STData = ST_dat_fun_out , CSCIcheckoutput = CSCI_check_fun_out)
    ST_sum_mod_out <- ST_sum_mod(SToutput = ST_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    
    RCC_dat_fun_out <- RCC_dat_fun(TestID = TestID, CompSites = Comp_Select_fun_out)
    RCC_fun_out <- RCC_fun(RCCData = RCC_dat_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    RCC_sum_mod_out <- RCC_sum_mod(RCCoutput = RCC_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    
    SR_log_dat_fun_out <- SR_log_dat_fun(TestID = TestID, CompSites = Comp_Select_fun_out)
    SR_log_fun_out <- SR_log_fun(SRData = SR_log_dat_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    SR_log_mod_sum_out <- SR_log_mod_sum(SRlogoutput = SR_log_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    
    # gathering all outputs into one list
    results_list <- list(Comp_Select_fun_out,
                         ST_dat_fun_out, ST_fun_out, ST_sum_mod_out,
                         RCC_dat_fun_out, RCC_fun_out, RCC_sum_mod_out,
                         SR_log_dat_fun_out, SR_log_fun_out, SR_log_mod_sum_out)
    
    # naming each dataframe in the list
    names(results_list) <- c('Comp_Select_fun_out',
                             'ST_dat_fun_out', 'ST_fun_out', 'ST_sum_mod_out',
                             'RCC_dat_fun_out','RCC_fun_out', 'RCC_sum_mod_out',
                             'SR_log_dat_fun_out', 'SR_log_fun_out', 'SR_log_mod_sum_out')
    print(TestID) #prints out site in terminal so you can check which sites have been processed when running batches
    return(results_list)
    
    
  }
  
  
}

# run RSCA analysis over all input test sites
# this generates a dataframe containing lists of results, that need to be extracted below
CORE_fun_out <- my_input_test_sites %>%  purrr::map(~CORE_fun(.x))



#### EXTRACTING RESULTS FROM CORE LISTS AND EXPORTING ####


Export_fun <- function(path, TestIDs, CORE_output){
  
  
  # data for comparator site mapping, include each test site and lat long, and corresponding comparator sites with lat longs
  Test_Comp_df <- lapply(CORE_fun_out, function(x){x$Comp_Select_fun_out}) %>% bind_rows()
  
  # for below:
  # dat is the comparator data set needed for the boxplots
  # LOE is the summary data set for the specific line of evidence, it looks like data in slide 5 in the dashboard sketch v1
  # sum is another summary data, which has the Spatial.Temporal column with the categorization
  
  # The sum dfs get combined later, so we technically do not need these individually. There is more description about that table later
  
  # The "dat" df is the raw comparator data behind each LOE plots, whether they be the boxplots or the logistic regression based on the different
  #   analytes, etc.
  
  # The "LOE" df is the data to display on the LOA results tab, as a table (see slide 5)
  # This data frame also contains the Test.Result column which should be an hline on each plot, 
  #   and colored according to the Score column (ST Score, etc.)
  
  # ST = Spatial Temporal
  ST_dat_df <- lapply(CORE_fun_out, function(x){x$ST_dat_fun_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  ST_LOE_df <- lapply(CORE_fun_out, function(x){x$ST_fun_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  
  # RCC = Reference Condition Comparison
  RCC_dat_df <- lapply(CORE_fun_out, function(x){x$RCC_dat_fun_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  RCC_LOE_df <- lapply(CORE_fun_out, function(x){x$RCC_fun_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  
  # SR = Stressor Response
  SR_log_dat_df <- lapply(CORE_fun_out, function(x){x$SR_log_dat_fun_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  SR_log_LOE_df <- lapply(CORE_fun_out, function(x){x$SR_log_fun_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  
  
  #### SUMMARIZING ####
  # from 0.5_LOE_summarize.R
  
  # These three are not needed for the final sets of data
  ST_sum_df <- lapply(CORE_fun_out, function(x){x$ST_sum_mod_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  RCC_sum_df <- lapply(CORE_fun_out, function(x){x$RCC_sum_mod_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  SR_log_sum_df <- lapply(CORE_fun_out, function(x){x$SR_log_mod_sum_out}) %>% bind_rows() %>% arrange(test_csci_sampleid)
  
  # combines the above summary df's with the final classification of each Line of analysis for each module for each sampleid
  # This is needed for the rectangle plot in slide 3 (Dashboard sketch v1)
  LOE_sum_df <- LOE_sum_fun(ST_sum = ST_sum_df, RCC_sum = RCC_sum_df, SR_log_sum = SR_log_sum_df) %>% 
    arrange(test_csci_sampleid)
  
  # For coloring the dots on the map, the CSCI plot, and the Module rectangle plot
  # Basically this data is for the first two tabs, (Map and Site summary)
  LOE_mod_sum_df <- LOE_samp_sum_fun(ST_sum = ST_sum_df, RCC_sum = RCC_sum_df, SR_log_sum = SR_log_sum_df) %>% 
    arrange(test_csci_sampleid)
  
  
  #### MONITORING NEEDS REPORT (DATA INVENTORY) ####
  
  Dat_invt_df <- Dat_invt_fun(LOEInputData = SR_log_dat_df)
  
  
  #### CSCI DATA ####
  
  # add a step here to export CSCI results for the test sites? this is just extracting data from base datasets
  
  CSCI_core <- csci_base_df %>%
    filter(masterid %in% TestIDs)
  
  #### FINAL EXPORT ####
  
  write_csv(Test_Comp_df, paste(path, "4.0_Comparators.csv", sep = "/"))
  write_csv(ST_dat_df, paste(path, "2.2.1_SpatialTemporal_data.csv", sep = "/"))
  write_csv(ST_LOE_df, paste(path, "2.2_SpatialTemporal_LOA.csv", sep = "/"))
  write_csv(RCC_dat_df, paste(path, "2.1.1_ReferenceConditionComparison_data.csv", sep = "/"))
  write_csv(RCC_LOE_df, paste(path, "2.1_ReferenceConditionComparison_LOA.csv", sep = "/"))
  write_csv(SR_log_dat_df, paste(path, "2.3.1_StressorResponse_logistic_data.csv", sep = "/"))
  write_csv(SR_log_LOE_df, paste(path, "2.3_StressorResponse_logistic_LOA.csv", sep = "/"))
  write_csv(LOE_sum_df, paste(path, "1.1_Module_LOAs.csv", sep = "/"))
  write_csv(LOE_mod_sum_df, paste(path, "1.0_Module_Summary.csv", sep = "/"))
  write_csv(Dat_invt_df, paste(path, "3.0_Monitoring_needs.csv", sep = "/"))
  
  write_csv(CSCI_core, paste(path,"4.1_CSCI_Core.csv", sep = "/"))
  
  
}

# run function
Export_fun(path = my_path, TestIDs = my_input_test_sites, CORE_output = CORE_fun_out)
