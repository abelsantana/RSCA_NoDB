# CORE location to run data assembly and analysis scripts for Rapid Screening Causal Assessment.

#### REQUIRED LIBRARIES ####

library(tidyverse)
library(tidyr)
library(lubridate)
library(vegan)
library(purrr)
library(DBI) # needed to connect to database
library(dbplyr) # needed to connect to database
library(RPostgreSQL) # needed to connect to our database
library(rstudioapi) # just so we can type the password as we run the script, so it is not written in the clear
#library(pool)

#### LOAD DATA ASSEMBLY & RSCA FUNCTIONS ####
#setwd("C:/Users/abelsantana/OneDrive - SCCWRP/Documents/MyR/RSCAProject")
# load Functions, by running each function script
source("R/1.0_Test_CSCI_check.R")
source("R/2.0_Comparator_Site_Selection_v2.R")
source("R/3.0_LOE_Data_Prep.R")
source("R/4.1_Spatial_CoOccurrence_LOE.R")
source("R/4.2_Ref_Condition_Comp_LOE.R")
source("R/4.3_Stressor_Response_LOE.R") #using log functions, not linear
source("R/5.0_LOE_summarize.R")
source("R/6.0_Data_Inventory.R")

#################################################
#### LOAD BASE DATA, set function thresholds ####

# this loads CSCI, station, stressor, and OE data to your environment, which are in RData file format
# NOTE: if want to re-save/update base datasets, re-run the 'prep_smc_data' function from 0.1_SMC_Data_Prepping.R

# source("R/0.1_Data_Prepping.R")
# prep_smc_data(con)

load("Base_Files/Base_Data.RData")
load("Base_Files/RSCA_Module_Direction_Assignments.RData")

# also import scape data
# from SMC database connection, information not posted on github


scape <- tbl(con, sql("SELECT * FROM sde.scape_strm_constraints")) %>% 
  as_tibble()
# scape <- read_csv("Data/scape_strm_constraints.csv")

# TODO Convert all -999 values to NA
scape <- scape %>% 
  mutate(across(where(is.numeric), ~ ifelse(.x <= 0, NA, .x)))


#TestID <- "SMC01004"
  CORE_fun <- function(TestID, MinCaptureProb=0.5, MaxBC=0.1){
  # browser()
  CSCI_check_fun_out <- CSCI_check_fun(TestID = TestID, CSCIthreshold = "default") #scape or "default"
  
  # If no data, don't run RSCA analysis
  if (CSCI_check_fun_out$csci_check[[1]] == "no"){
    message("No CSCI data for TestID: ", TestID)
    print(CSCI_check_fun_out)
    
    # Else run RSCA analysis
  } else {
    
    
    Comp_Select_fun_out <- Comp_Select_Modified_fun(TestID = TestID, Min.CaptureProb = MinCaptureProb, Max.BC = MaxBC, Type = Type)
    
    SCO_dat_fun_out <- SCO_dat_fun(TestID = TestID, CompSites = Comp_Select_fun_out)
    SCO_fun_out <- SCO_fun(SCOData = SCO_dat_fun_out , CSCIcheckoutput = CSCI_check_fun_out)
    SCO_sum_mod_out <- SCO_sum_mod(SCOoutput = SCO_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    
    RCC_dat_fun_out <- RCC_dat_fun(TestID = TestID, CompSites = Comp_Select_fun_out)
    RCC_fun_out <- RCC_fun(RCCData = RCC_dat_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    RCC_sum_mod_out <- RCC_sum_mod(RCCoutput = RCC_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    
    SR_log_dat_fun_out <- SR_log_dat_fun(TestID = TestID, CompSites = Comp_Select_fun_out)
    SR_log_fun_out <- SR_log_fun(SRData = SR_log_dat_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    SR_log_mod_sum_out <- SR_log_mod_sum(SRlogoutput = SR_log_fun_out, CSCIcheckoutput = CSCI_check_fun_out)
    
    # gathering all outputs into one list
    results_list <- list(Comp_Select_fun_out,
                         SCO_dat_fun_out, SCO_fun_out, SCO_sum_mod_out,
                         RCC_dat_fun_out, RCC_fun_out, RCC_sum_mod_out,
                         SR_log_dat_fun_out, SR_log_fun_out, SR_log_mod_sum_out)
    
    # naming each dataframe in the list
    names(results_list) <- c('Comp_Select_fun_out',
                             'SCO_dat_fun_out', 'SCO_fun_out', 'SCO_sum_mod_out',
                             'RCC_dat_fun_out','RCC_fun_out', 'RCC_sum_mod_out',
                             'SR_log_dat_fun_out', 'SR_log_fun_out', 'SR_log_mod_sum_out')
    print(TestID) #prints out site in terminal so you can check which sites have been processed when running batches
    return(results_list)
    
    
  }
  
}
# run RSCA analysis over all input test sites
# this generates a dataframe containing lists of results, that need to be extracted below
CORE_fun_out <- my_input_test_sites %>% purrr::map(~CORE_fun(.x))
## for broken site !!!!!!!!!!!!!!!!
# CORE_fun_out <- CORE_fun("801BBRC01")

#### EXTRACTING RESULTS FROM CORE LISTS ####

# data for comparator site mapping, include each test site and lat long, and corresponding comparator sites with lat longs
Test_Comp_df <- lapply(CORE_fun_out, function(x){x$Comp_Select_fun_out}) %>% bind_rows() %>% 
  # also adding stressor data to final dataset, for when data is downloaded on dashboard
  left_join(stressor_csci_base_df, by = c("comp_site" = "masterid"))
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
SCO_dat_df <- lapply(CORE_fun_out, function(x){x$SCO_dat_fun_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)
SCO_LOE_df <- lapply(CORE_fun_out, function(x){x$SCO_fun_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)

# RCC = Reference Condition Comparison
RCC_dat_df <- lapply(CORE_fun_out, function(x){x$RCC_dat_fun_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)
RCC_LOE_df <- lapply(CORE_fun_out, function(x){x$RCC_fun_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)

# SR = Stressor Response
SR_log_dat_df <- lapply(CORE_fun_out, function(x){x$SR_log_dat_fun_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)
SR_log_LOE_df <- lapply(CORE_fun_out, function(x){x$SR_log_fun_out}) %>% bind_rows() %>% 
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)

#### SUMMARIZING ####
# from 0.5_LOE_summarize.R

# These three are not needed for the final sets of data
SCO_sum_df <- lapply(CORE_fun_out, function(x){x$SCO_sum_mod_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)
RCC_sum_df <- lapply(CORE_fun_out, function(x){x$RCC_sum_mod_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)
SR_log_sum_df <- lapply(CORE_fun_out, function(x){x$SR_log_mod_sum_out}) %>% bind_rows() %>%
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)

# combines the above summary df's with the final classification of each Line of analysis for each module for each sampleid
# This is needed for the rectangle plot in slide 3 (Dashboard sketch v1)
LOE_sum_df <- LOE_sum_fun(SCO_sum = SCO_sum_df, RCC_sum = RCC_sum_df, SR_log_sum = SR_log_sum_df) %>% 
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)

# For coloring the dots on the map, the CSCI plot, and the Module rectangle plot
# Basically this data is for the first two tabs, (Map and Site summary)
LOE_mod_sum_df <- LOE_samp_sum_fun(SCO_sum = SCO_sum_df, RCC_sum = RCC_sum_df, SR_log_sum = SR_log_sum_df) %>% 
  arrange(test_site, sampledate, fieldreplicate, collectionmethodcode)


#### MONITORING NEEDS REPORT (DATA INVENTORY) ####

Dat_invt_df <- Dat_invt_fun(LOEInputData = SR_log_dat_df)


#### EXTRA: CSCI CORE DATA ####
# just for smc sites for now

# CSCI_core <- csci_base_df %>%
#   filter(masterid == "907SDSVC3")

