# Functions to export intermediate datasets, used for LOE analysis functions
# require data generated in SMC Data Prepping, and also Comparator site data from Comp Selection Function

#### SPATIAL CO-OCCURENCE DATA PREPPING ####

# Generate dataset of comparator site stressor data per test site
# input is TestID, and its correponsinding comparator sites generated in 0.1_Comparator_Site_selection.R
SCO_dat_fun <- function(TestID, CompSites){
  
  #### STEP 1: assemble stressor/csci data for test site and its specific comparator sites ####
  
  test_stress_csci <- stressor_csci_base_df %>% 
    filter(masterid == TestID) %>%
    select(masterid, sampledate, fieldreplicate, collectionmethodcode, csci, analytename, result, unit) %>% 
    # adding in direction assignments
    left_join(rsca_module_direction, by = "analytename") %>% 
    # defining as test result
    rename(test_result = result, test_site = masterid) %>% 
    # making a sampleid column, to define unique sampling events for each site
    # mutate(test_csci_sampleid = paste(test_site, sampledate, fieldreplicate, collectionmethodcode, sep = "_")) %>% 
    select(module,direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, test_result, unit, csci)
  
  
  comp_sites <- CompSites %>% 
    filter(test_site == TestID) %>%
    select(test_site, comp_site)
  
  comp_stress_csci <- stressor_csci_base_df %>%
    # retain just relevant comparator sites
    filter(masterid %in% comp_sites$comp_site) %>%
    # add direction assignments 
    left_join(rsca_module_direction, by = "analytename") %>%
    # defining as comparator result
    rename(comp_site = masterid, comp_result = result, comp_csci = csci, comp_unit = unit) %>% 
    # making sampleid column
    mutate(comp_csci_sampleid = paste(comp_site, sampledate, fieldreplicate, collectionmethodcode, sep = "_")) %>%
    select(module, direction, comp_site, comp_csci_sampleid, comp_csci, analytename, comp_result, comp_unit)
  
  #### STEP 2: combine into one final stressor dataset (test vs comp) and retain only sites with higher scoring Comp CSCI scores ####
  
  # want each test site and test stress result associated with comp site and comp stress data
  # TODO Raphs fix for missing data
  # check to see if any test sites do not have comp sites that pass the filter comp_csci > csci
  test_comp_data_check <- test_stress_csci %>% 
    left_join(comp_sites, by = c("test_site")) %>% 
    left_join(comp_stress_csci, by = c("comp_site", "module", "direction", "analytename")) %>%
    mutate(filter_pass = ifelse(comp_csci > csci, "Yes", "No"),
           unique_id = paste(test_site, sampledate, fieldreplicate, collectionmethodcode, sep = "_")) %>%
    group_by(unique_id) %>%
    summarise(has_compsites = any(filter_pass == "Yes", na.rm = TRUE)) %>%
    ungroup()
  
  # the original code
  test_comp_data <- test_stress_csci %>% 
    #associating each site with its comparator sites (each sample per site will have the same comp sites)
    left_join(comp_sites, by = c("test_site")) %>% 
    #adding comp stress data
    left_join(comp_stress_csci, by = c("comp_site", "module", "direction", "analytename")) %>% 
    #only retain data when Comp CSCI better than test CSCI
    filter(comp_csci > csci)
  
}

#### REFERENCE CONDITION COMPARISON DATA PREPPING ####


RCC_dat_fun <- function(TestID, CompSites){
  
  #### STEP 1: assemble stressor/csci data for test site and its specific comparator sites ####
  
  test_stress_csci <- stressor_csci_base_df %>% 
    filter(masterid == TestID) %>%
    select(masterid, sampledate, fieldreplicate, collectionmethodcode, csci, analytename, result, unit) %>% 
    # adding in direction assignments
    left_join(rsca_module_direction, by = "analytename") %>% 
    # defining as test result
    rename(test_result = result, test_site = masterid) %>% 
    # making a sampleid column, to define unique sampling events for each site
    # mutate(test_csci_sampleid = paste(test_site, sampledate, fieldreplicate, collectionmethodcode, sep = "_")) %>% 
    select(module,direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, test_result, unit, csci)
  
  comp_sites <- CompSites %>% 
    filter(test_site == TestID) %>%
    select(test_site, comp_site)
  
  comp_stress_csci <- stressor_csci_base_df %>%
    # retain just relevant comparator sites
    filter(masterid %in% comp_sites$comp_site) %>%
    # add direction assignments 
    left_join(rsca_module_direction, by = "analytename") %>%
    # defining as comparator result
    rename(comp_site = masterid, comp_result = result, comp_csci = csci, comp_unit = unit) %>% 
    # making sampleid column
    mutate(comp_csci_sampleid = paste(comp_site, sampledate, fieldreplicate, collectionmethodcode, sep = "_")) %>%
    select(module, direction, comp_site, comp_csci_sampleid, comp_csci, analytename, comp_result, comp_unit)
  
  #### STEP 2: combine into one final stressor dataset (test vs comp) and retain only reference condition comparator sites (CSCI score >=  0.79) ####
  
  # want each test site and test stress result associated with comp site and comp stress data
  
  test_comp_data <- test_stress_csci %>% 
    # associating each site with its comparator sites (each sample per site will have the same comp sites)
    left_join(comp_sites, by = c("test_site")) %>% 
    # adding comp stress data
    left_join(comp_stress_csci, by = c("comp_site", "module", "direction", "analytename")) %>% 
    # retaining only reference condition comparators (should this step still be done at this point? or earlier?)
    filter(comp_csci >= 0.79)
  
  
}



SR_log_dat_fun <- function(TestID, CompSites){
  
  #### STEP 1: assemble stressor/csci data for test site and its specific comparator sites ####
  
  test_stress_csci <- stressor_csci_base_df %>% 
    filter(masterid == TestID) %>%
    select(masterid, sampledate, fieldreplicate, collectionmethodcode, csci, analytename, result, unit) %>% 
    # adding in direction assignments
    left_join(rsca_module_direction, by = "analytename") %>% 
    # defining as test result
    rename(test_result = result, test_site = masterid) %>% 
    # making a sampleid column, to define unique sampling events for each site
    # mutate(test_csci_sampleid = paste(test_site, sampledate, fieldreplicate, collectionmethodcode, sep = "_")) %>% 
    select(module,direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, test_result, unit, csci)
  
  comp_sites <- CompSites %>% 
    filter(test_site == TestID) %>%
    select(test_site, comp_site)
  
  comp_stress_csci <- stressor_csci_base_df %>%
    # retain just relevant comparator sites
    filter(masterid %in% comp_sites$comp_site) %>% 
    # add direction assignments 
    left_join(rsca_module_direction, by = "analytename") %>%
    # defining as comparator result
    rename(comp_site = masterid, comp_result = result, comp_csci = csci, comp_unit = unit) %>% 
    # making sampleid column
    mutate(comp_csci_sampleid = paste(comp_site, sampledate, fieldreplicate, collectionmethodcode, sep = "_")) %>%
    select(module, direction, comp_site, comp_csci_sampleid, comp_csci, analytename, comp_result, comp_unit)
  
  
  #### STEP 2: combine into one final stressor dataset (test vs comp)  ####
  
  # want each test site and test stress result associated with comp site and comp stress data
  # then create categories for test site conditions compared to comparator site 
  # why does David's script rename all comp csci (from comparator csci dataset) to "Comp.MaxCSCI'?
  
  test_comp_data <- test_stress_csci %>% 
    # associating each site with its comparator sites (each sample per site will have the same comp sites)
    left_join(comp_sites, by = c("test_site")) %>% 
    # adding comp stress data
    left_join(comp_stress_csci, by = c("comp_site", "module", "direction", "analytename")) %>% 
    
    # create categories for sample condition, based on test vs comparator csci scores
    mutate(cond2_test = factor((if_else(comp_csci > csci,"Better", "Worse"))),
           cond2_ref_y = (if_else(comp_csci >= 0.79, 0, 1)), 
           cond2_ref = (if_else(comp_csci >= 0.79, "Reference","Non-Reference")))
  
  test_comp_data
  
}


