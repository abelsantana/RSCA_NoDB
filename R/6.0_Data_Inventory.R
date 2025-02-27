# Script in inventory datasets/generate monitoring recommendations

# inventory both test site and comparator sites stressor data, identify/count data occurrences and data gaps

# recommend using LOE data from Stressor Response LOE data prepping, as that stressor data was not filtered/reduced
# SR_log_dat_df

Dat_invt_fun <- function(LOEInputData){

  dat_invt <- LOEInputData %>% 
    # count number of comparator data for each sample and analyte (not including NA data)
    group_by(test_site, sampledate, fieldreplicate, collectionmethodcode, analytename) %>% 
    summarize(n_comp_samples = sum(!is.na(comp_result)),
              test_result = mean(test_result)) %>% 
    ungroup() %>% 
    
    # also count number of samples per test site (not including NA data)
    #  don't want to count replicates/different collection methods, so remove replicates first (same sample date) %>% 
    distinct(test_site, sampledate,analytename, .keep_all = TRUE) %>% 
    
    group_by(test_site, analytename, n_comp_samples) %>% 
    summarize(n_test_samples = sum(!is.na(test_result))) %>% 
    ungroup() %>% 
    
    # re-assing Module just for reference
    mutate(module = case_when(analytename %in% c("Chloride", "SpecificConductivity", "Sulfate", "Total Dissolved Solids") ~ "Conductivity",
                              analytename %in% c("AFDM_Algae", "Chlorophyll a", "Dissolved Oxygen", "Nitrogen,Total","Phosphorus as P") ~ "Eutrophication",
                              analytename %in% c("Ev_FlowHab", "H_AqHab", "H_SubNat", "PCT_SAFN") ~ "Habitat",
                              analytename %in% c("Temperature", "XCMG") ~ "Temperature")) %>% 
    # finally, count number of test samples per module (not including NA data)
    group_by(test_site, module) %>% 
    mutate(n_test_samples_module = sum(n_test_samples)) %>% 
    ungroup() %>% 
    
    # assigning comparator site priorities, per analyte and sample
    mutate(comp_analyte_priority = case_when(n_comp_samples == 0 ~ "Very High",
                                             n_comp_samples < 50 ~ "Moderate",
                                             n_comp_samples >= 50 ~ "Low")) %>% 
    # adding extra information to category
    mutate(comp_analyte_priority_note = case_when(comp_analyte_priority == "Very High" ~ "No comparators have analyte data.",
                                                  comp_analyte_priority == "Moderate" ~ "Less than 50 comparators have analyte data.",
                                                  comp_analyte_priority == "Low" ~ "More than 50 comparators have analyte data.")) %>% 
    
    # assigning test site priorites, also per analyte and sample
    mutate(test_priority = case_when(n_test_samples == 0 & n_test_samples_module == 0 ~ "Very High",
                                     n_test_samples == 0 & n_test_samples_module > 0 ~ "High",
                                     n_test_samples == 1 & n_test_samples_module > 0 ~ "Moderate",
                                     n_test_samples > 1 & n_test_samples_module > 0 ~ "Low",
                                     TRUE ~ "ERROR"),
           test_priority_note = case_when(test_priority == "Very High" ~ "Test site does not have data for this analyte AND does not have data for any other analytes for this module.",
                                          test_priority == "High" ~ "Test site does not have data for this analyte but the module can be assessed with other analytes.",
                                          test_priority == "Moderate" ~ "Test site has been sampled once for this analyte.",
                                          test_priority == "Low" ~ "Test site has been sampled two or more times for this analyte.")) 
  
  dat_invt
  
}




