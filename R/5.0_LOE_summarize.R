# Summarizing LOEs and Modules per Sites/samples for Dashboard

# how to deal with Linear vs Logistic Stressor Response summary?

# reworking sample summary, so uses new RSCA scoring rules
# want to add up the counts across all LOEs, so bind each LOE result long
# as bind LOE results together, create one column of all the scores
# then summarize across ST/RCC/SR LOES, using group/counting as before

# Supress Summeraize messages globaly
options(dplyr.summarise.inform = FALSE)

LOE_sum_fun <- function(SCO_sum, RCC_sum, SR_log_sum){
  
  #summarize per sample, per LOE, across modules
  LOE_sum <- SCO_sum %>%
    mutate(loe = "Spatial Co-Occurrence") %>% 
    rename(score = spatial_cooccurrence) %>% 
    bind_rows(RCC_sum %>% rename(score = reference_condition) %>% mutate(loe = "Reference Condition")) %>% 
    bind_rows(SR_log_sum %>% rename(score = stressor_response) %>% mutate(loe = "Stressor Response")) %>% 
    select(module,test_site, sampledate, fieldreplicate, collectionmethodcode, loe, score)
  # pivot_longer(c(Spatial.Temporal, Stressor.Response, Reference.Condition), names_to = "LOE", values_to = "Score")
  
  LOE_sum
  
}

LOE_samp_sum_fun <- function(SCO_sum, RCC_sum, SR_log_sum){
  # summarize per sample, per module, across LOEs
  LOE_samp_sum <- SCO_sum %>%
    mutate(loe = "Spatial Co_Occurrence") %>% 
    rename(score = spatial_cooccurrence) %>% 
    bind_rows(RCC_sum %>% rename(score = reference_condition) %>% mutate(loe = "Reference Condition")) %>% 
    bind_rows(SR_log_sum %>% rename(score = stressor_response) %>% mutate(loe = "Stressor Response")) %>% 
    select(module,test_site, sampledate, fieldreplicate, collectionmethodcode, loe, score) %>% 
    # create new columns for each potential score, filling with a yes/no for the score
    mutate(s_count = ifelse(score == "Supporting", 1 ,0),
           w_count = ifelse(score == "Weakening", 1 ,0),
           i_count = ifelse(score == "Indeterminate", 1 ,0),
           e_count = ifelse(score == "No Evidence", 1 ,0),
           n_count = ifelse(score == "No Test Data", 1 ,0),
           p_count = ifelse(score == "Passing CSCI", 1, 0)) %>% 
    group_by(module,test_site, sampledate, fieldreplicate, collectionmethodcode) %>% 
    summarise(s_count = sum(s_count),
              w_count = sum(w_count),
              i_count = sum(i_count),
              e_count = sum(e_count),
              n_count = sum(n_count),
              p_count = sum(p_count)) %>% 
    ungroup() %>% 
    mutate(overall = case_when(p_count > 1 ~ "Passing CSCI",
                               s_count >= 1 & s_count > w_count ~ "Likely Cause",
                               w_count >= 1 & w_count > s_count ~ "Unlikely Cause",
                               s_count == w_count & s_count >=1 ~ "Indeterminate Cause",
                               i_count >= 1 & s_count == 0 & w_count == 0 ~ "Indeterminate Cause",
                               # cannot be evaluated when no test data or no evidence across LOEs
                               p_count == 0 & s_count == 0 & w_count == 0 & i_count ==0 ~ "Cannot be Evaluated",
                               TRUE ~ "ERROR")) %>% 
    select(module, test_site, sampledate, fieldreplicate, collectionmethodcode, overall) %>% 
    tidyr::pivot_wider(names_from = module, values_from = overall) %>% 
    # add csci score
    left_join(csci_base_df %>% select(masterid, sampledate, collectionmethodcode, fieldreplicate, csci), by = c("test_site"= "masterid", "sampledate",
                                                                                                                "fieldreplicate","collectionmethodcode")) %>% 
    # add comid, so can join scape data
    left_join(station_base_df %>% select(masterid, comid, latitude,longitude), by = c("test_site" = "masterid")) %>% 
    # add scape data
    left_join(scape %>% select(comid, qt10, qt50, qt90) %>% mutate(comid = as.character(comid)), by = c("comid" = "comid")) %>% 
    mutate(CSCI_category_scape = case_when(csci >= qt10 ~ "Within or Above Expectation",
                                           csci < qt10 ~"Below Expectation")) %>% 
    mutate(CSCI_category = case_when(csci >= 0.79 ~ "≥ 0.79",
                                     csci < 0.79 ~ "< 0.79")) %>% 
    # final columns
    select(test_site, comid, latitude, longitude, sampledate, fieldreplicate, collectionmethodcode,  csci, qt10, qt50, qt90,
           csci_category_scape = CSCI_category_scape, csci_category = CSCI_category,
           conductivity = Conductivity, eutrophication = Eutrophication, habitat = Habitat, temperature = Temperature)
  
  LOE_samp_sum
}
