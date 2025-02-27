# RSCA spatial-temporal LOE functions

# first function to compare test site data to comparator site stressor dataset (running quantile regressions)
# second function to take those outputs and summarize per Module and Test Sample

#### FUNCTION/ANALYSIS ####

# input prepped Spatial Temporal dataset, CSCI data used for setting "Passing" thresholds
SCO_fun <- function(SCOData, CSCIcheckoutput){
  
  #### Perform Spatial Temporal LOE ####
  
  # group by each test csci sample and analyte
  # compare test stress data to analyte population distributions created from observed values at comp sites
  # create scores for results, based on comparisons to quantiles
  # for ex, if test result less than 10th percentile comp result, scored "supporting" evidence (also weakening, indeterminate, no evidence)
  
  
  # analysis for negative Direction
  quant_neg <- SCOData %>% 
    filter(direction == "Negative") %>% 
    group_by(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, test_result, unit) %>%
    # summarize comp stress data, create quantile stats
    summarise(p25 = quantile(probs=0.25, comp_result, na.rm = T),
              p50 = quantile(probs=0.50, comp_result, na.rm = T),
              p75 = quantile(probs=0.75, comp_result, na.rm = T),
              # count number of comparator data points used, that are not NA values
              n = length(which(!is.na(comp_result)))) %>% 
    ungroup() %>% 
    # compare test score to quantiles, generate summary scores
    mutate(sco_score = (case_when(n <= 4 ~ "No Evidence", #need 5 or more comparator sites, if not then no evidence,
                                 test_result > p75 ~ "Supporting",
                                  test_result < p50 ~ "Weakening",
                                  test_result >= p50 & test_result <= p75  ~ "Indeterminate",
                                  is.na(test_result) ~ "No Test Data")))
  
           # ST.Score.2 = (case_when(test_result > p75 ~ 1,
           #                          test_result < p50 ~ -1,
           #                          test_result >= p50 & test_result <= p75  ~ 0,
           #                          is.na(test_result) ~ as.numeric(NA))))
  
  # analysis for positive Direction
  quant_pos <- SCOData %>% 
    filter(direction == "Positive") %>% 
    group_by(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, test_result, unit) %>% 
    summarise(p25 = quantile(probs=0.25, comp_result, na.rm = T),
              p50 = quantile(probs=0.50, comp_result, na.rm = T),
              p75 = quantile(probs=0.75, comp_result, na.rm = T),
              # count number of comparator data points used, that are not NA values
              n = length(which(!is.na(comp_result)))) %>% 
    ungroup() %>% 
    mutate(sco_score = (case_when(n <= 4 ~ "No Evidence", #need 5 or more comparator sites, if not then no evidence
                                 test_result < p25 ~ "Supporting",
                                  test_result > p50 ~ "Weakening",
                                  test_result <= p50 & test_result >= p25 ~ "Indeterminate",
                                  is.na(test_result) ~ "No Test Data")))
  
           # ST.Score.2 = (case_when(test_result < p25 ~ 1,
           #                          test_result > p50 ~ -1,
           #                          test_result <= p50 & test_result >= p25 ~ 0,
           #                          is.na(test_result) ~ as.numeric(NA))))
  
  # combining analysis datasets
  sco_final <- bind_rows(quant_neg, quant_pos)
  
  #### Finalize Results: Check for CSCI scores passing threshold, check for adequate number of comparators  ####
  
  # add comparison to CSCI thresholds, like SCAPE
  # if csci > check_value, then don't report analysis and mark as "Passing CSCI"
  sco_final_checked <- sco_final %>% 
    left_join(CSCIcheckoutput, by = c("test_site" = "masterid", "sampledate" = "sampledate",
                                      "collectionmethodcode" = "collectionmethodcode", "fieldreplicate" = "fieldreplicate")) %>% 
    mutate(sco_score = case_when(csci > check_value ~ "Passing CSCI",
                                     TRUE ~ sco_score),
           # ST.Score.2 = case_when(csci > check_value ~ as.numeric(NA),
           #                        TRUE ~ ST.Score.2),
           p25 = case_when(csci > check_value ~ as.numeric(NA),
                                  TRUE ~ p25),
           p50 = case_when(csci > check_value ~ as.numeric(NA),
                                  TRUE ~ p50),
           p75 = case_when(csci > check_value ~ as.numeric(NA),
                                  TRUE ~ p75),
           n = case_when(csci > check_value ~ as.integer(NA),
                           TRUE ~ n)) %>% 
    # drop the columns used for checking for now
    select(module, direction, test_site, sampledate,fieldreplicate, collectionmethodcode, csci, analytename, test_result,
           unit, p25, p50, p75, n, sco_score)
    
  
  # return final dataframe
  sco_final_checked
  
}

SCO_sum_mod <- function(SCOoutput, CSCIcheckoutput){
  
  SCO_sum <- SCOoutput %>% 
    # create new column for each potential score, filling with a yes/no for the score
    mutate(s_count = ifelse(sco_score == "Supporting", 1 ,0),
           w_count = ifelse(sco_score == "Weakening", 1 ,0),
           i_count = ifelse(sco_score == "Indeterminate", 1 ,0),
           e_count = ifelse(sco_score == "No Evidence", 1 ,0),
           n_count = ifelse(sco_score == "No Test Data", 1 ,0),
           p_count = ifelse(sco_score == "Passing CSCI", 1, 0)) %>% 
    group_by(module, test_site, sampledate, fieldreplicate, collectionmethodcode) %>% 
    # counting the occurrence of each ST score per test sample/module, adding up/summing the counts
    summarise(s_count = sum(s_count),
              w_count = sum(w_count),
              i_count = sum(i_count),
              e_count = sum(e_count),
              n_count = sum(n_count),
              p_count = sum(p_count)) %>% 
    ungroup() %>% 
    # using those counts, create the final ST Score
    # order is important in the case whens... first look for supporting, then weakening, etc.
    # (if one supporting, whole module is supporting; if no supporting, and at least one weakening, whole module is weakening, etc.)
    mutate(spatial_cooccurrence = case_when(s_count >= 1 ~ "Supporting",
                                        w_count >= 1 ~ "Weakening",
                                        i_count >= 1 ~ "Indeterminate",
                                        s_count == 0 & w_count == 0 & i_count == 0 & e_count >= 1 ~ "No Evidence",
                                        s_count == 0 & w_count == 0 & i_count == 0 & e_count == 0 & n_count >= 1 ~ "No Test Data",
                                        s_count == 0 & w_count == 0 & i_count == 0 & e_count == 0 & n_count == 0 ~ "Passing CSCI",
                                        TRUE ~ "ERROR"))
  
  
}





