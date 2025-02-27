# RSCA Ref Condition LOE functions

## This function is designed to evaluate and score data used in a Rapid Screening-Level Causal Assessment using the Reference Condition
## Comparison line of evidence. This line of evidence compares the exposure to an individual measure of stress at the test site compared
## levels of exposure at reference-condition (CSCI score >=0.79) comparator sites.  The line of evidence is applied to each test site in 
## the data set independently.  

# see David's previous script to include further information/text

#### FUNCTION/ANALYSIS ####

# input prepped Reference Condition Comparator Data, CSCI checking data

RCC_fun <- function(RCCData, CSCIcheckoutput){
  
  
  #### Perform Reference Condition Comparison LOE ####
  
  # group by each test csci sample and analyte
  # compare test stress data to analyte population distributions created from observed values at comp sites
  # create scores for results, based on comparisons to quantiles
  # for ex, if test result less than 10th percentile comp result, scored "supporting" evidence (also weakening, indeterminate, no evidence)
  
  # analysis for negative Direction
  quant_neg <- RCCData %>% 
    filter(direction == "Negative") %>% 
    group_by(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, test_result, unit) %>%
    # summarize comp stress data, create quantile stats
    summarise(p10 = quantile(probs = 0.10, comp_result, na.rm = T),
              p25 = quantile(probs=0.25, comp_result, na.rm = T),
              p75 = quantile(probs=0.75, comp_result, na.rm = T),
              p90 = quantile(probs=0.90, comp_result, na.rm = T),
              # count number of comparator data points used, that are not NA values
              n = length(which(!is.na(comp_result)))) %>% 
    ungroup() %>% 
    # compare test score to quantiles, generate summary scores
    mutate(rcc_score = (case_when(n <= 4 ~ "No Evidence", #need 5 or more comparator sites, if not then no evidence,
                                  test_result > p90 ~ "Supporting",
                                  test_result < p75 ~ "Weakening",
                                  test_result >= p75 & test_result <= p90 ~ "Indeterminate",
                                  is.na(test_result) ~ "No Test Data")))
  
           # RCC.Score.2 = (case_when(test_result > p90 ~ 1,
           #                          test_result < p75 ~ -1,
           #                          test_result >= p75 & test_result <= p90 ~ 0,
           #                          is.na(test_result) ~ as.numeric(NA))))
  
  # analysis for positive Direction
  quant_pos <- RCCData %>% 
    filter(direction == "Positive") %>% 
    group_by(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, test_result, unit) %>% 
    summarise(p10 = quantile(probs = 0.10, comp_result, na.rm = T),
              p25 = quantile(probs=0.25, comp_result, na.rm = T),
              p75 = quantile(probs=0.75, comp_result, na.rm = T),
              p90 = quantile(probs=0.90, comp_result, na.rm = T),
              # count number of comparator data points used, that are not NA values
              n = length(which(!is.na(comp_result)))) %>% 
    ungroup() %>% 
    mutate(rcc_score = (case_when(n <= 4 ~ "No Evidence", #need 5 or more comparator sites, if not then no evidence,
                                  test_result < p10 ~ "Supporting",
                                  test_result > p25 ~ "Weakening",
                                  test_result <= p25 & test_result >= p10 ~ "Indeterminate",
                                  is.na(test_result) ~ "No Test Data")))
  
           # RCC.Score.2 = (case_when(test_result < p10 ~ 1,
           #                          test_result > p25 ~ -1,
           #                          test_result <= p25 & test_result >= p10 ~ 0,
           #                          is.na(test_result) ~ as.numeric(NA))))
  
  # combining analysis datasets
  rcc_final <- bind_rows(quant_neg, quant_pos)
  
  #### Finalize results ####
  
  # add comparison to CSCI thresholds, like SCAPE
  # if csci > check_value, then don't report analysis and mark as "Passing CSCI"
  rcc_final_checked <- rcc_final %>% 
    left_join(CSCIcheckoutput, by = c("test_site" = "masterid", "sampledate" = "sampledate",
                                      "collectionmethodcode" = "collectionmethodcode", "fieldreplicate" = "fieldreplicate")) %>% 
    mutate(rcc_score = case_when(csci > check_value ~ "Passing CSCI",
                                TRUE ~ rcc_score),
           # RCC.Score.2 = case_when(csci > check_value ~ as.numeric(NA),
           #                        TRUE ~ RCC.Score.2),
           p10 = case_when(csci > check_value ~ as.numeric(NA),
                           TRUE ~ p10),
           p25 = case_when(csci > check_value ~ as.numeric(NA),
                           TRUE ~ p25),
           p75 = case_when(csci > check_value ~ as.numeric(NA),
                           TRUE ~ p75),
           p90 = case_when(csci > check_value ~ as.numeric(NA),
                           TRUE ~ p90),
           n = case_when(csci > check_value ~ as.integer(NA),
                         TRUE ~ n)) %>% 
    # drop the columns used for checking for now
    select(module, direction, test_site, sampledate,fieldreplicate, collectionmethodcode, analytename, test_result,
           unit, p10, p25, p75,p90, n, rcc_score)
  
  
  # return final dataframe
  rcc_final_checked
  
}

RCC_sum_mod <- function(RCCoutput, CSCIcheckoutput){
  
  RCC_sum <- RCCoutput %>% 
    # create new column for each potential score, filling with a yes/no for the score
    mutate(s_count = ifelse(rcc_score == "Supporting", 1 ,0),
           w_count = ifelse(rcc_score == "Weakening", 1 ,0),
           i_count = ifelse(rcc_score == "Indeterminate", 1 ,0),
           e_count = ifelse(rcc_score == "No Evidence", 1 ,0),
           n_count = ifelse(rcc_score == "No Test Data", 1 ,0),
           p_count = ifelse(rcc_score == "Passing CSCI", 1, 0)) %>% 
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
    mutate(reference_condition = case_when(s_count >= 1 ~ "Supporting",
                                        w_count >= 1 ~ "Weakening",
                                        i_count >= 1 ~ "Indeterminate",
                                        s_count == 0 & w_count == 0 & i_count == 0 & e_count >= 1 ~ "No Evidence",
                                        s_count == 0 & w_count == 0 & i_count == 0 & e_count == 0 & n_count >= 1 ~ "No Test Data",
                                        s_count == 0 & w_count == 0 & i_count == 0 & e_count == 0 & n_count == 0 ~ "Passing CSCI",
                                        TRUE ~ "ERROR"))
}

