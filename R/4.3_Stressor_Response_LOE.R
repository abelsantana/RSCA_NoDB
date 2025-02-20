# RSCA Stressor-Response LOE functions

SR_log_fun <- function(SRData, CSCIcheckoutput){
  
  ####Prepare Data for Nested Modeling ####
  
  # will score each stressor measure within each module for every sample 
  # create input comparator dataset, and also input test site dataset for predicting
  # for now, mapped models seem to work best when both those datasets are the same length, even though test sites get repeated
  # POTENTIAL FIX, CHECK FOR CASE WHEN NO COMPARATOR STRESSOR DATA PER MODULE/ANALYTE , is this 'No Evidence'??
  
  mod_comp_data <- SRData %>% 
    # want dataset per module, analyte, and sample for each site
    group_by(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode) %>% 
    # only keep data points when at least 10 per group
    mutate(n = length(which(!is.na(comp_result)))) %>% 
    filter(n >= 10) %>% 
    select(-n) %>% 
     # running model on comparator results, but rename to result
    rename(result = comp_result) %>% 
    # nest each group into its own dataset, so can perform analysis on each group
    nest() %>% 
    ungroup()
  
  # a second dataset for comparator data, when not enough comparator points to make model
  # want 10 or more data points
  # this immediately gets assigned "No Evidence" when gets added back later
  mod_comp_data_noev <- SRData %>% 
    group_by(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode) %>% 
    summarize(n = length(which(!is.na(comp_result)))) %>% 
    filter(n < 10) %>% 
    ungroup()
  
  mod_test_data <- SRData %>% 
    # want dataset per module, analyte, and sample for each site
    group_by(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode) %>% 
    # only keep data points when at least 10 per group
    mutate(n = length(which(!is.na(comp_result)))) %>% 
    filter(n >= 10) %>% 
    select(-n) %>% 
    # rename test result as result, so can use in model as new data 
    rename(result = test_result) %>% 
    # nest each group into its own dataset, so can perform analysis on each group
    nest() %>% 
    ungroup()
  
  # second dataset for test results without enough comps
  # will be scored as no evidence
  mod_test_data_noev <- SRData %>% 
    group_by(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode) %>% 
    summarize(n = length(which(!is.na(comp_result)))) %>% 
    filter(n < 10) %>% 
    select(-n) %>% 
    mutate(SR.score_extra = "No Evidence") %>% 
    ungroup()
  
#### Perform Stressor Response LOE, Logistic ####
  
  # if we have at least one sample/module/analyte with enough comparator data points, run model/extract output/summarize
  # basically, nested dataset has to have at least one row
  
  if(nrow(mod_comp_data) > 0){

    # run logistic regression per nested group using comparator stressor results and ref condition
    # then apply model to predict values for test stressor results
    # model outputs and predicted values are stored in the nested columns 
    SR_log_model <- mod_comp_data %>% 
      mutate(mod_test_data = mod_test_data$data) %>% 
      mutate(model = purrr::map(data, ~glm(cond2_ref_y ~ result, family = binomial(link = "logit"), data = . )),
             predicted = purrr::map2(model, mod_test_data, predict.glm, type = "response", se.fit = T)) 
    # note: we seems to get warnings in glms due to outliers in stressor data?? will have to add outlier analysis to data prepping
    
    SR_log_model_output <- SR_log_model %>%
      # extract model results per group, using broom's tidy() results
      mutate(summary = purrr::map(model, broom::tidy)) %>%
      unnest(summary) %>%
      # can drop any of the nested results
      select(-data,-mod_test_data, -model, -predicted) %>% 
      #David's suggested changes for adding stressor thresholds
      #Identifying the parameters to put in the logistic regression equation and put them on one line for each combination
        #of module+analyte+sampleid
      left_join(., (pivot_wider(.,id_cols = c(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode), names_from=term, values_from=estimate)),
                by=c("module", "analytename", "test_site", "sampledate", "fieldreplicate", "collectionmethodcode")) %>% 
      # now now just keep the result row, since we added the intercept estimate with the join
      filter(term=="result") %>%
      # final columns
      select(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode, intercept = `(Intercept)`, result, std.error, model_p_value=p.value) %>% 
      #using the logistic regression equation to calculate stressor values at 0.6 and 0.4 for each model@
      #logstic equation: ln(probabilty of result/(1-probability))=intercept + slope*stressor value
      mutate(threshold_60=((log(0.6/(1-0.6))-intercept)/result),
             threshold_40=((log(0.4/(1-0.4))-intercept)/result) ) %>% 
      select(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode, model_p_value, threshold_60, threshold_40)
    
    SR_log_pred_output <- SR_log_model %>% 
      # extract prediction results, need to add results to nested dataframes before unnesting
      mutate(summary = map(predicted, data.frame))%>% 
      unnest(summary) %>% 
      # drop nested datasets/models
      select(-data,-mod_test_data, -model, -predicted) %>% 
      # keep only one row per test stressor result (the test data had repeated due to alignment with comparator data)
      distinct() %>% 
      rename(p_of_bad = fit, se_of_p = se.fit) %>% 
      # final column selection
      select(module, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode, p_of_bad, se_of_p)
    
    # create final results dataset:
    # add test site data info
    # join model and prediction outputs
    # create LOE summary scores 
    SR_log_output <- SRData %>% 
      select(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode,analytename, test_result, unit, csci) %>% 
      distinct( test_site, sampledate, fieldreplicate, collectionmethodcode,analytename,test_result, .keep_all = TRUE) %>% 
      # adding the no evidence datapoints due to lack of comp data
      left_join(mod_test_data_noev, by = c("module", "test_site", "sampledate", "fieldreplicate", "collectionmethodcode", "analytename")) %>% 
      # adding rest of results
      left_join(SR_log_model_output, by = c("module", "test_site", "sampledate", "fieldreplicate", "collectionmethodcode", "analytename")) %>% 
      left_join(SR_log_pred_output, by = c("module", "test_site", "sampledate", "fieldreplicate", "collectionmethodcode", "analytename")) %>% 
      mutate(sr_score=case_when(SR.score_extra == "No Evidence" ~ "No Evidence",
                                is.na(test_result) ~ "No Test Data",
                                model_p_value>0.1~"No Evidence",
                                p_of_bad>=0.6~"Supporting",
                                p_of_bad<0.6 & p_of_bad>=0.4~"Indeterminate",
                                p_of_bad<0.4~"Weakening",
                                TRUE~"ERROR")) %>% 
      # ,
      # SR.score.2= case_when(p_of_bad>=0.6 ~ 1,
      #                       p_of_bad<0.6 & p_of_bad>=0.4 ~ 0,
      #                       p_of_bad<0.4 ~ -1)) %>% 
      # mutate(SR.score.2=as.numeric(ifelse(model.p.value>0.1, NA, SR.score.2))) %>% 
      select(module, direction, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode, test_result, unit, csci, prob_of_poor=p_of_bad, se_of_prob=se_of_p,
             model_p_value, threshold_60, threshold_40, sr_score)
    
  } else {
    
    
    # when not enough comparator data for all analytes/models, then just use "No Evidence" scores
    
    SR_log_output <- SRData %>% 
      select(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode,analytename, test_result, unit, csci) %>% 
      distinct(test_site, sampledate, fieldreplicate, collectionmethodcode,analytename,test_result, .keep_all = TRUE) %>% 
      # adding the no evidence datapoints due to lack of comp data
      left_join(mod_test_data_noev, by = c("module", "test_site", "sampledate", "fieldreplicate", "collectionmethodcode", "analytename")) %>%
      # creating model details columns, but NAs for data
      mutate(sr_score = SR.score_extra, prob_of_poor = as.numeric(NA), se_of_prob = as.numeric(NA),model_p_value = as.numeric(NA),
             threshold_60 = as.numeric(NA), threshold_40 = as.numeric(NA)) %>%  
      select(module, direction, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode, test_result, unit, csci, prob_of_poor, se_of_prob,
             model_p_value, threshold_60, threshold_40, sr_score)
    
  }
  
  
  #### Check/finalize results ####
  
  # add comparison to CSCI thresholds, like SCAPE
  # if csci > check_value, then don't report analysis and mark as "Passing CSCI"
  SR_log_output_checked <- SR_log_output %>% 
    left_join(CSCIcheckoutput %>% select(-csci), by = c("test_site" = "masterid", "sampledate" = "sampledate",
                                      "collectionmethodcode" = "collectionmethodcode", "fieldreplicate" = "fieldreplicate")) %>% 
    mutate(sr_score = case_when(csci > check_value ~ "Passing CSCI",
                                TRUE ~ sr_score),
           # SR.score.2 = case_when(csci > check_value ~ as.numeric(NA),
           #                        TRUE ~ SR.score.2),
           prob_of_poor = case_when(csci > check_value ~ as.numeric(NA),
                                 TRUE ~ prob_of_poor),
           se_of_prob = case_when(csci > check_value ~ as.numeric(NA),
                                     TRUE ~ se_of_prob),
           model_p_value = case_when(csci > check_value ~ as.numeric(NA),
                                   TRUE ~ model_p_value)) %>% 
    # drop the columns used for checking for now
    select(module, direction, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode, test_result, unit, csci, prob_of_poor, se_of_prob,
           model_p_value, threshold_60, threshold_40, sr_score)
  
  SR_log_output_checked
    
}
SR_log_mod_sum <- function(SRlogoutput, CSCIcheckoutput){
  
  
  SR_log_sum <- SRlogoutput %>% 
    # create new column for each potential score, filling with a yes/no for the score
    mutate(s_count = ifelse(sr_score == "Supporting", 1 ,0),
           w_count = ifelse(sr_score == "Weakening", 1 ,0),
           i_count = ifelse(sr_score == "Indeterminate", 1 ,0),
           e_count = ifelse(sr_score == "No Evidence", 1 ,0),
           n_count = ifelse(sr_score == "No Test Data", 1 ,0),
           p_count = ifelse(sr_score == "Passing CSCI", 1, 0)) %>% 
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
    mutate(stressor_response = case_when(s_count >= 1 ~ "Supporting",
                                           w_count >= 1 ~ "Weakening",
                                           i_count >= 1 ~ "Indeterminate",
                                           s_count == 0 & w_count == 0 & i_count == 0 & e_count >= 1 ~ "No Evidence",
                                           s_count == 0 & w_count == 0 & i_count == 0 & e_count == 0 & n_count >= 1 ~ "No Test Data",
                                           s_count == 0 & w_count == 0 & i_count == 0 & e_count == 0 & n_count == 0 ~ "Passing CSCI",
                                           TRUE ~ "ERROR"))
}
