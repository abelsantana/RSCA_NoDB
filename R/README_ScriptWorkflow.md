# RSCA Script Guidance
by Annie Holt & Rachel Darling 


### Overview:

This workflow was designed to create batch RSCA results supporting the development of a RSCA Dashboard (the results mapping/data exploration tool).
That dashboard currently only displays San Gabriel sites, but this workflow and dashboard were designed so that they can be easily expanded in the future.
R scripts and functions for this project repo have been organized in an ordered fashion, and they are named with leading numbers to designate that order.
There are also currently two un-numbered scripts (*RSCA_Explore.R* and *RSCA_Graphing.R*). Those are for running new test sites and graphing and/or exporting the results locally (this is so that RSCA functions can also be run, tested, graphed etc. without messing with database scripts).

Quick Take :

+ All RSCA analysis steps (data prep, executing RSCA functions, summarizing, exporting results to database) can be executed through *main.R*.
    - So to generate or refresh RSCA results in the SMC database tables (which are currently queried in the Dashboard), only need to run *main.R*.
+ Most RSCA analysis steps (data prep, executing RSCA functions, summarizing) are sourced in *0.2_Core.R*; Core just does not include the export to database. (the Core is sourced in main)
    - So to change the list of test sites, or to alter the overall way the RSCA analysis functions are wrapped together or extracted, work on *0.2_Core.R*.
+ Scripts numbered 1.0 through 6.0 house the various individual RSCA analysis and summarizing functions (see Function Details section)
    - So if functions/analysis/summaries need to be updated or improved, will need to update those scripts and functions.

### R Workflow:

The workflow is as follows (all executed through *main.R*):

1. Assemble SMC datasets, to create base datasets for RSCA analysis (function loaded from *0.1_Data_Prepping.R*)
2. Run RSCA Core Script (*0.2_RSCA_Core.R*), which executes the following:
+ Load base data (in Base_Files), packages, and RSCA functions (in 1.0 through 6.0 Scripts)
+ Assign batch of test sites (currently San Gabriel sites), and run analyses steps in order as below (designed to run with one input test site at a time):
  - Check if test site CSCI scores are passing/failing (Casaul Assessment is only triggered for under-performing sites): *1.0_Test_CSCI_check.R*
  - Identify comparator sites for each test site, assemble into comparator dataset for subsequent use: *2.0_Comparator_Site_Selection.R*
  - Prepare datasets for each Line of Evidence; each analysis has a uniquely prepped dataset: *3.0_LOE_Data_Prep.R*
  - Run LOE analysis for each analyte, aggregate results per Module per LOE (Spatial Co-Occurrence, Reference Condition Comparison, Stressor Response). Note that Stressor Response analysis was tested with linear and logistic models, and logistic were chosen for final LOE: *4.1_Spatial_CoOccurrence_LOE.R, 4.2_Ref_Condition_Comp_LOE.R, 4.3_Stressor_Response_LOE.R*
+ Summarize results across all LOEs, resulting in an overall performance score for each Module (inputs are the LOE outputs): *5.0_LOE_summarize.R*
+ Report test site and comparator site inventory/data gaps (inputs are the underlying LOE input datasets): *6.0_Data_Inventory.R*
3. Write underlying data and results to SMC database tables

### Plotting & Analysis
Writing the data
*0.1_Data_Prepping.R*: Assemble SMC datasets, to create base datasets for RSCA analysis (only needed if wanting to re-save/update base datasets). This script does not need specific site input, it simply formats the data from the SMC tables.  

*0.2_RSCA_Core.R*: Run RSCA Core Script, which executes the following:
+ Load base data (in Base_Files), packages, and RSCA functions (in 1.0 through 6.0 Scripts)
+ Assign batch of test sites, and run analyses 
  
*main*: Creates the data tables with the data from *0.2_RSCA_Core* and then adds said data to the SMC database. This must be updated when there are new sites added (so the plotting scripts can pull the correct data). 

For producing plots and excel sheets:
*RSCA_graphing.R*:  This script creates the first 2 plots that are used on the RSCA dashboard. These plots are the CSCI Score plot and the rectangular module plot. 

*plots.R*: this contains the code for the plots but the code gets sourced by script plumber.R. If changes need to be made to the visual aspects of the plots, edit plots.R. This script was originally pulled from the repository: SCCWRP/sgrrmp_dashboard. File has been edited for current use.

*plumber.R*: Sources plots.R and creates the remainder of the graphs. This script was originally pulled from the repository: SCCWRP/sgrrmp_dashboard. File has been edited for current use.

*excel_sheets.R*: Lastly, this script creates the excel sheets that are output in the dashboard. These excel sheets are the tables that are in the SMC dashboard, just printed out and digestible. Formatting of the columns and printing/saving the sheets all occurs in this single script. 


### Additional Dashboard Notes:

We created a mapping application to display our various generated RSCA results for the San Gabriel group and their sampling locations.

From Robert: There are 9 table outputs which are currently used by the dashboard app, and they are stored in the SMC database. This repository has been cloned to the SMC server and will be set to run periodically on the server to update those analysis tables. The code is executed through a docker container on the server with the name rsca_analysis, which is a container instance ran from the image "rocker/r-base:4.0.1". Inside the container Robert installed the necessary R packages as well as the corresponding necessary debian packages (libpq-dev, libopenssl-dev and a few others, but when an R package installation fails, it typically tells you which debian package is needed to install). Robert committed the container to an image (on the server, not in dockerhub) called sccwrp/rsca_analysis:latest.


### Function Details:

**prep_smc_data**
(in *0.1_SMC_Data_Prepping.R*): This function is designed to populate the underlying biological/chemical/habitat/station datasets needed for our RSCA analyses. All required data is queried from the SMC Database (using SCCWRP's in-house read-only connection), and the final datasets are saved as this project's 'Base_Files' in RData file format. Note that the database query involves both SQL statemetns and subseqent R filtering/cleaning (this is generally so that we don't have to import the entire unified tables into R); thus, to update the query, you may have to edit both the SQL statement and subsequent R code. These 'Base_Files' are loaded and used as inputs in subsequent analysis functions. Running this *prep_smc_data* function re-writes the existing Base_Files, adding any new data from the database. It only need to be run when the user wants to refresh the datasets, as the 'Base_Files' are already saved with the project.

This function generates the following datasets (all part of the **Base_Data.RData** file) :

+ **csci_base_df**: Dataframe of all available samples with their respective CSCI scores, along with other underlying metrics.
+ **stressor_csci_base_df** : Dataframe of field and lab results for selected analytes per Module, corresponding to the samples with CSCI scores.
  - Conductivity Module: Chloride, Specific Conductivity, TDS, Sulfate
  - Habitat Module: PCT_SAFN, Ev_FlowHab, H_AqHab, H_SubNat
  - Temperature Module: Temperature, XCMG
  - Eutrophication Module: AFDM_Algae, Chlorophyll a, Dissolved Oxygen, Nitrogen,Total, Phosphorus as P 
+ **oe_base_df**: Dataframe of OTUs and their modeled capture probabilities, corresponding to stations and their locations/lat/longs.
+ **station_base_df**: Dataframe of all CSCI samples and their corresponding latitude/longitude/county/huc/comid/stationname

**CORE_fun**
(in *0.2_Core.R*): This function wraps the various current Lines of Evidence (LOE) functions (three overall LOEs: Spatial Temporal, Reference Condition, Stressor Response) as well as the comparator selection function, so all analyses can be run at once and in order. The function writes all output dataframes into a list. Other subsequent sections of *0.2_Core.R* extract and summarize those outputs by applying *5.0_LOE_summarize.R* and *6.0_Data_Inventory.R* functions, which prepares data for reporting and visualizations.

**CSCI_check_fun**
(in *1.0_Test_CSCI_check.R*): This function assembles CSCI data for the input test sites, and checks if the test site CSCI scores meet the conditions for Causual Assessment (the check is sample-specific). User can choose if the CSCI scores should be compared to SCAPE expectations, or the 0.79 threshold. Sites that are equal to or greater than the chosen threshold should not be assessed, and are scored as "Passing CSCI". This CSCI check is required before any causal assessment.

**Comp_Select_fun**
(in *2.0_Comparator_Site_Selection.R*): This function identifies Comparator Sites for each casual assessment test site (based on stream benthic macroinvertebrates data across California), and produces a dataset of those comparator sites labled with their matching test site.
[Description from David] The function is built upon the approach described in Gillett et al. 2019 "Selecting comparator sites for ecological causal assessment based on expected biological similarity" in Freshwater Science. It uses the terminology described in that manuscript, where the site on which the assessment is to be done is referred to as the "Test Site", ecologically similar sites used in analysis of the test site are referred to as "Comparator Sites", and the ecological similarity of sites to each other is measured as Bray-Curtis (BC) dissimilarity calculated from CSCI OTU capture probabilities and referred to as Comparator Similarity.

**SCO_dat_fun**
(in *3.0_LOE_Data_Prep.R*): This function prepares test site and comparator site data for the Spatial Co-Occurrence (previously Spatial Temporal) LOE analysis (in the SCO LOE, each test result is compared to quantiles of the distribution of the comparator results). The final dataset includes test sample results for each analyte and Module aligned with the comparator results. If there are multiple test results/samples for an analyte (say AFDM was measured both in 2015 and 2016 for site B), the comparator dataset is then repeated/the same for that analyte (the same AFDM comparator dataset will be used for each).

**RCC_dat_fun**
(in *3.0_LOE_Data_Prep.R*): This function prepares test site and comparator site data for the Reference Condition Comparison LOE analysis (in the RCC LOE, each test result is compared to the level of exposure at the reference condition comparator sites where CSCI score >= 0.79). The final dataset includes test sample results for each analyte and Module aligned with the comparator results, but only the compator results for the comparator sites where CSCI score >= 0.79. As before, the comparator results are repeated if an anlyte was measured multiple times at a test site.

**SR_log_dat_fun**
(in *3.0_LOE_Data_Prep.R*): This function prepares test site and comparator site data for the logistic Stressor Response LOE analysis (in the SR LOE, each test result is compared to an expected level of biotic response inferred from the combination of stressor and response observed at appropriate comparator sites). The final dataset includes test sample results for each analyte and Module aligned with the comparator results, with an additional binary response category on whether the comparator CSCI is > 0.79 or not. As before, the comparator results are repeated if an analyte was measured multiple times at a test site.

**SCO_fun**
(in *4.1_Spatial_CoOccurrence_LOE.R*): This function runs the Spatial Co-Occurrence LOE analysis per test sample and analyte. Generally, the test site stressor results are compared to percentiles of the corresponding comparator site stressor results to generate LOE Scores (sco_score). For example, if the test result value is higher than the value of the 75th percentile of comparator results, this leads to a "Supporting" conclusion that the site is likely stressed for that analyte. Note that the quantile comparisons depend on the "direction"" of the bio/stressor relationship (negative/positive). If the sample has a "Passing CSCI", the "sco_score" is overwritten on the backend. The function also outputs data of analysis details, like 'p25' and 'n' for the number of comparator data points.

**SCO_sum_mod**
(in *4.1_Spatial_CoOccurrence_LOE.R*): This function summarizes the Spatial Co-Occurrence LOE scores across analytes in each Module. The count of each score category (Supporting, Weakening, etc.) is added up per Module, and the overall "spatial_co_occurrence" score is determined based on those counts. Note that there is generally a 'one in all in' pattern. For example, if one or more of the analytes is "Supporting", then the whole module is scored "Supporting". Followed by "Weakening", "Indeterminate", "No Evidence", "No Test Data", in that respective priority. 

**RCC_fun**
(in *2_Ref_Condition_Comp_LOE.R*): This function runs the Reference Condition Comparison LOE analysis per test sample and analyte. Generally, the test site stressor results are compared to percentiles of the corresponding reference condition comparator site stressor results (samples whose CSCIs where greater than 0.79) to generate LOE Scores (rcc_score). For example, if the test result value is higher than the value of the 75th percentile of comparator results, this leads to a "Supporting" conclusion that the site is likely stressed for that analyte. Note that the quantile comparisons depend on the "direction"" of the bio/stressor relationship (negative/positive). If the sample has a "Passing CSCI", the "sco_score" is overwritten on the backend. The function also outputs data of analysis details, like 'p25' and 'n' for the number of comparator data points.


**RCC_sum_mod**
(in *2_Ref_Condition_Comp_LOE.R*): This function summarizes the Reference Condition Comparison LOE scores across analytes in each Module. The count of each score category (Supporting, Weakening, etc.) is added up per Module, and the overall "spatial_cooccurrence" score is determined based on those counts. Note that there is generally a 'one in all in' pattern. For example, if one or more of the analytes is "Supporting", then the whole module is scored "Supporting". Followed by "Weakening", "Indeterminate", "No Evidence", "No Test Data", in that respective priority. 

**SR_log_fun**
(in *4.3_Stressor_Response_LOE.R*): This function runs the (logistic) Stressor Response LOE analysis per test smaple and analyte. The script  creates logistic regressions per nested comparator data groups using comparator stressor results and the reference/non-reference site condition (based on CSCI score). This results in probability estimates of observing a poor CSCI score across stress gradients. The 0.4 and 0.6 probabilities are used to compare to the test site results and determine if the score is "Supporting", "Weakening", etc. The function also outputs data of anlaysis details, including "prob_of_poor", "se_of_prob", "model_p_value", "threshold_60", and "threshold_40".

**SR_log_mod_sum**
(in *4.3_Stressor_Reponse_LOE.R*): This function summarizes the Stressor Response (Logistic) scores across analytes in each Module. The count of each score category (Supporting, Weakening, etc.) is added up per Module, and the overall "spatial_cooccurrence" score is determined based on those counts. Note that there is generally a 'one in all in' pattern. For example, if one or more of the analytes is "Supporting", then the whole module is scored "Supporting". Followed by "Weakening", "Indeterminate", "No Evidence", "No Test Data", in that respective priority. 

**LOE_sum_fun**
(in *5.0_LOE_summarize.R*): This function binds together the individual LOE summary datasets (from SCO_mod_sum, RCC_mod_sum, SR_log_mod_sum) to create a long-format table, with scores per sample per loe per module.

**LOE_samp_sum_fun**
(in *5.0_LOE_summarize.R*): This function summarizes scores across the Lines of Evidence, generating "Overall" scores per sampel per module. Gnerally, the script counts the occurrence of each score category. The "overall" score is determined based on those counts. Note that there is still generally a 'one in all in' pattern. For example, if one or more of the LOEs is "Supporting", then the whole module is scored "Supporting". Followed by "Weakening", "Indeterminate", "No Evidence", "No Test Data", in that respective priority. "No Evidence" and "No Test Data" are combined into the category "Cannot be Evaluated".

**Dat_invt_fun**
(in *6.0_Data_Inventory.R*): This function inventories each test site's stressor data and the available comparator stressor data corresponding to that site. The test site stressor data inventories are generated for per test site per module, and the number of non-NA data points are counted. The comparator site stressor data inventories are generated for per test site per analyte, and the number of non-NA data points are counted. These counts are used to assing monitoring/data collection priorities and recommendations.

### Additional Script Details:

*RSCA_Explore.R*: This script is largely equivalent to *0.2_Core.R*, but it is intended for running the RSCA analysis on new test sites and for exporting the various results locally. So far this workflow has been used for analysis applications other than San Gabriel, like to run the RSCA analysis on SMC sites for further exploration. It was created so that the core script could be left unchanged for dashboard use, unless edits to that workflow are needed.

*RSCA_Graphing.R*: This script includes plotting functions that generate plots of the various RSCA data products. While this script was largely written for the SMC RSCA presentations, it can be used for future data exploration etc. so it is saved here for reference. These functions were also referenced when creating the ggplot scripts that are used in the San Gabriel dashboard, but those plotting R scripts are separate/saved alongside Roberts other dashboard scripts.


