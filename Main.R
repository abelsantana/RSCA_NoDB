library(jsonlite)
library(tidyverse)
library(lubridate)
library(dplyr)
library(openxlsx)
library(DBI) 
library(RPostgreSQL) 


###  You need to run 0.1_Data_Prepping.R and the database connection string (con) before you run this script
###  Only once per session just to generate the data that 0.2_RSCA_Core needs

# Define Channel Engineering Type 
# Change this to "HB", "SB1" etc. to filter specific test sites 
# Leave it NA to Filter by the defined class per site in the .csv
Type <- NA 

# User-defined switch for graph generation
# Options: "none", "primary", "secondary", "both"
graph_mode <- "primary"  

# Define Output Directory for Processed Data
output_base_dir <- "~/My_R/RSCA_NoDB/output"

# Load test site data
import_sites <- read.csv("~/My_R/RSCA_NoDB/input/PSA_RSCA_SitesTEST.csv")

# If 'channel_engineering_class' exists and Type is specified, filter test sites
if ("channel_engineering_class" %in% colnames(import_sites) && !is.na(Type)) {
  import_sites <- import_sites %>% filter(channel_engineering_class == Type)
}

# Get the list of test sites after filtering
my_input_sites <- import_sites$masterid

# Define chunk size and split into chunks
chunk_size <- 25
site_chunks <- split(my_input_sites, ceiling(seq_along(my_input_sites) / chunk_size))

### If the process gets interrupted, you can restart from a specific chunk.
## Uncomment and set `chunk_start` to resume from that chunk index.
# Example: Start from chunk 3
# chunk_start <- 3  
# for (chunk_idx in seq(chunk_start, length(site_chunks))) {

# Process each chunk of 25 sites
for (chunk_idx in seq_along(site_chunks)) { 
  # Define the subset of sites for the current chunk
  my_input_test_sites <- site_chunks[[chunk_idx]]
  print(paste("Processing chunk", chunk_idx, "of", length(site_chunks), "of", length(my_input_sites), "total sites."))
  
  # Load the main script to build all the tables
  source('R/0.2_RSCA_Core.R')
  print('0.2_RSCA_Core.R finished for this chunk')
  
  # Annie recommended to run with the SR_log_dat_df
  inv <- Dat_invt_fun(SR_log_dat_df)
  
  # Function to make dataframe column names compatible with naming conventions
  clean_colnames <- function(df){
    names(df) <- str_replace_all(tolower(names(df)), '\\.', '_')
    return(df)
  }
  
  # Prepare dataframes and clean column names
  Test_Comp_df <- clean_colnames(Test_Comp_df) %>% mutate(objectid = as.integer(row.names(.)))
  LOE_mod_sum_df <- clean_colnames(LOE_mod_sum_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  LOE_sum_df <- clean_colnames(LOE_sum_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  SCO_dat_df <- clean_colnames(SCO_dat_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  SCO_LOE_df <- clean_colnames(SCO_LOE_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  RCC_dat_df <- clean_colnames(RCC_dat_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  RCC_LOE_df <- clean_colnames(RCC_LOE_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  SR_log_dat_df <- clean_colnames(SR_log_dat_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  SR_log_LOE_df <- clean_colnames(SR_log_LOE_df) %>% mutate(objectid = as.integer(row.names(.)), sampledate = as.Date(sampledate), test_csci_sampleid = paste(test_site, sampledate, collectionmethodcode, fieldreplicate, sep = '_'))
  inv <- clean_colnames(inv) %>% mutate(objectid = as.integer(row.names(.)))
  print('Dataframes are Cleaned')
  
  # Define data for report creation directly from the data frames
  monitoring_recs <- inv %>% select(test_site, analytename, n_comp_samples, n_test_samples, module, n_test_samples_module, 
                                    comp_analyte_priority, comp_analyte_priority_note, test_priority, test_priority_note)
  
  # Create summarized data frames directly from in-memory data
  module_summary <- LOE_mod_sum_df %>% 
    select(test_site, comid, latitude, longitude, sampledate, fieldreplicate, collectionmethodcode, 
           csci, qt10, qt50, qt90, csci_category_scape, csci_category, conductivity, eutrophication, 
           habitat, temperature, test_csci_sampleid) %>%
    arrange(test_site, sampledate)
  
  loa_summary <- LOE_sum_df %>% 
    select(module, test_site, sampledate, fieldreplicate, collectionmethodcode, loe, score, test_csci_sampleid) %>%
    arrange(test_site, sampledate)
  
  ref_con_comp <- RCC_LOE_df %>% 
    select(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode, analytename, 
           test_result, unit, p10, p25, p75, p90, n, rcc_score, test_csci_sampleid) %>%
    arrange(test_site, sampledate)
  
  stress_resp_sum <- SR_log_LOE_df %>% 
    select(module, direction, analytename, test_site, sampledate, fieldreplicate, collectionmethodcode, 
           test_result, unit, csci, prob_of_poor, se_of_prob, model_p_value, threshold_60, threshold_40, 
           sr_score, test_csci_sampleid) %>%
    arrange(test_site, sampledate)
  
  spatial_co_sum <- SCO_LOE_df %>% 
    select(module, direction, test_site, sampledate, fieldreplicate, collectionmethodcode, csci, analytename, 
           test_result, unit, p25, p50, p75, n, sco_score, test_csci_sampleid) %>%
    arrange(test_site, sampledate)
  print('Dataframes are Summerized')

  comp_site_data2 <- Test_Comp_df %>%
    rename(comparator_site = comp_site, comparator_lat = comp_lat, comparator_long = comp_long,
           comparator_county = comp_county, Bray_Curtis_Dissimilarity = bc_dist) %>%
    select(-objectid)

   print('Generating Excel Sheets')
  
  # Generate Excel sheets for each site in the current chunk
  for (i in my_input_test_sites) {
    module_summary.i <- module_summary %>% filter(test_site == i)
    loa_summary.i <- loa_summary %>% filter(test_site == i)
    ref_con_comp.i <- ref_con_comp %>% filter(test_site == i)
    stress_resp_sum.i <- stress_resp_sum %>% filter(test_site == i)
    spatial_co_sum.i <- spatial_co_sum %>% filter(test_site == i)
    comp_site_data2.i <- comp_site_data2 %>% filter(test_site == i)
    monitoring_recs.i <- monitoring_recs %>% filter(test_site == i)
    
    # Ensure the output directory exists
    output_dir <- file.path(output_base_dir, i)
    if (!dir.exists(output_dir)) {
      dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
    }
    
    # Write out sheets
    list_of_sheets <- list(
      "Module Summary" = module_summary.i,
      "LOA Summary" = loa_summary.i,
      "Reference Condition Comparison" = ref_con_comp.i,
      "Stressor Response Summary" = stress_resp_sum.i,
      "Spatial Co-Occurrence Summary" = spatial_co_sum.i,
      "RSCA Comparator Site Data" = comp_site_data2.i
    )
    
    # Summary Excel sheet
    summary_file_path <- paste0(output_dir, "/", i, "_Summary_Site_Data.xlsx")
    write.xlsx(list_of_sheets, file = summary_file_path)
    print(paste("Summary excel sheet created for site:", i)) # , "at", monitoring_file_path <- add if you want the file location printed.
    
    # Monitoring recommendations sheet
    monitoring_file_path <- paste0(output_dir, "/", i, "_Monitoring_Recommendations.xlsx")
    write.xlsx(monitoring_recs.i, file = monitoring_file_path)
    print(paste("Monitoring recommendations created for site:", i))
  }
  
  # Primary Graphing Functions
  if (graph_mode %in% c("primary", "both")) {
    print(paste("Starting Primary graphing"))
    source('R/RSCA_Graphing.R') 
    tryCatch({
      site_csci_module_plotter(i, output_dir)
      site_loe_plotter(i, output_dir)
      print(paste("Primary graphing completed"))
    }, error = function(e) {
      print(paste("Error in Primary graphing", e$message))
    })
  }
  
  # Secondary Graphing Functions
  if (graph_mode %in% c("secondary", "both")) {
    print(paste("Starting Secondary graphing"))
    source('R/plumber.R')  
    secondary_graphs_dir <- file.path(output_dir, "secondary_graphs")
    if (!dir.exists(secondary_graphs_dir)) {
      dir.create(secondary_graphs_dir, recursive = TRUE, showWarnings = FALSE)
    }
    tryCatch({
      for (sample in unique(LOE_sum_df$test_csci_sampleid[LOE_sum_df$test_site == i])) {
        for (mod in unique(LOE_sum_df$module[LOE_sum_df$test_site == i])) {
          for (analysis in unique(LOE_sum_df$loe[LOE_sum_df$test_site == i])) {
            last_plots(my_site = i, sampleid = sample, module = mod, loa = analysis)
          }
        }
      }
      print(paste("Secondary graphing completed"))
    }, error = function(e) {
      print(paste("Error in Secondary graphing", e$message))
    })
  }
 
  # Clear data frames to free up memory after each chunk
  data_frames_to_remove <- c(
    "scape", "my_input_test_sites", "CORE_fun_out", "Test_Comp_df", 
    "SCO_dat_df", "SCO_LOE_df", "RCC_dat_df", "RCC_LOE_df", "SR_log_dat_df", 
    "SR_log_LOE_df", "SCO_sum_df", "RCC_sum_df", "SR_log_sum_df", 
    "LOE_sum_df", "LOE_mod_sum_df", "Dat_invt_df", "monitoring_recs","monitoring_recs", 
    "module_summary", "loa_summary", "ref_con_comp", "stress_resp_sum", 
    "spatial_co_sum", "comp_site_data", "comp_site_data2", "module_summary.i", 
    "loa_summary.i", "ref_con_comp.i", "stress_resp_sum.i", "spatial_co_sum.i", 
    "comp_site_data2.i", "monitoring_recs.i", "list_of_sheets"
  )
  rm(list = data_frames_to_remove)
  gc()  # Perform garbage collection to release memory
  print(paste("Memory cleared after processing chunk", chunk_idx))
}
