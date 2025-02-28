# plumber.R
library(jsonlite)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(glue)

# load the plotting functions
# setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source('R/plots.R')

# Define the last_plots function that uses dataframes instead of database queries
last_plots <- function(test_site, sampleid, module, loa){
  # Define output file path
  # Define output directory relative to the home directory
  output_dir <- file.path(output_base_dir, test_site, "Secondary_graphs")
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Define output file path
  fpath <- file.path(output_dir, glue('{sampleid}_{module}_{loa}.png'))
  
  # Basic input sanitization
  sampleid = str_replace(sampleid, ';', '')
  module = str_replace(module, ';', '')
  
  # Define valid lines of analysis
  valid_loa <- c('spatial_co_occurrence', 'reference_condition', 'stressor_response')
  
  if (!(loa %in% valid_loa)) {
    return(list(paste('Line of Analysis should be ', glue::glue_collapse(valid_loa, ', '))))
  }
  
  # Create local variables to avoid conflict with function parameters
  module_param <- module
  sampleid_param <- sampleid
  
  # Spatial Co-occurrence
  if (loa == 'spatial_co_occurrence') {
    # Filter dataframes instead of querying database
    sco_dat <- SCO_dat_df %>% 
      filter(
        tolower(module) == tolower(module_param),
        tolower(test_csci_sampleid) == tolower(sampleid_param),
        !is.na(comp_result)
      )
    
    sco_loe <- SCO_LOE_df %>%
      filter(
        tolower(module) == tolower(module_param),
        tolower(test_csci_sampleid) == tolower(sampleid_param),
        !is.na(test_result)
      )
    
    plt <- spatial.co.plot(sco_dat, sco_loe, sampleid, module)
    pltwidth <- 1600
    pltheight <- 900
  }
  
  # Stressor Response
  if (loa == 'stressor_response') {
    # Filter dataframes instead of querying database
    sr_log_dat <- SR_log_dat_df %>%
      filter(
        tolower(module) == tolower(module_param),
        tolower(test_csci_sampleid) == tolower(sampleid_param),
        !is.na(comp_result)
      )
    
    sr_log_loe <- SR_log_LOE_df %>%
      filter(
        tolower(module) == tolower(module_param),
        tolower(test_csci_sampleid) == tolower(sampleid_param),
        !is.na(test_result)
      )
    
    plt <- stress.resp.plot(sr_log_dat, sr_log_loe, sampleid, module)
    pltwidth <- 1600
    pltheight <- 900
  }
  
  # Reference Condition Comparison
  if (loa == 'reference_condition') {
    # Filter dataframes instead of querying database
    rcc_dat <- RCC_dat_df %>%
      filter(
        tolower(module) == tolower(module_param),
        tolower(test_csci_sampleid) == tolower(sampleid_param),
        !is.na(comp_result)
      )
    
    rcc_loe <- RCC_LOE_df %>%
      filter(
        tolower(module) == tolower(module_param),
        tolower(test_csci_sampleid) == tolower(sampleid_param),
        !is.na(test_result)
      )
    
    plt <- ref.cond.plot(rcc_dat, rcc_loe, sampleid, module)
    pltwidth <- 1600
    pltheight <- 900
  }
  
  ggsave(fpath, plt, width = pltwidth, height = pltheight, units = 'px', device = "png")
  # base::readBin(fpath, 'raw', n = file.info(fpath)$size)
}

# Define mapping of table names to dataframes
df_map <- list(
  "temp_smc_sco_dat" = SCO_dat_df,
  "temp_smc_sco_loe" = SCO_LOE_df,
  "temp_smc_sr_log_dat" = SR_log_dat_df,
  "temp_smc_sr_log_loe" = SR_log_LOE_df,
  "temp_smc_rcc_dat" = RCC_dat_df,
  "temp_smc_rcc_loe" = RCC_LOE_df
)

# List of table names
tbl_x <- c("temp_smc_sco_dat", "temp_smc_sco_loe",
           "temp_smc_sr_log_dat", "temp_smc_sr_log_loe",
           "temp_smc_rcc_dat", "temp_smc_rcc_loe")

# Valid lines of analysis
valid_loa <- c('spatial_co_occurrence', 'reference_condition', 'stressor_response')

# Create the bold_df dataframe by combining distinct values from all dataframes
bold_df <- do.call(rbind,
                   lapply(tbl_x,
                          function(tbl){
                            # Get the appropriate dataframe based on table name
                            df <- df_map[[tbl]]
                            
                            # Extract distinct combinations
                            df %>%
                              select(test_site, test_csci_sampleid, module) %>%
                              distinct() %>%
                              mutate(loa = tbl) %>%
                              mutate(loa = as.character(loa)) %>%
                              mutate(loa = case_when(loa == "temp_smc_sco_dat" ~ "spatial_co_occurrence", 
                                                     loa == "temp_smc_sco_loe" ~ "spatial_co_occurrence", 
                                                     loa == "temp_smc_sr_log_dat" ~ "stressor_response", 
                                                     loa == "temp_smc_sr_log_loe" ~ "stressor_response", 
                                                     loa == "temp_smc_rcc_dat" ~ "reference_condition",
                                                     loa == "temp_smc_rcc_loe" ~ "reference_condition",
                                                     TRUE ~ loa))
                          }))

# Optional: Filter for specific sites if needed
# bold_df <- filter(bold_df, test_site %in% "SMC31343")

# Run the last_plots function with the data
mapply(last_plots, bold_df$test_site, bold_df$test_csci_sampleid, bold_df$module, bold_df$loa)
