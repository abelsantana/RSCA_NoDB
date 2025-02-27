# plumber.R
library(jsonlite)
library(ggplot2)
library(glue)
library(tidyverse)
library(lubridate)
library(ggpubr)

# Load the plotting functions
source('R/plots.R')

last_plots <- function(my_site, sampleid, module, loa){
  # Define output file path
  # Define output directory relative to the home directory
  output_dir <- file.path(output_base_dir, my_site, "Secondary_graphs")
  
  # Ensure the output directory exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # Define output file path
  fpath <- file.path(output_dir, glue('{sampleid}_{module}_{loa}.png'))
  
  # Ensure valid LOA values
  valid_loa <- c('spatial_co_occurrence','reference_condition','stressor_response')
  if (!(loa %in% valid_loa)) {
    return(list(paste('Line of Analysis should be ', glue::glue_collapse(valid_loa, ', '))))
  }
  
  # Spatial Temporal
  if (loa == 'spatial_co_occurrence') {
    sco_dat <- SCO_dat_df %>% filter(module == module, test_csci_sampleid == sampleid, !is.na(comp_result))
    sco_loe <- SCO_LOE_df %>% filter(module == module, test_csci_sampleid == sampleid, !is.na(test_result))
    
    plt <- spatial.co.plot(sco_dat, sco_loe, sampleid, module)
    pltwidth <- 1600
    pltheight <- 900
  }
  
  # Stressor Response
  if (loa == 'stressor_response') {
    sr_log_dat <- SR_log_dat_df %>% filter(module == module, test_csci_sampleid == sampleid, !is.na(comp_result))
    sr_log_loe <- SR_log_LOE_df %>% filter(module == module, test_csci_sampleid == sampleid, !is.na(test_result))
    
    plt <- stress.resp.plot(sr_log_dat, sr_log_loe, sampleid, module)
    pltwidth <- 1600
    pltheight <- 900
  }
  
  # Reference Condition Comparison
  if (loa == 'reference_condition') {
    rcc_dat <- RCC_dat_df %>% filter(module == module, test_csci_sampleid == sampleid, !is.na(comp_result))
    rcc_loe <- RCC_LOE_df %>% filter(module == module, test_csci_sampleid == sampleid, !is.na(test_result))
    
    plt <- ref.cond.plot(rcc_dat, rcc_loe, sampleid, module)
    pltwidth <- 1600
    pltheight <- 900
  }
  
  # Save the plot
  ggsave(fpath, plt, width = pltwidth, height = pltheight, units = 'px', device = "png")
}

# Replace the database query for `bold_df` with in-memory data
bold_df <- bind_rows(
  SCO_dat_df %>% select(test_site, test_csci_sampleid, module) %>% mutate(loa = "spatial_co_occurrence"),
  SCO_LOE_df %>% select(test_site, test_csci_sampleid, module) %>% mutate(loa = "spatial_co_occurrence"),
  SR_log_dat_df %>% select(test_site, test_csci_sampleid, module) %>% mutate(loa = "stressor_response"),
  SR_log_LOE_df %>% select(test_site, test_csci_sampleid, module) %>% mutate(loa = "stressor_response"),
  RCC_dat_df %>% select(test_site, test_csci_sampleid, module) %>% mutate(loa = "reference_condition"),
  RCC_LOE_df %>% select(test_site, test_csci_sampleid, module) %>% mutate(loa = "reference_condition")
) %>% distinct()

# Generate plots for each test site
mapply(last_plots, bold_df$test_site, bold_df$test_csci_sampleid, bold_df$module, bold_df$loa)
