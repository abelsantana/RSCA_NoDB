# Troubleshoot RSCA target sites without modifying source workflow files.

suppressPackageStartupMessages({
  library(tidyverse)
  library(tidyr)
  library(lubridate)
  library(vegan)
  library(purrr)
  library(broom)
})

options(dplyr.summarise.inform = FALSE)

target_file <- "troubleshooting/input/target_sites.csv"
output_dir <- "troubleshooting/output"
log_dir <- "troubleshooting/logs"

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

target_sites <- read.csv(target_file, stringsAsFactors = FALSE)$masterid
target_sites <- unique(trimws(target_sites))
target_year <- 2025
target_year_chr <- as.character(target_year)

Type <- NA
import_sites <- tibble(masterid = target_sites)

load("Base_Files/Base_Data.RData")
load("Base_Files/RSCA_Module_Direction_Assignments.RData")

# The source workflow expects a scape object from the database. This diagnostic
# runs from saved RData only; default CSCI thresholding uses 0.79, so these
# columns are present for compatibility and reported as NA.
scape <- station_base_df %>%
  distinct(comid) %>%
  mutate(
    comid = as.character(comid),
    qt10 = as.numeric(NA),
    qt50 = as.numeric(NA),
    qt90 = as.numeric(NA)
  )

source("R/1.0_Test_CSCI_check.R")
source("R/2.0_Comparator_Site_Selection_v2.R")
source("R/3.0_LOE_Data_Prep.R")
source("R/4.1_Spatial_CoOccurrence_LOE.R")
source("R/4.2_Ref_Condition_Comp_LOE.R")
source("R/4.3_Stressor_Response_LOE.R")
source("R/5.0_LOE_summarize.R")
source("R/6.0_Data_Inventory.R")

safe_run <- function(expr) {
  warnings <- character()
  value <- withCallingHandlers(
    tryCatch(
      list(ok = TRUE, value = force(expr), error = NA_character_),
      error = function(e) list(ok = FALSE, value = NULL, error = conditionMessage(e))
    ),
    warning = function(w) {
      warnings <<- c(warnings, conditionMessage(w))
      invokeRestart("muffleWarning")
    }
  )
  value$warnings <- paste(unique(warnings), collapse = " | ")
  value
}

df_rows <- function(x) {
  if (is.data.frame(x)) {
    nrow(x)
  } else if (is.null(x)) {
    0L
  } else {
    NA_integer_
  }
}

df_cols <- function(x) {
  if (is.data.frame(x)) ncol(x) else NA_integer_
}

distinct_count <- function(x, col) {
  if (!is.data.frame(x) || !(col %in% names(x))) return(NA_integer_)
  dplyr::n_distinct(x[[col]], na.rm = TRUE)
}

score_values <- function(x, col) {
  if (!is.data.frame(x) || !(col %in% names(x)) || nrow(x) == 0) return(NA_character_)
  paste(sort(unique(as.character(na.omit(x[[col]])))), collapse = "; ")
}

write_table <- function(df, path) {
  readr::write_csv(df, path, na = "")
}

stage_row <- function(site, stage, result, value = result$value, score_col = NA_character_) {
  tibble(
    masterid = site,
    stage = stage,
    status = if (isTRUE(result$ok)) "ok" else "error",
    rows = df_rows(value),
    cols = df_cols(value),
    samples = distinct_count(value, "sampledate"),
    analytes = distinct_count(value, "analytename"),
    modules = distinct_count(value, "module"),
    score_values = if (!is.na(score_col)) score_values(value, score_col) else NA_character_,
    error = result$error,
    warnings = result$warnings
  )
}

input_site_audit <- tibble(masterid = target_sites) %>%
  mutate(
    exact_in_csci = masterid %in% csci_base_df$masterid,
    exact_in_stressor = masterid %in% stressor_csci_base_df$masterid,
    exact_in_station = masterid %in% station_base_df$masterid,
    exact_in_oe = masterid %in% oe_base_df$masterid,
    csci_rows = map_int(masterid, ~sum(csci_base_df$masterid == .x)),
    stressor_rows = map_int(masterid, ~sum(stressor_csci_base_df$masterid == .x)),
    station_rows = map_int(masterid, ~sum(station_base_df$masterid == .x)),
    oe_rows = map_int(masterid, ~sum(oe_base_df$masterid == .x)),
    case_insensitive_csci_matches = map_chr(masterid, ~paste(sort(unique(csci_base_df$masterid[toupper(csci_base_df$masterid) == toupper(.x)])), collapse = "; ")),
    case_insensitive_stressor_matches = map_chr(masterid, ~paste(sort(unique(stressor_csci_base_df$masterid[toupper(stressor_csci_base_df$masterid) == toupper(.x)])), collapse = "; ")),
    case_insensitive_station_matches = map_chr(masterid, ~paste(sort(unique(station_base_df$masterid[toupper(station_base_df$masterid) == toupper(.x)])), collapse = "; ")),
    case_insensitive_oe_matches = map_chr(masterid, ~paste(sort(unique(oe_base_df$masterid[toupper(oe_base_df$masterid) == toupper(.x)])), collapse = "; "))
  )

write_table(input_site_audit, file.path(output_dir, "input_site_audit.csv"))

site_year_coverage <- tibble(masterid = target_sites) %>%
  mutate(
    csci_rows = map_int(masterid, ~sum(csci_base_df$masterid == .x)),
    csci_min_date = map_chr(masterid, ~{
      d <- as.Date(csci_base_df$sampledate[csci_base_df$masterid == .x])
      if (length(d) == 0) NA_character_ else as.character(min(d, na.rm = TRUE))
    }),
    csci_max_date = map_chr(masterid, ~{
      d <- as.Date(csci_base_df$sampledate[csci_base_df$masterid == .x])
      if (length(d) == 0) NA_character_ else as.character(max(d, na.rm = TRUE))
    }),
    csci_years = map_chr(masterid, ~{
      d <- as.Date(csci_base_df$sampledate[csci_base_df$masterid == .x])
      paste(sort(unique(format(d, "%Y"))), collapse = ";")
    }),
    csci_target_year_rows = map_int(masterid, ~{
      d <- as.Date(csci_base_df$sampledate[csci_base_df$masterid == .x])
      sum(format(d, "%Y") == target_year_chr, na.rm = TRUE)
    }),
    stressor_rows = map_int(masterid, ~sum(stressor_csci_base_df$masterid == .x)),
    stressor_min_date = map_chr(masterid, ~{
      d <- as.Date(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])
      if (length(d) == 0) NA_character_ else as.character(min(d, na.rm = TRUE))
    }),
    stressor_max_date = map_chr(masterid, ~{
      d <- as.Date(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])
      if (length(d) == 0) NA_character_ else as.character(max(d, na.rm = TRUE))
    }),
    stressor_years = map_chr(masterid, ~{
      d <- as.Date(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])
      paste(sort(unique(format(d, "%Y"))), collapse = ";")
    }),
    stressor_target_year_rows = map_int(masterid, ~{
      d <- as.Date(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])
      sum(format(d, "%Y") == target_year_chr, na.rm = TRUE)
    })
  )

write_table(site_year_coverage, file.path(output_dir, "site_year_coverage.csv"))

rdata_date_coverage <- tibble(
  data_frame = c("csci_base_df", "stressor_csci_base_df"),
  min_sampledate = c(
    as.character(min(as.Date(csci_base_df$sampledate), na.rm = TRUE)),
    as.character(min(as.Date(stressor_csci_base_df$sampledate), na.rm = TRUE))
  ),
  max_sampledate = c(
    as.character(max(as.Date(csci_base_df$sampledate), na.rm = TRUE)),
    as.character(max(as.Date(stressor_csci_base_df$sampledate), na.rm = TRUE))
  ),
  target_year = target_year,
  target_year_rows = c(
    sum(format(as.Date(csci_base_df$sampledate), "%Y") == target_year_chr, na.rm = TRUE),
    sum(format(as.Date(stressor_csci_base_df$sampledate), "%Y") == target_year_chr, na.rm = TRUE)
  )
)

write_table(rdata_date_coverage, file.path(output_dir, "rdata_date_coverage.csv"))

site_results <- list()
stage_rows <- list()
stage_details <- list()

for (site in target_sites) {
  message("Troubleshooting site: ", site)

  site_dir <- file.path(output_dir, site)
  dir.create(site_dir, recursive = TRUE, showWarnings = FALSE)

  stages <- list()
  details <- list()

  csci <- safe_run(CSCI_check_fun(TestID = site, CSCIthreshold = "default"))
  stages <- append(stages, list(stage_row(site, "01_CSCI_check", csci)))
  if (is.data.frame(csci$value)) {
    write_table(csci$value, file.path(site_dir, paste0(site, "_01_CSCI_check.csv")))
  }

  if (!isTRUE(csci$ok) || !is.data.frame(csci$value) || nrow(csci$value) == 0 || identical(csci$value$csci_check[[1]], "no")) {
    if (length(stages) > 0 && identical(csci$value$csci_check[[1]], "no")) {
      stages[[length(stages)]] <- stages[[length(stages)]] %>%
        mutate(warnings = "No CSCI data for TestID in saved Base_Data.RData")
    }
    empty_result <- list(
      masterid = site,
      stages = bind_rows(stages),
      outputs = list()
    )
    site_results[[site]] <- empty_result
    stage_rows[[site]] <- empty_result$stages
    next
  }

  comp <- safe_run(Comp_Select_Modified_fun(TestID = site, Min.CaptureProb = 0.5, Max.BC = 0.1, Type = Type))
  stages <- append(stages, list(stage_row(site, "02_Comparator_selection", comp)))
  if (is.data.frame(comp$value)) {
    write_table(comp$value, file.path(site_dir, paste0(site, "_02_Comparator_selection.csv")))
  }

  if (!isTRUE(comp$ok) || !is.data.frame(comp$value) || nrow(comp$value) == 0) {
    empty_result <- list(
      masterid = site,
      stages = bind_rows(stages),
      outputs = list()
    )
    site_results[[site]] <- empty_result
    stage_rows[[site]] <- empty_result$stages
    next
  }

  sco_dat <- safe_run(SCO_dat_fun(TestID = site, CompSites = comp$value))
  stages <- append(stages, list(stage_row(site, "03_SCO_dat", sco_dat)))
  if (is.data.frame(sco_dat$value)) write_table(sco_dat$value, file.path(site_dir, paste0(site, "_03_SCO_dat.csv")))

  sco_loe <- safe_run(SCO_fun(SCOData = sco_dat$value, CSCIcheckoutput = csci$value))
  stages <- append(stages, list(stage_row(site, "04_SCO_LOE", sco_loe, score_col = "sco_score")))
  if (is.data.frame(sco_loe$value)) write_table(sco_loe$value, file.path(site_dir, paste0(site, "_04_SCO_LOE.csv")))

  sco_sum <- safe_run(SCO_sum_mod(SCOoutput = sco_loe$value, CSCIcheckoutput = csci$value))
  stages <- append(stages, list(stage_row(site, "05_SCO_module_summary", sco_sum, score_col = "spatial_cooccurrence")))
  if (is.data.frame(sco_sum$value)) write_table(sco_sum$value, file.path(site_dir, paste0(site, "_05_SCO_module_summary.csv")))

  rcc_dat <- safe_run(RCC_dat_fun(TestID = site, CompSites = comp$value))
  stages <- append(stages, list(stage_row(site, "06_RCC_dat", rcc_dat)))
  if (is.data.frame(rcc_dat$value)) write_table(rcc_dat$value, file.path(site_dir, paste0(site, "_06_RCC_dat.csv")))

  rcc_loe <- safe_run(RCC_fun(RCCData = rcc_dat$value, CSCIcheckoutput = csci$value))
  stages <- append(stages, list(stage_row(site, "07_RCC_LOE", rcc_loe, score_col = "rcc_score")))
  if (is.data.frame(rcc_loe$value)) write_table(rcc_loe$value, file.path(site_dir, paste0(site, "_07_RCC_LOE.csv")))

  rcc_sum <- safe_run(RCC_sum_mod(RCCoutput = rcc_loe$value, CSCIcheckoutput = csci$value))
  stages <- append(stages, list(stage_row(site, "08_RCC_module_summary", rcc_sum, score_col = "reference_condition")))
  if (is.data.frame(rcc_sum$value)) write_table(rcc_sum$value, file.path(site_dir, paste0(site, "_08_RCC_module_summary.csv")))

  sr_dat <- safe_run(SR_log_dat_fun(TestID = site, CompSites = comp$value))
  stages <- append(stages, list(stage_row(site, "09_SR_log_dat", sr_dat)))
  if (is.data.frame(sr_dat$value)) write_table(sr_dat$value, file.path(site_dir, paste0(site, "_09_SR_log_dat.csv")))

  sr_loe <- safe_run(SR_log_fun(SRData = sr_dat$value, CSCIcheckoutput = csci$value))
  stages <- append(stages, list(stage_row(site, "10_SR_log_LOE", sr_loe, score_col = "sr_score")))
  if (is.data.frame(sr_loe$value)) write_table(sr_loe$value, file.path(site_dir, paste0(site, "_10_SR_log_LOE.csv")))

  sr_sum <- safe_run(SR_log_mod_sum(SRlogoutput = sr_loe$value, CSCIcheckoutput = csci$value))
  stages <- append(stages, list(stage_row(site, "11_SR_log_module_summary", sr_sum, score_col = "stressor_response")))
  if (is.data.frame(sr_sum$value)) write_table(sr_sum$value, file.path(site_dir, paste0(site, "_11_SR_log_module_summary.csv")))

  loe_sum <- safe_run(LOE_sum_fun(SCO_sum = sco_sum$value, RCC_sum = rcc_sum$value, SR_log_sum = sr_sum$value))
  stages <- append(stages, list(stage_row(site, "12_LOE_summary", loe_sum, score_col = "score")))

  loe_mod_sum <- safe_run(LOE_samp_sum_fun(SCO_sum = sco_sum$value, RCC_sum = rcc_sum$value, SR_log_sum = sr_sum$value))
  stages <- append(stages, list(stage_row(site, "13_Module_summary", loe_mod_sum)))

  inv <- safe_run(Dat_invt_fun(LOEInputData = sr_dat$value))
  stages <- append(stages, list(stage_row(site, "14_Data_inventory", inv)))

  outputs <- list(
    module_summary = loe_mod_sum$value,
    loa_summary = loe_sum$value,
    reference_condition_comparison = rcc_loe$value,
    stressor_response_summary = sr_loe$value,
    spatial_cooccurrence_summary = sco_loe$value,
    rsca_comparator_site_data = comp$value,
    monitoring_recommendations = inv$value
  )

  for (nm in names(outputs)) {
    if (is.data.frame(outputs[[nm]])) {
      write_table(outputs[[nm]], file.path(site_dir, paste0(site, "_", nm, ".csv")))
      stages <- append(stages, list(stage_row(site, paste0("output_", nm), list(ok = TRUE, value = outputs[[nm]], error = NA_character_, warnings = ""))))
    } else {
      stages <- append(stages, list(stage_row(site, paste0("output_", nm), list(ok = FALSE, value = NULL, error = "output object was not a data frame", warnings = ""))))
    }
  }

  site_results[[site]] <- list(
    masterid = site,
    stages = bind_rows(stages),
    outputs = outputs
  )
  stage_rows[[site]] <- site_results[[site]]$stages
}

workflow_stage_summary <- bind_rows(stage_rows)
write_table(workflow_stage_summary, file.path(output_dir, "workflow_stage_summary.csv"))

combined_outputs <- list()
output_names <- c(
  "module_summary",
  "loa_summary",
  "reference_condition_comparison",
  "stressor_response_summary",
  "spatial_cooccurrence_summary",
  "rsca_comparator_site_data",
  "monitoring_recommendations"
)

for (nm in output_names) {
  combined <- map(site_results, ~.x$outputs[[nm]]) %>%
    compact() %>%
    bind_rows()
  combined_outputs[[nm]] <- combined
  write_table(combined, file.path(output_dir, paste0("combined_", nm, ".csv")))
}

first_problem_stage <- workflow_stage_summary %>%
  filter(
    status != "ok" |
      rows == 0 |
      is.na(rows) |
      (stage == "01_CSCI_check" & cols == 1)
  ) %>%
  group_by(masterid) %>%
  arrange(match(stage, unique(workflow_stage_summary$stage)), .by_group = TRUE) %>%
  slice(1) %>%
  ungroup() %>%
  transmute(
    masterid,
    first_problem_stage = stage,
    status,
    rows,
    error,
    warnings
  )

site_outcome_summary <- tibble(masterid = target_sites) %>%
  left_join(input_site_audit, by = "masterid") %>%
  left_join(site_year_coverage, by = "masterid", suffix = c("", "_year_coverage")) %>%
  left_join(first_problem_stage, by = "masterid") %>%
  mutate(
    final_module_summary_rows = map_int(masterid, ~{
      x <- site_results[[.x]]$outputs$module_summary
      if (is.data.frame(x)) nrow(x) else 0L
    }),
    final_loa_summary_rows = map_int(masterid, ~{
      x <- site_results[[.x]]$outputs$loa_summary
      if (is.data.frame(x)) nrow(x) else 0L
    }),
    final_monitoring_rows = map_int(masterid, ~{
      x <- site_results[[.x]]$outputs$monitoring_recommendations
      if (is.data.frame(x)) nrow(x) else 0L
    }),
    troubleshooting_status = case_when(
      final_module_summary_rows > 0 & final_loa_summary_rows > 0 ~ "produced_tabular_output",
      first_problem_stage == "01_CSCI_check" & rows == 1 & status == "ok" ~ "no_csci_data_in_saved_rdata",
      is.na(first_problem_stage) ~ "no_problem_stage_detected_but_no_final_output",
      TRUE ~ "dropped_or_failed_before_final_output"
    )
  ) %>%
  select(
    masterid,
    troubleshooting_status,
    first_problem_stage,
    status,
    rows,
    error,
    warnings,
    final_module_summary_rows,
    final_loa_summary_rows,
    final_monitoring_rows,
    everything()
  )

write_table(site_outcome_summary, file.path(output_dir, "site_outcome_summary.csv"))

session_info <- capture.output(sessionInfo())
writeLines(session_info, file.path(log_dir, "session_info.txt"))

message("Troubleshooting complete. Outputs written to: ", normalizePath(output_dir))
