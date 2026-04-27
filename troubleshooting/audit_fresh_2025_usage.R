# Summarize whether fresh 2025 records reach each troubleshooting output stage.

suppressPackageStartupMessages({
  library(tidyverse)
})

workflow_dir <- "troubleshooting/output/fresh_workflow"
output_dir <- workflow_dir
target_sites <- read.csv("troubleshooting/input/target_sites.csv", stringsAsFactors = FALSE)$masterid %>%
  trimws() %>%
  unique()

read_stage <- function(site, suffix) {
  path <- file.path(workflow_dir, site, paste0(site, suffix))
  if (!file.exists(path)) return(NULL)
  readr::read_csv(path, show_col_types = FALSE, guess_max = 100000) %>%
    mutate(.source_file = basename(path))
}

score_column <- function(df) {
  candidates <- c("sco_score", "rcc_score", "sr_score", "spatial_cooccurrence", "reference_condition", "stressor_response", "score")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

stage_specs <- tribble(
  ~stage, ~suffix,
  "01_CSCI_check", "_01_CSCI_check.csv",
  "03_SCO_dat", "_03_SCO_dat.csv",
  "04_SCO_LOE", "_04_SCO_LOE.csv",
  "05_SCO_module_summary", "_05_SCO_module_summary.csv",
  "06_RCC_dat", "_06_RCC_dat.csv",
  "07_RCC_LOE", "_07_RCC_LOE.csv",
  "08_RCC_module_summary", "_08_RCC_module_summary.csv",
  "09_SR_log_dat", "_09_SR_log_dat.csv",
  "10_SR_log_LOE", "_10_SR_log_LOE.csv",
  "11_SR_log_module_summary", "_11_SR_log_module_summary.csv",
  "final_module_summary", "_module_summary.csv",
  "final_loa_summary", "_loa_summary.csv"
)

stage_usage <- purrr::map_dfr(target_sites, function(site) {
  purrr::pmap_dfr(stage_specs, function(stage, suffix) {
    df <- read_stage(site, suffix)
    if (is.null(df)) {
      return(tibble(masterid = site, stage = stage, rows = 0L, rows_2025 = 0L))
    }

    date_col <- intersect(c("sampledate"), names(df))
    rows_2025 <- if (length(date_col) == 0) {
      NA_integer_
    } else {
      sum(format(as.Date(df[[date_col[[1]]]]), "%Y") == "2025", na.rm = TRUE)
    }

    nonblank_test_result_2025 <- if ("test_result" %in% names(df) && length(date_col) > 0) {
      sum(format(as.Date(df[[date_col[[1]]]]), "%Y") == "2025" & !is.na(df$test_result) & trimws(as.character(df$test_result)) != "", na.rm = TRUE)
    } else {
      NA_integer_
    }

    sc <- score_column(df)
    scores_2025 <- if (!is.na(sc) && length(date_col) > 0) {
      df %>%
        filter(format(as.Date(.data[[date_col[[1]]]]), "%Y") == "2025") %>%
        pull(all_of(sc)) %>%
        as.character() %>%
        na.omit() %>%
        unique() %>%
        sort() %>%
        paste(collapse = "; ")
    } else {
      NA_character_
    }

    tibble(
      masterid = site,
      stage = stage,
      rows = nrow(df),
      rows_2025 = rows_2025,
      nonblank_test_result_2025 = nonblank_test_result_2025,
      scores_2025 = scores_2025
    )
  })
})

analyte_usage <- purrr::map_dfr(target_sites, function(site) {
  loe_specs <- tribble(
    ~loe, ~suffix, ~score_col,
    "Spatial Co-Occurrence", "_04_SCO_LOE.csv", "sco_score",
    "Reference Condition", "_07_RCC_LOE.csv", "rcc_score",
    "Stressor Response", "_10_SR_log_LOE.csv", "sr_score"
  )

  purrr::pmap_dfr(loe_specs, function(loe, suffix, score_col) {
    df <- read_stage(site, suffix)
    if (is.null(df) || !all(c("sampledate", "analytename", "test_result", score_col) %in% names(df))) {
      return(tibble())
    }

    df %>%
      filter(format(as.Date(sampledate), "%Y") == "2025") %>%
      group_by(masterid = test_site, loe, analytename) %>%
      summarise(
        rows_2025 = n(),
        nonblank_test_result_2025 = sum(!is.na(test_result) & trimws(as.character(test_result)) != ""),
        scores_2025 = paste(sort(unique(as.character(.data[[score_col]]))), collapse = "; "),
        .groups = "drop"
      )
  })
})

passing_override <- purrr::map_dfr(target_sites, function(site) {
  df <- read_stage(site, "_01_CSCI_check.csv")
  if (is.null(df)) return(tibble())

  df %>%
    filter(format(as.Date(sampledate), "%Y") == "2025") %>%
    transmute(
      masterid,
      sampledate = as.character(sampledate),
      collectionmethodcode,
      fieldreplicate,
      csci,
      check_value,
      csci_gt_check_value = csci > check_value,
      expected_loe_behavior = if_else(csci_gt_check_value, "LOE scores overridden to Passing CSCI", "LOE scores evaluate stressor evidence")
    )
})

readr::write_csv(stage_usage, file.path(output_dir, "fresh_workflow_2025_stage_usage.csv"), na = "")
readr::write_csv(analyte_usage, file.path(output_dir, "fresh_workflow_2025_analyte_usage.csv"), na = "")
readr::write_csv(passing_override, file.path(output_dir, "fresh_workflow_2025_passing_override.csv"), na = "")

message("Fresh 2025 usage audit written to: ", normalizePath(output_dir))
