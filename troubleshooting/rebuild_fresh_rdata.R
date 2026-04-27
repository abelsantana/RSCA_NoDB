# Rebuild fresh RSCA base tables in troubleshooting/ without overwriting repo data.

suppressPackageStartupMessages({
  library(DBI)
  library(RPostgreSQL)
  library(tidyverse)
})

args <- commandArgs(trailingOnly = FALSE)
file_arg <- args[grepl("^--file=", args)]
script_path <- if (length(file_arg) > 0) sub("^--file=", "", file_arg[[1]]) else "troubleshooting/rebuild_fresh_rdata.R"
repo_root <- normalizePath(file.path(dirname(script_path), ".."), winslash = "/", mustWork = TRUE)
fresh_root <- file.path(repo_root, "troubleshooting", "fresh_rdata")
fresh_base_dir <- file.path(fresh_root, "Base_Files")
output_dir <- file.path(repo_root, "troubleshooting", "output")
log_dir <- file.path(repo_root, "troubleshooting", "logs")
target_file <- file.path(repo_root, "troubleshooting", "input", "target_sites.csv")

dir.create(fresh_base_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

file.copy(
  from = file.path(repo_root, "Base_Files", "RSCA_Module_Direction_Assignments.RData"),
  to = file.path(fresh_base_dir, "RSCA_Module_Direction_Assignments.RData"),
  overwrite = TRUE
)

source(file.path(repo_root, "DB_Connection.txt"))
source(file.path(repo_root, "R", "0.1_Data_Prepping.R"))

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(fresh_root)

message("Rebuilding fresh Base_Data.RData under: ", fresh_base_dir)
prep_smc_data(con)

fresh_rdata_path <- file.path(fresh_base_dir, "Base_Data.RData")
load(fresh_rdata_path)

target_sites <- read.csv(target_file, stringsAsFactors = FALSE)$masterid |>
  trimws() |>
  unique()

year_values <- function(dates) {
  dates <- as.Date(dates)
  paste(sort(unique(format(dates, "%Y"))), collapse = ";")
}

min_date <- function(dates) {
  dates <- as.Date(dates)
  if (length(dates) == 0 || all(is.na(dates))) return(NA_character_)
  as.character(min(dates, na.rm = TRUE))
}

max_date <- function(dates) {
  dates <- as.Date(dates)
  if (length(dates) == 0 || all(is.na(dates))) return(NA_character_)
  as.character(max(dates, na.rm = TRUE))
}

count_2025 <- function(dates) {
  dates <- as.Date(dates)
  sum(format(dates, "%Y") == "2025", na.rm = TRUE)
}

fresh_site_2025_audit <- tibble(masterid = target_sites) %>%
  mutate(
    exact_in_csci = masterid %in% csci_base_df$masterid,
    exact_in_stressor = masterid %in% stressor_csci_base_df$masterid,
    exact_in_station = masterid %in% station_base_df$masterid,
    exact_in_oe = masterid %in% oe_base_df$masterid,
    csci_rows = map_int(masterid, ~sum(csci_base_df$masterid == .x)),
    csci_2025_rows = map_int(masterid, ~count_2025(csci_base_df$sampledate[csci_base_df$masterid == .x])),
    csci_min_date = map_chr(masterid, ~min_date(csci_base_df$sampledate[csci_base_df$masterid == .x])),
    csci_max_date = map_chr(masterid, ~max_date(csci_base_df$sampledate[csci_base_df$masterid == .x])),
    csci_years = map_chr(masterid, ~year_values(csci_base_df$sampledate[csci_base_df$masterid == .x])),
    stressor_rows = map_int(masterid, ~sum(stressor_csci_base_df$masterid == .x)),
    stressor_2025_rows = map_int(masterid, ~count_2025(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])),
    stressor_min_date = map_chr(masterid, ~min_date(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])),
    stressor_max_date = map_chr(masterid, ~max_date(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])),
    stressor_years = map_chr(masterid, ~year_values(stressor_csci_base_df$sampledate[stressor_csci_base_df$masterid == .x])),
    station_rows = map_int(masterid, ~sum(station_base_df$masterid == .x)),
    oe_rows = map_int(masterid, ~sum(oe_base_df$masterid == .x)),
    has_2025_primary_tables = csci_2025_rows > 0 | stressor_2025_rows > 0
  )

readr::write_csv(
  fresh_site_2025_audit,
  file.path(output_dir, "fresh_rdata_site_2025_audit.csv"),
  na = ""
)

fresh_rdata_coverage <- tibble(
  data_frame = c("csci_base_df", "stressor_csci_base_df"),
  rows = c(nrow(csci_base_df), nrow(stressor_csci_base_df)),
  min_sampledate = c(min_date(csci_base_df$sampledate), min_date(stressor_csci_base_df$sampledate)),
  max_sampledate = c(max_date(csci_base_df$sampledate), max_date(stressor_csci_base_df$sampledate)),
  rows_2025 = c(count_2025(csci_base_df$sampledate), count_2025(stressor_csci_base_df$sampledate))
)

readr::write_csv(
  fresh_rdata_coverage,
  file.path(output_dir, "fresh_rdata_coverage.csv"),
  na = ""
)

writeLines(capture.output(sessionInfo()), file.path(log_dir, "rebuild_fresh_rdata_session_info.txt"))

message("Fresh RData rebuild complete: ", normalizePath(fresh_rdata_path, winslash = "/"))
message("Fresh 2025 site audit written to: ", normalizePath(file.path(output_dir, "fresh_rdata_site_2025_audit.csv"), winslash = "/"))
