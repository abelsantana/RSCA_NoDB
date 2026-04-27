# Audit target sites in the SMC database without refreshing local RData.
#
# Connection options:
# 1. Source this file after creating an existing DBI connection object named `con`.
# 2. Or set SMC_DB_HOST, SMC_DB_USER, SMC_DB_PASSWORD, SMC_DB_NAME, and
#    optional SMC_DB_PORT.

suppressPackageStartupMessages({
  library(DBI)
  library(dbplyr)
  library(RPostgreSQL)
  library(tidyverse)
})

target_file <- "troubleshooting/input/target_sites.csv"
output_dir <- "troubleshooting/output/database_audit"
log_dir <- "troubleshooting/logs"
target_year <- 2025
target_year_chr <- as.character(target_year)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)

target_sites <- read.csv(target_file, stringsAsFactors = FALSE)$masterid
target_sites <- unique(trimws(target_sites))

write_table <- function(df, path) {
  readr::write_csv(df, path, na = "")
}

make_connection <- function() {
  if (exists("con", envir = .GlobalEnv)) {
    return(list(con = get("con", envir = .GlobalEnv), owns_connection = FALSE))
  }

  required_env <- c("SMC_DB_HOST", "SMC_DB_USER", "SMC_DB_PASSWORD", "SMC_DB_NAME")
  missing_env <- required_env[Sys.getenv(required_env) == ""]
  if (length(missing_env) > 0) {
    stop(
      "No active `con` object found and these env vars are missing: ",
      paste(missing_env, collapse = ", "),
      call. = FALSE
    )
  }

  new_con <- DBI::dbConnect(
    RPostgreSQL::PostgreSQL(),
    host = Sys.getenv("SMC_DB_HOST"),
    user = Sys.getenv("SMC_DB_USER"),
    password = Sys.getenv("SMC_DB_PASSWORD"),
    dbname = Sys.getenv("SMC_DB_NAME"),
    port = as.integer(ifelse(Sys.getenv("SMC_DB_PORT") == "", "5432", Sys.getenv("SMC_DB_PORT")))
  )
  list(con = new_con, owns_connection = TRUE)
}

quote_sql_values <- function(values) {
  paste(DBI::dbQuoteString(con, values), collapse = ", ")
}

run_query <- function(name, sql_text) {
  message("Querying ", name)
  result <- tryCatch(
    {
      df <- DBI::dbGetQuery(con, sql_text) %>% as_tibble()
      write_table(df, file.path(output_dir, paste0(name, ".csv")))
      tibble(query_name = name, status = "ok", rows = nrow(df), error = NA_character_)
    },
    error = function(e) {
      tibble(query_name = name, status = "error", rows = NA_integer_, error = conditionMessage(e))
    }
  )
  result
}

connection_info <- make_connection()
con <- connection_info$con
on.exit({
  if (isTRUE(connection_info$owns_connection) && exists("con")) {
    DBI::dbDisconnect(con)
  }
}, add = TRUE)

site_list_sql <- quote_sql_values(target_sites)

queries <- list(
  lu_stations_exact = paste0(
    "SELECT stationid, masterid, stationname, latitude, longitude, huc, county, psa_lu, comid ",
    "FROM sde.lu_stations ",
    "WHERE masterid IN (", site_list_sql, ") OR stationid IN (", site_list_sql, ") ",
    "ORDER BY masterid, stationid"
  ),

  lu_stations_case_insensitive = paste0(
    "SELECT stationid, masterid, stationname, latitude, longitude, huc, county, psa_lu, comid ",
    "FROM sde.lu_stations ",
    "WHERE upper(masterid) IN (", paste(DBI::dbQuoteString(con, toupper(target_sites)), collapse = ", "), ") ",
    "OR upper(stationid) IN (", paste(DBI::dbQuoteString(con, toupper(target_sites)), collapse = ", "), ") ",
    "ORDER BY masterid, stationid"
  ),

  csci_by_masterid = paste0(
    "SELECT s.masterid, c.stationcode, c.sampleid, c.sampledate, ",
    "c.collectionmethodcode, c.fieldreplicate, c.count, c.csci, c.csci_percentile ",
    "FROM sde.analysis_csci_core c ",
    "JOIN sde.lu_stations s ON c.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "ORDER BY s.masterid, c.sampledate, c.stationcode"
  ),

  csci_2025_by_masterid = paste0(
    "SELECT s.masterid, c.stationcode, c.sampleid, c.sampledate, ",
    "c.collectionmethodcode, c.fieldreplicate, c.count, c.csci, c.csci_percentile ",
    "FROM sde.analysis_csci_core c ",
    "JOIN sde.lu_stations s ON c.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "AND c.sampledate >= DATE '", target_year_chr, "-01-01' ",
    "AND c.sampledate < DATE '", target_year + 1, "-01-01' ",
    "ORDER BY s.masterid, c.sampledate, c.stationcode"
  ),

  csci_by_stationcode = paste0(
    "SELECT c.stationcode, c.sampleid, c.sampledate, c.collectionmethodcode, ",
    "c.fieldreplicate, c.count, c.csci, c.csci_percentile ",
    "FROM sde.analysis_csci_core c ",
    "WHERE c.stationcode IN (", site_list_sql, ") ",
    "ORDER BY c.stationcode, c.sampledate"
  ),

  oe_by_masterid = paste0(
    "SELECT s.masterid, o.stationcode, COUNT(*) AS oe_rows, ",
    "COUNT(DISTINCT o.otu) AS otu_count, ",
    "MIN(o.captureprob) AS min_captureprob, MAX(o.captureprob) AS max_captureprob ",
    "FROM sde.analysis_csci_suppl1_oe o ",
    "JOIN sde.lu_stations s ON o.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "GROUP BY s.masterid, o.stationcode ",
    "ORDER BY s.masterid, o.stationcode"
  ),

  unified_chemistry_relevant_by_masterid = paste0(
    "SELECT s.masterid, u.stationcode, u.sampledate, u.matrixname, u.fieldreplicate, ",
    "u.labreplicate, u.methodname, u.sampletypecode, u.analytename, u.fractionname, ",
    "u.unit, u.result, u.resqualcode, u.mdl, u.rl, u.qacode, u.record_origin ",
    "FROM sde.unified_chemistry u ",
    "JOIN sde.lu_stations s ON u.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "AND u.analytename IN (",
    "'Chlorophyll a, Total', 'Chlorophyll a, Particulate', 'Chlorophyll a', 'Chlorophyll a, Not Recorded', ",
    "'Ash Free Dry Mass', 'Ash Free Dry Mass, Total', 'AFDM_Algae, Particulate', 'AFDM_Algae, Total', ",
    "'Chloride', 'Chloride, Dissolved', 'Chloride, Not Recorded', 'Chloride, Total', ",
    "'Sulfate', 'Sulfate, Dissolved', 'Sulfate, Not Recorded', 'Sulfate, Total', ",
    "'Dissolved Solids', 'Total Dissolved Solids, Dissolved', 'Total Dissolved Solids, Total', ",
    "'Total Dissolved Solids', 'Total Dissolved Solids, Fixed', ",
    "'SpecificConductivity, Not Recorded', 'SpecificConductivity', 'SpecificConductivity, Dissolved', ",
    "'ElectricalConductivity, Total', 'SpecificConductivity, Total', 'ElectricalConductivity', ",
    "'Oxygen, Dissolved, Not Recorded', 'Oxygen, Dissolved, Dissolved', 'Oxygen, Dissolved, Total', ",
    "'Temperature', 'Temperature, Total') ",
    "ORDER BY s.masterid, u.sampledate, u.analytename"
  ),

  unified_chemistry_relevant_2025_by_masterid = paste0(
    "SELECT s.masterid, u.stationcode, u.sampledate, u.matrixname, u.fieldreplicate, ",
    "u.labreplicate, u.methodname, u.sampletypecode, u.analytename, u.fractionname, ",
    "u.unit, u.result, u.resqualcode, u.mdl, u.rl, u.qacode, u.record_origin ",
    "FROM sde.unified_chemistry u ",
    "JOIN sde.lu_stations s ON u.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "AND u.sampledate >= DATE '", target_year_chr, "-01-01' ",
    "AND u.sampledate < DATE '", target_year + 1, "-01-01' ",
    "AND u.analytename IN (",
    "'Chlorophyll a, Total', 'Chlorophyll a, Particulate', 'Chlorophyll a', 'Chlorophyll a, Not Recorded', ",
    "'Ash Free Dry Mass', 'Ash Free Dry Mass, Total', 'AFDM_Algae, Particulate', 'AFDM_Algae, Total', ",
    "'Chloride', 'Chloride, Dissolved', 'Chloride, Not Recorded', 'Chloride, Total', ",
    "'Sulfate', 'Sulfate, Dissolved', 'Sulfate, Not Recorded', 'Sulfate, Total', ",
    "'Dissolved Solids', 'Total Dissolved Solids, Dissolved', 'Total Dissolved Solids, Total', ",
    "'Total Dissolved Solids', 'Total Dissolved Solids, Fixed', ",
    "'SpecificConductivity, Not Recorded', 'SpecificConductivity', 'SpecificConductivity, Dissolved', ",
    "'ElectricalConductivity, Total', 'SpecificConductivity, Total', 'ElectricalConductivity', ",
    "'Oxygen, Dissolved, Not Recorded', 'Oxygen, Dissolved, Dissolved', 'Oxygen, Dissolved, Total', ",
    "'Temperature', 'Temperature, Total') ",
    "ORDER BY s.masterid, u.sampledate, u.analytename"
  ),

  nutrients_by_masterid = paste0(
    "SELECT n.* ",
    "FROM sde.analysis_chem_nutrients_0 n ",
    "WHERE n.masterid IN (", site_list_sql, ") ",
    "ORDER BY n.masterid, n.sampledate"
  ),

  nutrients_2025_by_masterid = paste0(
    "SELECT n.* ",
    "FROM sde.analysis_chem_nutrients_0 n ",
    "WHERE n.masterid IN (", site_list_sql, ") ",
    "AND n.sampledate >= DATE '", target_year_chr, "-01-01' ",
    "AND n.sampledate < DATE '", target_year + 1, "-01-01' ",
    "ORDER BY n.masterid, n.sampledate"
  ),

  unified_phab_wq_by_masterid = paste0(
    "SELECT s.masterid, p.stationcode, p.sampledate, p.matrixname, p.replicate, ",
    "p.methodname, p.collectiondepth, p.analytename, p.fractionname, p.unitname, ",
    "p.result, p.resqualcode, p.qacode, p.sampleagencycode ",
    "FROM sde.unified_phab p ",
    "JOIN sde.lu_stations s ON p.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "AND p.analytename IN ('SpecificConductivity', 'Oxygen, Dissolved', 'Temperature') ",
    "ORDER BY s.masterid, p.sampledate, p.analytename"
  ),

  unified_phab_wq_2025_by_masterid = paste0(
    "SELECT s.masterid, p.stationcode, p.sampledate, p.matrixname, p.replicate, ",
    "p.methodname, p.collectiondepth, p.analytename, p.fractionname, p.unitname, ",
    "p.result, p.resqualcode, p.qacode, p.sampleagencycode ",
    "FROM sde.unified_phab p ",
    "JOIN sde.lu_stations s ON p.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "AND p.sampledate >= DATE '", target_year_chr, "-01-01' ",
    "AND p.sampledate < DATE '", target_year + 1, "-01-01' ",
    "AND p.analytename IN ('SpecificConductivity', 'Oxygen, Dissolved', 'Temperature') ",
    "ORDER BY s.masterid, p.sampledate, p.analytename"
  ),

  phab_metrics_by_masterid = paste0(
    "SELECT s.masterid, p.stationcode, p.sampledate, p.variable, p.result, p.unit ",
    "FROM analysis_phabmetrics p ",
    "JOIN sde.lu_stations s ON p.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "AND p.variable IN ('XCMG', 'PCT_SAFN', 'H_SubNat', 'H_AqHab', 'Ev_FlowHab') ",
    "ORDER BY s.masterid, p.sampledate, p.variable"
  ),

  phab_metrics_2025_by_masterid = paste0(
    "SELECT s.masterid, p.stationcode, p.sampledate, p.variable, p.result, p.unit ",
    "FROM analysis_phabmetrics p ",
    "JOIN sde.lu_stations s ON p.stationcode = s.stationid ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "AND p.sampledate >= DATE '", target_year_chr, "-01-01' ",
    "AND p.sampledate < DATE '", target_year + 1, "-01-01' ",
    "AND p.variable IN ('XCMG', 'PCT_SAFN', 'H_SubNat', 'H_AqHab', 'Ev_FlowHab') ",
    "ORDER BY s.masterid, p.sampledate, p.variable"
  ),

  scape_by_site_comid = paste0(
    "SELECT s.masterid, s.stationid, s.comid, sc.* ",
    "FROM sde.lu_stations s ",
    "LEFT JOIN sde.scape_strm_constraints sc ON CAST(s.comid AS text) = CAST(sc.comid AS text) ",
    "WHERE s.masterid IN (", site_list_sql, ") ",
    "ORDER BY s.masterid, s.stationid"
  )
)

query_summary <- purrr::imap_dfr(queries, ~run_query(.y, .x))
write_table(query_summary, file.path(output_dir, "query_summary.csv"))

summarize_date_file <- function(file_name, date_col = "sampledate") {
  path <- file.path(output_dir, file_name)
  if (!file.exists(path)) return(tibble())
  df <- readr::read_csv(path, show_col_types = FALSE)
  if (!("masterid" %in% names(df)) || !(date_col %in% names(df))) return(tibble())

  df %>%
    mutate(
      sampledate = as.Date(.data[[date_col]]),
      year = format(sampledate, "%Y")
    ) %>%
    group_by(masterid) %>%
    summarise(
      rows = n(),
      min_sampledate = as.character(min(sampledate, na.rm = TRUE)),
      max_sampledate = as.character(max(sampledate, na.rm = TRUE)),
      years = paste(sort(unique(year)), collapse = ";"),
      target_year_rows = sum(year == target_year_chr, na.rm = TRUE),
      .groups = "drop"
    )
}

coverage_summary <- bind_rows(
  summarize_date_file("csci_by_masterid.csv") %>% mutate(source = "analysis_csci_core"),
  summarize_date_file("unified_chemistry_relevant_by_masterid.csv") %>% mutate(source = "unified_chemistry_relevant"),
  summarize_date_file("nutrients_by_masterid.csv") %>% mutate(source = "analysis_chem_nutrients_0"),
  summarize_date_file("unified_phab_wq_by_masterid.csv") %>% mutate(source = "unified_phab_wq"),
  summarize_date_file("phab_metrics_by_masterid.csv") %>% mutate(source = "analysis_phabmetrics")
) %>%
  select(source, masterid, everything())

write_table(coverage_summary, file.path(output_dir, "database_site_year_coverage_summary.csv"))

session_info <- capture.output(sessionInfo())
writeLines(session_info, file.path(log_dir, "database_site_audit_session_info.txt"))

message("Database audit complete. Outputs written to: ", normalizePath(output_dir))
