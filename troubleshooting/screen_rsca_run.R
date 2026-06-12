# Screen an RSCA run for missing data, dropped samples/sites, and unusual output.
#
# This script is intentionally separate from the base workflow. It only reads:
# - an input site CSV
# - Base_Data.RData
# - an RSCA output folder
#
# Useful env vars:
# RSCA_SCREEN_INPUT_SITES   input CSV with a masterid column
# RSCA_SCREEN_OUTPUT_DIR    regular RSCA output folder, or troubleshooting/output/fresh_workflow
# RSCA_SCREEN_BASE_DATA     Base_Data.RData path
# RSCA_SCREEN_REPORT_DIR    where screening CSVs should be written
# RSCA_SCREEN_DATABASE_AUDIT_DIR optional database_site_audit.R output folder
# RSCA_SCREEN_TARGET_YEAR   optional year to require/check, for example 2025
# RSCA_SCREEN_TYPE          optional channel_engineering_class filter value

suppressPackageStartupMessages({
  library(tidyverse)
  library(readr)
})

read_config_value <- function(name, default = NA_character_) {
  config_env <- new.env(parent = baseenv())
  if (file.exists("config.R")) {
    sys.source("config.R", envir = config_env)
  }
  if (exists(name, envir = config_env, inherits = FALSE)) {
    as.character(get(name, envir = config_env))
  } else {
    default
  }
}

env_or_config <- function(env_name, config_name, default) {
  value <- Sys.getenv(env_name)
  if (nzchar(value)) return(value)
  config_value <- read_config_value(config_name, default = NA_character_)
  if (!is.na(config_value) && nzchar(config_value)) return(config_value)
  default
}

normalize_path_maybe <- function(path) {
  if (is.na(path) || !nzchar(path)) return(path)
  normalizePath(path.expand(path), winslash = "/", mustWork = FALSE)
}

input_sites_path <- normalize_path_maybe(env_or_config(
  "RSCA_SCREEN_INPUT_SITES",
  "import_sites_path",
  "troubleshooting/input/target_sites.csv"
))
output_base_dir <- normalize_path_maybe(env_or_config(
  "RSCA_SCREEN_OUTPUT_DIR",
  "output_base_dir",
  "troubleshooting/output"
))
base_data_path <- normalize_path_maybe(Sys.getenv(
  "RSCA_SCREEN_BASE_DATA",
  "Base_Files/Base_Data.RData"
))
report_dir <- normalize_path_maybe(Sys.getenv(
  "RSCA_SCREEN_REPORT_DIR",
  "troubleshooting/output/run_screen"
))
database_audit_dir <- normalize_path_maybe(Sys.getenv(
  "RSCA_SCREEN_DATABASE_AUDIT_DIR",
  "troubleshooting/output/database_audit"
))
target_year <- Sys.getenv("RSCA_SCREEN_TARGET_YEAR", "")
target_year_chr <- if (nzchar(target_year)) as.character(target_year) else NA_character_
screen_type <- Sys.getenv("RSCA_SCREEN_TYPE", read_config_value("Type", default = NA_character_))

min_comparators <- as.integer(Sys.getenv("RSCA_SCREEN_MIN_COMPARATORS", "1"))
expected_loa_per_sample <- as.integer(Sys.getenv("RSCA_SCREEN_EXPECTED_LOA_PER_SAMPLE", "12"))
high_no_test_data_fraction <- as.numeric(Sys.getenv("RSCA_SCREEN_HIGH_NO_TEST_DATA_FRACTION", "0.75"))
max_fuzzy_missing <- as.integer(Sys.getenv("RSCA_SCREEN_MAX_FUZZY_MISSING", "500"))

dir.create(report_dir, recursive = TRUE, showWarnings = FALSE)

write_table <- function(df, file_name) {
  readr::write_csv(df, file.path(report_dir, file_name), na = "")
}

read_csv_safely <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(
    readr::read_csv(path, show_col_types = FALSE, guess_max = 100000),
    error = function(e) NULL
  )
}

clean_names <- function(df) {
  if (is.null(df)) return(NULL)
  names(df) <- names(df) %>%
    tolower() %>%
    str_replace_all("[^a-z0-9]+", "_") %>%
    str_replace_all("^_|_$", "")
  df
}

read_xlsx_sheet_safely <- function(path, sheet) {
  if (!file.exists(path) || !requireNamespace("openxlsx", quietly = TRUE)) return(NULL)
  tryCatch(
    openxlsx::read.xlsx(path, sheet = sheet),
    error = function(e) NULL
  ) %>%
    clean_names()
}

site_sample_key <- function(df, site_col = "masterid") {
  if (is.null(df) || nrow(df) == 0) return(character())
  needed <- c(site_col, "sampledate", "collectionmethodcode", "fieldreplicate")
  if (!all(needed %in% names(df))) return(character())

  paste(
    df[[site_col]],
    as.character(as.Date(df$sampledate)),
    as.character(df$collectionmethodcode),
    as.character(df$fieldreplicate),
    sep = "_"
  )
}

site_col_for_df <- function(df) {
  if (is.null(df)) return(NA_character_)
  candidates <- c("test_site", "masterid", "stationcode", "site", "siteid")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

comp_col_for_df <- function(df) {
  if (is.null(df)) return(NA_character_)
  candidates <- c("comparator_site", "comp_site")
  hit <- candidates[candidates %in% names(df)]
  if (length(hit) == 0) NA_character_ else hit[[1]]
}

count_target_year <- function(dates) {
  if (is.na(target_year_chr)) return(NA_integer_)
  sum(format(as.Date(dates), "%Y") == target_year_chr, na.rm = TRUE)
}

target_year_filter <- function(df) {
  if (is.null(df) || is.na(target_year_chr) || !("sampledate" %in% names(df))) return(df)
  df %>% filter(format(as.Date(sampledate), "%Y") == target_year_chr)
}

distinct_chr <- function(x) {
  paste(sort(unique(as.character(na.omit(x)))), collapse = "; ")
}

collapse_limited <- function(x, max_items = 25) {
  x <- sort(unique(as.character(na.omit(x))))
  if (length(x) == 0) return(NA_character_)
  shown <- head(x, max_items)
  suffix <- if (length(x) > max_items) paste0("; ... +", length(x) - max_items, " more") else ""
  paste0(paste(shown, collapse = "; "), suffix)
}

issue_rows <- list()
add_issue <- function(site, check, severity, detail, evidence = NA_character_) {
  issue_rows[[length(issue_rows) + 1]] <<- tibble(
    masterid = site,
    check = check,
    severity = severity,
    detail = detail,
    evidence = evidence
  )
}

input_raw <- read.csv(input_sites_path, stringsAsFactors = FALSE)
if (!("masterid" %in% names(input_raw))) {
  stop("Input site CSV must contain a `masterid` column: ", input_sites_path, call. = FALSE)
}

input_raw <- input_raw %>%
  mutate(
    .input_row = row_number(),
    masterid = trimws(as.character(masterid))
  )

blank_input <- input_raw %>% filter(is.na(masterid) | masterid == "")
if (nrow(blank_input) > 0) {
  add_issue(
    NA_character_,
    "input_blank_masterid",
    "critical",
    "Input file has blank masterid rows.",
    paste(blank_input$.input_row, collapse = "; ")
  )
}

duplicated_input <- input_raw %>%
  filter(!is.na(masterid), masterid != "") %>%
  count(masterid, name = "input_rows") %>%
  filter(input_rows > 1)
if (nrow(duplicated_input) > 0) {
  purrr::pwalk(duplicated_input, function(masterid, input_rows) {
    add_issue(masterid, "input_duplicate_masterid", "warning", "Input file repeats this masterid.", as.character(input_rows))
  })
}

analysis_input <- input_raw %>%
  filter(!is.na(masterid), masterid != "")

if ("channel_engineering_class" %in% names(analysis_input) && !is.na(screen_type)) {
  before_filter <- analysis_input
  analysis_input <- analysis_input %>%
    filter(!is.na(channel_engineering_class)) %>%
    filter(channel_engineering_class == screen_type)

  dropped <- setdiff(before_filter$masterid, analysis_input$masterid)
  purrr::walk(dropped, ~add_issue(
    .x,
    "dropped_by_channel_engineering_filter",
    "warning",
    "This input site would be removed before RSCA processing by the channel_engineering_class filter.",
    paste0("RSCA_SCREEN_TYPE/config Type = ", screen_type)
  ))
}

target_sites <- analysis_input$masterid %>% unique()
all_requested_sites <- input_raw$masterid %>% discard(~is.na(.x) || .x == "") %>% unique()

base_env <- new.env(parent = emptyenv())
if (!file.exists(base_data_path)) {
  stop("Base data file not found: ", base_data_path, call. = FALSE)
}
load(base_data_path, envir = base_env)

get_base_df <- function(name) {
  if (exists(name, envir = base_env, inherits = FALSE)) {
    get(name, envir = base_env)
  } else {
    tibble()
  }
}

csci_base_df <- get_base_df("csci_base_df") %>% clean_names()
stressor_csci_base_df <- get_base_df("stressor_csci_base_df") %>% clean_names()
station_base_df <- get_base_df("station_base_df") %>% clean_names()
oe_base_df <- get_base_df("oe_base_df") %>% clean_names()

database_csci_all <- read_csv_safely(file.path(database_audit_dir, "csci_by_masterid.csv")) %>% clean_names()
database_csci_target_year <- read_csv_safely(file.path(database_audit_dir, paste0("csci_", target_year_chr, "_by_masterid.csv"))) %>% clean_names()
database_station <- read_csv_safely(file.path(database_audit_dir, "lu_stations_exact.csv")) %>% clean_names()

has_database_audit <- !is.null(database_csci_all) || !is.null(database_station)
expected_source <- if (!is.null(database_csci_all)) "database_audit" else "base_rdata"

base_ids <- list(
  csci = unique(csci_base_df$masterid),
  stressor = unique(stressor_csci_base_df$masterid),
  station = unique(station_base_df$masterid),
  oe = unique(oe_base_df$masterid)
)

closest_ids <- function(site, choices) {
  choices <- unique(na.omit(as.character(choices)))
  if (length(choices) == 0 || is.na(site) || site == "") return(NA_character_)
  distances <- utils::adist(toupper(site), toupper(choices))[1, ]
  min_distance <- min(distances, na.rm = TRUE)
  if (is.infinite(min_distance) || min_distance > 3) return(NA_character_)
  paste(choices[distances == min_distance], collapse = "; ")
}

summarise_base_site <- function(site) {
  csci_site <- csci_base_df %>% filter(masterid == site)
  stressor_site <- stressor_csci_base_df %>% filter(masterid == site)
  station_site <- station_base_df %>% filter(masterid == site)
  oe_site <- oe_base_df %>% filter(masterid == site)

  tibble(
    masterid = site,
    in_analysis_input = site %in% target_sites,
    csci_rows = nrow(csci_site),
    csci_samples = n_distinct(site_sample_key(csci_site, "masterid")),
    csci_target_year_rows = count_target_year(csci_site$sampledate),
    stressor_rows = nrow(stressor_site),
    stressor_samples = n_distinct(site_sample_key(stressor_site, "masterid")),
    stressor_target_year_rows = count_target_year(stressor_site$sampledate),
    station_rows = nrow(station_site),
    oe_rows = nrow(oe_site),
    csci_years = if ("sampledate" %in% names(csci_site)) distinct_chr(format(as.Date(csci_site$sampledate), "%Y")) else NA_character_,
    stressor_years = if ("sampledate" %in% names(stressor_site)) distinct_chr(format(as.Date(stressor_site$sampledate), "%Y")) else NA_character_
  )
}

expected_csci_for_site <- function(site) {
  if (!is.null(database_csci_all)) {
    expected <- database_csci_all %>% filter(masterid == site)
    if (!is.na(target_year_chr)) {
      target_expected <- database_csci_target_year
      if (!is.null(target_expected)) {
        expected <- target_expected %>% filter(masterid == site)
      } else {
        expected <- expected %>% filter(format(as.Date(sampledate), "%Y") == target_year_chr)
      }
    }
    return(expected)
  }

  expected <- csci_base_df %>% filter(masterid == site)
  if (!is.na(target_year_chr)) {
    expected <- expected %>% filter(format(as.Date(sampledate), "%Y") == target_year_chr)
  }
  expected
}

output_sample_keys <- function(df, expected_site) {
  if (is.null(df) || nrow(df) == 0) return(character())
  site_col <- site_col_for_df(df)
  if (is.na(site_col)) return(character())
  keys <- site_sample_key(df, site_col)
  if (length(keys) == 0) return(character())

  # stationcode can describe the test site in some database extracts, but final
  # RSCA outputs should use test_site. Keep the generic path for diagnostics.
  keys
}

validate_output_table <- function(site, table_name, df, expected_keys, required = TRUE) {
  rows <- row_count(df)
  site_col <- site_col_for_df(df)
  site_values <- if (!is.null(df) && !is.na(site_col)) {
    unique_values <- unique(as.character(na.omit(df[[site_col]])))
    paste(sort(unique_values), collapse = "; ")
  } else {
    NA_character_
  }

  table_keys <- output_sample_keys(df, site)
  missing_keys <- if (length(expected_keys) > 0 && length(table_keys) > 0) setdiff(expected_keys, table_keys) else character()
  extra_keys <- if (length(expected_keys) > 0 && length(table_keys) > 0) setdiff(table_keys, expected_keys) else character()
  wrong_site_values <- if (!is.null(df) && !is.na(site_col)) {
    setdiff(unique(as.character(na.omit(df[[site_col]]))), site)
  } else {
    character()
  }

  if (required && rows == 0) {
    add_issue(site, paste0("empty_or_missing_", table_name), "critical", "Required output table is missing or has zero rows.", table_name)
  }
  if (rows > 0 && is.na(site_col)) {
    add_issue(site, paste0(table_name, "_missing_site_column"), "warning", "Output table has rows but no recognizable site column.", table_name)
  }
  if (rows > 0 && !is.na(site_col) && !(site %in% unique(as.character(df[[site_col]])))) {
    add_issue(site, paste0(table_name, "_does_not_contain_requested_site"), "critical", "Output table exists but does not contain the requested site ID inside the data.", site_values)
  }
  if (length(wrong_site_values) > 0) {
    add_issue(site, paste0(table_name, "_contains_other_sites"), "warning", "Output table contains site IDs other than the requested site.", collapse_limited(wrong_site_values))
  }
  if (length(expected_keys) > 0 && length(table_keys) == 0 && rows > 0 && all(c("sampledate", "collectionmethodcode", "fieldreplicate") %in% names(df))) {
    add_issue(site, paste0(table_name, "_sample_key_not_readable"), "warning", "Output table has sample fields, but sample keys could not be built.", table_name)
  }
  if (length(missing_keys) > 0) {
    add_issue(site, paste0(table_name, "_missing_expected_database_samples"), "critical", "Expected database/base CSCI sample keys are absent from this output table.", collapse_limited(missing_keys))
  }

  tibble(
    masterid = site,
    table_name = table_name,
    rows = rows,
    site_column = site_col,
    site_values = site_values,
    expected_sample_count = length(unique(expected_keys)),
    output_sample_count = length(unique(table_keys)),
    missing_expected_sample_count = length(missing_keys),
    extra_output_sample_count = length(extra_keys),
    missing_expected_samples = paste(missing_keys, collapse = "; "),
    extra_output_samples = paste(extra_keys, collapse = "; ")
  )
}

read_site_outputs <- function(site) {
  site_dir <- file.path(output_base_dir, site)
  summary_xlsx <- file.path(site_dir, paste0(site, "_Summary_Site_Data.xlsx"))
  monitoring_xlsx <- file.path(site_dir, paste0(site, "_Monitoring_Recommendations.xlsx"))

  regular <- list(
    module_summary = read_xlsx_sheet_safely(summary_xlsx, "Module Summary"),
    loa_summary = read_xlsx_sheet_safely(summary_xlsx, "LOA Summary"),
    reference_condition_comparison = read_xlsx_sheet_safely(summary_xlsx, "Reference Condition Comparison"),
    stressor_response_summary = read_xlsx_sheet_safely(summary_xlsx, "Stressor Response Summary"),
    spatial_cooccurrence_summary = read_xlsx_sheet_safely(summary_xlsx, "Spatial Co-Occurrence Summary"),
    comparator_site_data = read_xlsx_sheet_safely(summary_xlsx, "RSCA Comparator Site Data"),
    monitoring_recommendations = if (file.exists(monitoring_xlsx) && requireNamespace("openxlsx", quietly = TRUE)) {
      read_xlsx_sheet_safely(monitoring_xlsx, 1)
    } else {
      NULL
    }
  )

  troubleshooting <- list(
    module_summary = read_csv_safely(file.path(site_dir, paste0(site, "_module_summary.csv"))) %>% clean_names(),
    loa_summary = read_csv_safely(file.path(site_dir, paste0(site, "_loa_summary.csv"))) %>% clean_names(),
    reference_condition_comparison = read_csv_safely(file.path(site_dir, paste0(site, "_reference_condition_comparison.csv"))) %>% clean_names(),
    stressor_response_summary = read_csv_safely(file.path(site_dir, paste0(site, "_stressor_response_summary.csv"))) %>% clean_names(),
    spatial_cooccurrence_summary = read_csv_safely(file.path(site_dir, paste0(site, "_spatial_cooccurrence_summary.csv"))) %>% clean_names(),
    comparator_site_data = read_csv_safely(file.path(site_dir, paste0(site, "_rsca_comparator_site_data.csv"))) %>% clean_names(),
    monitoring_recommendations = read_csv_safely(file.path(site_dir, paste0(site, "_monitoring_recommendations.csv"))) %>% clean_names()
  )

  first_df <- function(...) {
    paths <- c(...)
    for (path in paths) {
      df <- read_csv_safely(path) %>% clean_names()
      if (!is.null(df)) return(df)
    }
    NULL
  }

  level3 <- list(
    sco_dat = first_df(
      file.path(site_dir, paste0(site, "_SCO_dat_df.csv")),
      file.path(site_dir, paste0(site, "_03_SCO_dat.csv"))
    ),
    sco_loe = first_df(
      file.path(site_dir, paste0(site, "_SCO_LOE_df.csv")),
      file.path(site_dir, paste0(site, "_04_SCO_LOE.csv"))
    ),
    rcc_dat = first_df(
      file.path(site_dir, paste0(site, "_RCC_dat_df.csv")),
      file.path(site_dir, paste0(site, "_06_RCC_dat.csv"))
    ),
    rcc_loe = first_df(
      file.path(site_dir, paste0(site, "_RCC_LOE_df.csv")),
      file.path(site_dir, paste0(site, "_07_RCC_LOE.csv"))
    ),
    sr_log_dat = first_df(
      file.path(site_dir, paste0(site, "_SR_log_dat_df.csv")),
      file.path(site_dir, paste0(site, "_09_SR_log_dat.csv"))
    ),
    sr_log_loe = first_df(
      file.path(site_dir, paste0(site, "_SR_log_LOE_df.csv")),
      file.path(site_dir, paste0(site, "_10_SR_log_LOE.csv"))
    )
  )

  outputs <- purrr::imap(regular, function(df, nm) {
    if (!is.null(df)) df else troubleshooting[[nm]]
  })

  list(
    site_dir = site_dir,
    summary_xlsx = summary_xlsx,
    monitoring_xlsx = monitoring_xlsx,
    used_regular_xlsx = file.exists(summary_xlsx) && any(purrr::map_lgl(regular, ~!is.null(.x))),
    outputs = outputs,
    level3 = level3
  )
}

row_count <- function(df) {
  if (is.null(df)) 0L else nrow(df)
}

score_distribution <- function(site, stage, df, score_col) {
  if (is.null(df) || !(score_col %in% names(df))) return(tibble())
  df %>%
    mutate(score = as.character(.data[[score_col]])) %>%
    count(masterid = site, stage = stage, score, name = "rows") %>%
    mutate(fraction = rows / sum(rows))
}

compare_comp_sites <- function(site, comp_data, detail_tables) {
  comp_col <- comp_col_for_df(comp_data)
  selected <- if (!is.null(comp_data) && !is.na(comp_col)) {
    unique(as.character(na.omit(comp_data[[comp_col]])))
  } else {
    character()
  }

  missing_station <- setdiff(selected, unique(as.character(station_base_df$masterid)))
  missing_oe <- setdiff(selected, unique(as.character(oe_base_df$masterid)))

  if (length(selected) > 0 && length(missing_station) > 0) {
    add_issue(site, "comparator_sites_missing_from_station_base", "warning", "Comparator output contains sites not found in station_base_df.", collapse_limited(missing_station))
  }
  if (length(selected) > 0 && length(missing_oe) > 0) {
    add_issue(site, "comparator_sites_missing_from_oe_base", "warning", "Comparator output contains sites not found in oe_base_df.", collapse_limited(missing_oe))
  }

  detail_rows <- purrr::imap_dfr(detail_tables, function(df, table_name) {
    detail_comp_col <- comp_col_for_df(df)
    detail_sites <- if (!is.null(df) && !is.na(detail_comp_col)) unique(as.character(na.omit(df[[detail_comp_col]]))) else character()
    missing_from_detail <- if (length(selected) > 0 && length(detail_sites) > 0) setdiff(selected, detail_sites) else character()

    if (length(missing_from_detail) > 0) {
      add_issue(
        site,
        paste0("comparator_sites_missing_from_", table_name),
        "warning",
        "Comparator sites listed in RSCA Comparator Site Data are absent from this detail table. This can be normal when a comparator lacks the detail data used by that LOE, but it is useful for data-loss review.",
        collapse_limited(missing_from_detail)
      )
    }

    tibble(
      masterid = site,
      detail_table = table_name,
      selected_comparator_sites = length(selected),
      detail_comparator_sites = length(detail_sites),
      missing_selected_comparator_count = length(missing_from_detail),
      missing_selected_comparators = paste(missing_from_detail, collapse = "; ")
    )
  })

  summary_row <- tibble(
    masterid = site,
    detail_table = "comparator_site_data",
    selected_comparator_sites = length(selected),
    detail_comparator_sites = NA_integer_,
    missing_selected_comparator_count = NA_integer_,
    missing_selected_comparators = NA_character_
  )

  bind_rows(summary_row, detail_rows)
}

no_test_fraction <- function(df, score_col) {
  if (is.null(df) || nrow(df) == 0) return(NA_real_)
  score_no_data <- if (score_col %in% names(df)) {
    str_detect(tolower(as.character(df[[score_col]])), "no test data|no evidence")
  } else {
    rep(FALSE, nrow(df))
  }
  result_blank <- if ("test_result" %in% names(df)) {
    is.na(df$test_result) | trimws(as.character(df$test_result)) == ""
  } else {
    rep(FALSE, nrow(df))
  }
  mean(score_no_data | result_blank, na.rm = TRUE)
}

base_status <- purrr::map_dfr(all_requested_sites, summarise_base_site)

missing_for_fuzzy <- base_status %>%
  filter(csci_rows == 0 | station_rows == 0) %>%
  pull(masterid) %>%
  head(max_fuzzy_missing)

fuzzy_matches <- tibble(
  masterid = missing_for_fuzzy,
  closest_csci_masterid = purrr::map_chr(missing_for_fuzzy, closest_ids, choices = base_ids$csci),
  closest_station_masterid = purrr::map_chr(missing_for_fuzzy, closest_ids, choices = base_ids$station)
)

site_status_rows <- list()
missing_sample_rows <- list()
score_rows <- list()
output_content_rows <- list()
comparator_accounting_rows <- list()

for (site in all_requested_sites) {
  base_site <- base_status %>% filter(masterid == site)
  outputs <- read_site_outputs(site)
  out <- outputs$outputs
  level3 <- outputs$level3

  module_summary <- out$module_summary
  loa_summary <- out$loa_summary
  ref_con <- out$reference_condition_comparison
  stress_resp <- out$stressor_response_summary
  spatial_co <- out$spatial_cooccurrence_summary
  comp_data <- out$comparator_site_data
  monitoring <- out$monitoring_recommendations

  expected_csci <- expected_csci_for_site(site)
  expected_keys <- site_sample_key(expected_csci, "masterid")
  output_keys <- site_sample_key(module_summary, "test_site")
  missing_keys <- setdiff(expected_keys, output_keys)
  extra_keys <- setdiff(output_keys, expected_keys)

  final_tables <- list(
    module_summary = module_summary,
    loa_summary = loa_summary,
    reference_condition_comparison = ref_con,
    stressor_response_summary = stress_resp,
    spatial_cooccurrence_summary = spatial_co,
    comparator_site_data = comp_data,
    monitoring_recommendations = monitoring
  )

  purrr::iwalk(final_tables, function(df, table_name) {
    # Comparator and monitoring tables do not necessarily carry one row per
    # test sample, but they still need to contain the requested site ID.
    keys_for_table <- if (table_name %in% c("comparator_site_data", "monitoring_recommendations")) character() else expected_keys
    output_content_rows[[length(output_content_rows) + 1]] <<- validate_output_table(
      site = site,
      table_name = table_name,
      df = df,
      expected_keys = keys_for_table,
      required = table_name %in% c("module_summary", "loa_summary", "comparator_site_data")
    )
  })

  purrr::iwalk(level3, function(df, table_name) {
    output_content_rows[[length(output_content_rows) + 1]] <<- validate_output_table(
      site = site,
      table_name = paste0("level3_", table_name),
      df = df,
      expected_keys = expected_keys,
      required = FALSE
    )
  })

  comparator_accounting_rows[[length(comparator_accounting_rows) + 1]] <- compare_comp_sites(
    site = site,
    comp_data = comp_data,
    detail_tables = list(
      sco_dat = level3$sco_dat,
      rcc_dat = level3$rcc_dat,
      sr_log_dat = level3$sr_log_dat
    )
  )

  if (nrow(base_site) == 0 || base_site$csci_rows == 0) {
    add_issue(site, "missing_from_csci_base", "critical", "Site has no exact masterid match in csci_base_df.", NA_character_)
  }
  if (nrow(base_site) > 0 && base_site$station_rows == 0) {
    add_issue(site, "missing_from_station_base", "critical", "Site has no exact masterid match in station_base_df.", NA_character_)
  }
  if (nrow(base_site) > 0 && base_site$stressor_rows == 0) {
    add_issue(site, "missing_from_stressor_base", "warning", "Site has no exact masterid match in stressor_csci_base_df.", NA_character_)
  }
  if (!is.na(target_year_chr) && nrow(base_site) > 0 && base_site$csci_target_year_rows == 0) {
    add_issue(site, "no_target_year_csci_in_base", "warning", "No CSCI rows in Base_Data.RData for the requested target year.", target_year_chr)
  }
  if (!is.na(target_year_chr) && nrow(base_site) > 0 && base_site$stressor_target_year_rows == 0) {
    add_issue(site, "no_target_year_stressor_in_base", "warning", "No stressor rows in Base_Data.RData for the requested target year.", target_year_chr)
  }
  if (site %in% target_sites && !dir.exists(outputs$site_dir) && base_site$csci_rows > 0) {
    add_issue(site, "missing_output_site_folder", "critical", "Site has base CSCI data but no output folder.", outputs$site_dir)
  }
  if (site %in% target_sites && dir.exists(outputs$site_dir) && row_count(module_summary) == 0) {
    add_issue(site, "missing_or_empty_module_summary", "critical", "Output has no Module Summary rows.", outputs$summary_xlsx)
  }
  if (site %in% target_sites && dir.exists(outputs$site_dir) && row_count(loa_summary) == 0) {
    add_issue(site, "missing_or_empty_loa_summary", "critical", "Output has no LOA Summary rows.", outputs$summary_xlsx)
  }
  if (site %in% target_sites && dir.exists(outputs$site_dir) && row_count(monitoring) == 0) {
    add_issue(site, "missing_or_empty_monitoring_recommendations", "warning", "Output has no Monitoring Recommendations rows.", outputs$monitoring_xlsx)
  }
  if (row_count(comp_data) < min_comparators && base_site$csci_rows > 0) {
    add_issue(site, "low_or_missing_comparator_sites", "critical", "Comparator site output has fewer rows than the configured minimum.", as.character(row_count(comp_data)))
  }
  if (length(missing_keys) > 0) {
    add_issue(site, "expected_csci_samples_missing_from_module_summary", "critical", "One or more expected CSCI samples are missing from Module Summary.", collapse_limited(missing_keys))
    missing_sample_rows[[length(missing_sample_rows) + 1]] <- tibble(masterid = site, missing_sample_key = missing_keys)
  }
  if (length(extra_keys) > 0 && length(expected_keys) > 0) {
    add_issue(site, "output_samples_not_expected_from_base", "warning", "Module Summary contains samples not expected from Base_Data.RData and target year settings.", paste(extra_keys, collapse = "; "))
  }
  if (row_count(module_summary) > 0 && length(unique(output_keys)) < length(output_keys)) {
    add_issue(site, "duplicate_module_summary_samples", "warning", "Module Summary has duplicate test sample keys.", NA_character_)
  }
  if (row_count(module_summary) > 0 && length(expected_keys) > 0 && length(output_keys) != length(expected_keys)) {
    add_issue(site, "module_summary_sample_count_mismatch", "warning", "Module Summary sample count does not match expected CSCI sample count.", paste0("expected=", length(expected_keys), "; output=", length(output_keys)))
  }
  if (row_count(module_summary) > 0 && row_count(loa_summary) > 0) {
    expected_loa_rows <- length(unique(output_keys)) * expected_loa_per_sample
    if (row_count(loa_summary) != expected_loa_rows) {
      add_issue(site, "loa_row_count_unusual", "warning", "LOA Summary row count differs from expected rows per output sample.", paste0("expected=", expected_loa_rows, "; output=", row_count(loa_summary)))
    }
  }

  loe_checks <- list(
    spatial_cooccurrence = list(df = spatial_co, score_col = "sco_score"),
    reference_condition = list(df = ref_con, score_col = "rcc_score"),
    stressor_response = list(df = stress_resp, score_col = "sr_score")
  )
  purrr::iwalk(loe_checks, function(spec, stage) {
    frac <- no_test_fraction(spec$df, spec$score_col)
    if (!is.na(frac) && frac >= high_no_test_data_fraction && row_count(spec$df) > 0) {
      add_issue(site, paste0(stage, "_high_no_test_data_fraction"), "warning", "A large fraction of LOE rows have blank test_result, No Test Data, or No Evidence.", sprintf("%.3f", frac))
    }
    score_rows[[length(score_rows) + 1]] <<- score_distribution(site, stage, spec$df, spec$score_col)
  })

  if (row_count(loa_summary) > 0 && "score" %in% names(loa_summary)) {
    loa_scores <- unique(na.omit(as.character(loa_summary$score)))
    if (length(loa_scores) == 1 && loa_scores == "Passing CSCI") {
      add_issue(site, "all_loa_scores_passing_csci", "info", "All LOA scores are Passing CSCI. This can be valid when all CSCI scores exceed the threshold.", NA_character_)
    }
  }

  purrr::iwalk(level3, function(df, nm) {
    if (dir.exists(outputs$site_dir) && is.null(df)) {
      add_issue(site, paste0("missing_level3_", nm), "info", "Level 3 CSV was not found. This is expected for troubleshooting-style output but useful for regular runs.", NA_character_)
    } else if (!is.null(df) && nrow(df) == 0) {
      add_issue(site, paste0("empty_level3_", nm), "warning", "Level 3 CSV exists but has zero rows.", NA_character_)
    }
  })

  site_status_rows[[length(site_status_rows) + 1]] <- tibble(
    masterid = site,
    in_analysis_input = site %in% target_sites,
    output_site_dir_exists = dir.exists(outputs$site_dir),
    used_regular_xlsx = outputs$used_regular_xlsx,
    base_csci_rows = base_site$csci_rows,
    base_csci_samples = base_site$csci_samples,
    base_csci_target_year_rows = base_site$csci_target_year_rows,
    base_stressor_rows = base_site$stressor_rows,
    base_stressor_target_year_rows = base_site$stressor_target_year_rows,
    base_station_rows = base_site$station_rows,
    base_oe_rows = base_site$oe_rows,
    expected_output_samples = length(unique(expected_keys)),
    module_summary_rows = row_count(module_summary),
    module_summary_samples = length(unique(output_keys)),
    loa_summary_rows = row_count(loa_summary),
    reference_condition_rows = row_count(ref_con),
    stressor_response_rows = row_count(stress_resp),
    spatial_cooccurrence_rows = row_count(spatial_co),
    comparator_rows = row_count(comp_data),
    comparator_sites = if (!is.null(comp_data) && "comparator_site" %in% names(comp_data)) n_distinct(comp_data$comparator_site, na.rm = TRUE) else NA_integer_,
    monitoring_rows = row_count(monitoring),
    missing_output_sample_count = length(missing_keys),
    extra_output_sample_count = length(extra_keys)
  )
}

site_status <- bind_rows(site_status_rows) %>%
  left_join(fuzzy_matches, by = "masterid")

issue_log <- bind_rows(issue_rows) %>%
  arrange(
    factor(severity, levels = c("critical", "warning", "info")),
    masterid,
    check
  )

issue_summary <- issue_log %>%
  count(severity, check, name = "sites_or_rows") %>%
  arrange(factor(severity, levels = c("critical", "warning", "info")), desc(sites_or_rows), check)

score_summary <- bind_rows(score_rows) %>%
  arrange(masterid, stage, desc(rows), score)

missing_samples <- bind_rows(missing_sample_rows)
output_content_accounting <- bind_rows(output_content_rows) %>%
  arrange(masterid, table_name)
comparator_accounting <- bind_rows(comparator_accounting_rows) %>%
  arrange(masterid, detail_table)

run_metadata <- tibble(
  field = c(
    "input_sites_path",
    "output_base_dir",
    "base_data_path",
    "report_dir",
    "database_audit_dir",
    "expected_source",
    "database_audit_found",
    "target_year",
    "screen_type",
    "requested_site_count",
    "analysis_site_count",
    "critical_issue_count",
    "warning_issue_count"
  ),
  value = c(
    input_sites_path,
    output_base_dir,
    base_data_path,
    report_dir,
    database_audit_dir,
    expected_source,
    as.character(has_database_audit),
    ifelse(is.na(target_year_chr), "", target_year_chr),
    ifelse(is.na(screen_type), "", screen_type),
    as.character(length(all_requested_sites)),
    as.character(length(target_sites)),
    as.character(sum(issue_log$severity == "critical", na.rm = TRUE)),
    as.character(sum(issue_log$severity == "warning", na.rm = TRUE))
  )
)

write_table(run_metadata, "screening_run_metadata.csv")
write_table(site_status, "screening_site_status.csv")
write_table(issue_log, "screening_issue_log.csv")
write_table(issue_summary, "screening_issue_summary.csv")
write_table(score_summary, "screening_score_summary.csv")
write_table(missing_samples, "screening_missing_samples.csv")
write_table(output_content_accounting, "screening_output_content_accounting.csv")
write_table(comparator_accounting, "screening_comparator_accounting.csv")

message("RSCA screening complete. Reports written to: ", normalizePath(report_dir, winslash = "/"))
message("Critical issues: ", sum(issue_log$severity == "critical", na.rm = TRUE))
message("Warning issues: ", sum(issue_log$severity == "warning", na.rm = TRUE))
