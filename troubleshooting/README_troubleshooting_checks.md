# RSCA Troubleshooting Checks

These scripts are a diagnostic layer around the RSCA workflow. They do not edit or replace the base code.

## Fast Batch Screen

Use `troubleshooting/screen_rsca_run.R` after a regular RSCA run to find:

- input sites with no matching base data
- likely site-code misspellings or case mismatches
- sites/samples that were present in `Base_Data.RData` but absent from outputs
- missing output folders, workbooks, sheets, or monitoring files
- missing or low comparator-site output
- output tables whose internal `test_site`/sample keys do not match the requested/database sites
- selected comparator sites that disappear from detailed SCO/RCC/SR data tables
- unusual row counts in Module Summary and LOA Summary
- high fractions of `No Test Data`, `No Evidence`, or blank `test_result`

Example for a regular run:

```powershell
$env:RSCA_SCREEN_INPUT_SITES = "input/NewTest.csv"
$env:RSCA_SCREEN_OUTPUT_DIR = "output/NewTest_TEST"
$env:RSCA_SCREEN_BASE_DATA = "Base_Files/Base_Data.RData"
$env:RSCA_SCREEN_TARGET_YEAR = "2025"
$env:RSCA_SCREEN_REPORT_DIR = "troubleshooting/output/run_screen"
Rscript troubleshooting/screen_rsca_run.R
```

Main outputs:

- `screening_issue_log.csv`: one row per issue; start here
- `screening_site_status.csv`: one row per requested site with base-data and output counts
- `screening_issue_summary.csv`: counts by severity and check
- `screening_missing_samples.csv`: exact CSCI sample keys expected from base data but missing from output
- `screening_output_content_accounting.csv`: per-site, per-output-table check of the site IDs and sample keys inside each file
- `screening_comparator_accounting.csv`: selected comparator sites vs comparator sites present in detailed SCO/RCC/SR data
- `screening_score_summary.csv`: output score distributions by site and LOE

If `troubleshooting/output/database_audit/` exists, the screen uses the database audit CSCI rows as the expected source. Otherwise it uses `Base_Data.RData`. You can override that folder with `RSCA_SCREEN_DATABASE_AUDIT_DIR`.

## Detailed Per-Site Troubleshooting

Once the fast screen identifies suspicious sites, put those `masterid` values in:

```text
troubleshooting/input/target_sites.csv
```

Then use the existing detailed scripts:

```powershell
Rscript troubleshooting/troubleshoot_sites.R
```

For a fresh-data check, rebuild troubleshooting-only RData first, then point the detailed workflow at it:

```powershell
Rscript troubleshooting/rebuild_fresh_rdata.R

$env:RSCA_BASE_DATA_PATH = "troubleshooting/fresh_rdata/Base_Files/Base_Data.RData"
$env:RSCA_MODULE_ASSIGNMENTS_PATH = "troubleshooting/fresh_rdata/Base_Files/RSCA_Module_Direction_Assignments.RData"
$env:RSCA_TROUBLESHOOT_OUTPUT_DIR = "troubleshooting/output/fresh_workflow"
$env:RSCA_TROUBLESHOOT_LOG_DIR = "troubleshooting/logs/fresh_workflow"
Rscript troubleshooting/troubleshoot_sites.R
Rscript troubleshooting/audit_fresh_2025_usage.R
```

## Optional Thresholds

`screen_rsca_run.R` supports these optional env vars:

- `RSCA_SCREEN_MIN_COMPARATORS`, default `1`
- `RSCA_SCREEN_EXPECTED_LOA_PER_SAMPLE`, default `12`
- `RSCA_SCREEN_HIGH_NO_TEST_DATA_FRACTION`, default `0.75`
- `RSCA_SCREEN_MAX_FUZZY_MISSING`, default `500`
