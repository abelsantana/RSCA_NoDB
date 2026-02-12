# ============================================================================
# RSCA Configuration File
# ============================================================================
# This is the main configuration file for the RSCA (Regional Stream Condition 
# Assessment) workflow. All analysis options are controlled here. Users should 
# only need to modify this file - do NOT edit Main.R or other scripts.
# ============================================================================

# ============================================================================
# WORKFLOW CONTROL - Choose what to run
# ============================================================================

# Set to TRUE to regenerate the base data from the database
# Only set this to TRUE if the source database has been updated
# This requires a valid database connection (con)
# Default: FALSE (use existing Base_Data.RData)
run_data_prep <- FALSE

# Set to TRUE to run the main RSCA analysis
# This processes the sites specified in import_sites_path
# Default: TRUE
run_analysis <- TRUE

# ============================================================================
# ANALYSIS PARAMETERS
# ============================================================================

# Define Channel Engineering Type for filtering
# Change this to "HB", "SB1" etc. to filter specific test sites
# Leave it NA to use the defined class per site in the input .csv file
Type <- "NA"

# User-defined switch for graph generation
# Options: "none", "primary", "secondary", "both"
# "none" = no graphs
# "primary" = summary plots (CSCI time-series and module assessment tiles)
# "secondary" = detailed LOE plots (boxplots, regression curves, etc.)
# "both" = all graphs
graph_mode <- "primary"

# Toggle for CSV merging at the end (TRUE/FALSE)
# If TRUE, individual site CSV files will be merged into summary files
merge_csvs <- FALSE

# What chunk to start processing the data. Default is 1
# If the process gets interrupted, you can restart from a specific chunk
# Example: If you were processing 100 sites in chunks of 20, and it failed
# on chunk 3, set this to 3 to resume from there
chunk_start <- 1

# ============================================================================
# FILE PATHS
# ============================================================================

# Define Output Directory for Processed Data
# All results (Excel reports, CSVs, plots) will be saved here
output_base_dir <- "~/MyR/RSCA_NoDB/output/NewTest_TEST"

# Define path for the input sites CSV file
# This file should contain a column called "masterid" with the site identifiers
import_sites_path <- "~/MyR/RSCA_NoDB/input/NewTest.csv"
