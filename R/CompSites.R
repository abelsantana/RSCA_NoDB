
# This script generates a list of all comparator sites
# before they are filtered by the test sites the user provided.
# This uses the oe_base_df dataframe created in 0.1 Data Prepping script.


# Load required libraries
library(tidyverse)
library(vegan)


# Load base data
load("Base_Files/Base_Data.RData")

# Set minimum capture probability threshold
Min.CaptureProb <- 0.5

# Prepare OTU Data (All Sites)
all_otu <- oe_base_df %>%
  select(masterid, otu, captureprob) %>%
  filter(captureprob >= Min.CaptureProb) %>%
  tidyr::pivot_wider(names_from = otu, values_from = captureprob, values_fn = max, values_fill = 0) %>%
  as.data.frame()

# Prepare Station Information
all_stationinfo <- oe_base_df %>%
  select(masterid, latitude, longitude, county) %>%
  distinct() %>%
  mutate(masterid_extra = str_replace(masterid, "-", "."))

# Set row names for dissimilarity matrix
row.names(all_otu) <- all_otu$masterid

# Compute Bray-Curtis Dissimilarity Matrix
all_sims_matrix <- as.matrix(vegdist(all_otu[, -1], method = "bray"))
all_sims_df <- data.frame(all_sims_matrix)

# Convert to Long Format (All Site Comparisons)
all_sims_df_long <- rownames_to_column(all_sims_df, var = "Site1") %>%
  pivot_longer(cols = -Site1, names_to = "Site_x", values_to = "bc_dist")

# Create Comparator Sites Dataframe (Before Filtering by Test Sites)
comparator_sites_all <- all_sims_df_long %>%
  mutate(Site2 = str_replace(Site_x, "^X", "")) %>%  # Fix site names
  left_join(all_stationinfo %>% select(masterid, latitude, longitude), by = c("Site1" = "masterid")) %>%
  rename(test_site = Site1, test_lat = latitude, test_long = longitude) %>%
  left_join(all_stationinfo, by = c("Site2" = "masterid_extra")) %>%
  rename(comp_site = masterid, comp_lat = latitude, comp_long = longitude, comp_county = county) %>%
  filter(test_site != comp_site) %>%  # Ensure test sites are not their own comparators
  na.omit() %>%
  select(test_site, test_lat, test_long, comp_site, bc_dist, comp_lat, comp_long, comp_county)

# Get Unique Comparator Sites Before Filtering
unique_comparator_sites <- comparator_sites_all %>%
  distinct(comp_site, comp_lat, comp_long, comp_county)

# Save the unique comparator sites dataframe as CSV
write_csv(unique_comparator_sites, "unique_comparator_sites.csv")

# Display results
print(unique_comparator_sites)
