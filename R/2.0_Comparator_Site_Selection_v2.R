# RSCA Comparator Site Selection 

##### FUNCTION/ANALYSIS ####
Comp_Select_Modified_fun <- function(TestID, Min.CaptureProb = 0.5, Max.BC = 0.1, Type = NA){
  
  # Load base data
  
  if ("channel_engineering_class" %in% colnames(import_sites)) {
    test_site_class <- import_sites %>% filter(masterid == TestID) %>% pull(channel_engineering_class)
  } else {
    test_site_class <- NA  # If missing, set to NA to skip Class filtering
  }
  
  # If Type is specified (not NA), only run test sites where Class matches Type
  if (!is.na(Type) && !is.na(test_site_class) && test_site_class != Type) {
    print(paste("Skipping test site", TestID, "because it does not match specified Type:", Type))
    return(NULL)  # Skip this test site and move to the next one
  }

  # prepping otu data, wide format, retaining capture probs greater than the minimum set 
  all_otu <- oe_base_df %>% 
    select(masterid, otu, captureprob) %>% 
    filter(captureprob >= Min.CaptureProb) %>% 
    tidyr::pivot_wider(names_from = otu, values_from = captureprob, values_fn = max, values_fill = 0) %>% 
    as.data.frame()
  
  all_stationinfo <- oe_base_df %>% 
    select(masterid, latitude, longitude, county) %>% 
    distinct() %>% 
    # prepping new masterid column, for joining, because formatting gets weird
    # dashes seem to be replaced with dots during the dissimilarity analysis  (for ex SAR-7 replaced with SAR.7)
    # will later join back data by this masterid_extra column, but will only later retain true masterid
    mutate(masterid_extra = str_replace(masterid, "-", "."))
  
  row.names(all_otu) <- all_otu$masterid
  
  # dissimilarity indices
  # Bray Curtis dissimilarity, save as data frame
  all_sims_matrix <- as.matrix(vegdist(all_otu[,c(2:(ncol(all_otu)))],method = "bray"))
  all_sims_df <- data.frame(all_sims_matrix)
  
  # get info for one test site at a time, make long format
  all_sims_df_long <-rownames_to_column(all_sims_df,var="Site1") %>% #make the site row names back into a column
    # one test site at a time
    filter(Site1 == TestID) %>% 
    # pivot all sites long format, besides test site (Site 1) column
    pivot_longer(-(Site1), names_to = "Site_x", values_to = "bc_dist")
  # note that Site_x retains slight reformatting from vegan functions (leading X, and "-" replaced with ".")
  
  # create final test/comp dataset
  comparator_sites <- all_sims_df_long %>% 
    # fix site names (those starting with numbers had X in front)
    # Site2 still has "-" replaced by "."
    mutate(Site2 = (str_replace(Site_x, "^X", ""))) %>% 
    # add back test station information (masterid, latitude, longitude)
    # join by masterid_extra, which takes into account the format changes identified thus far
    left_join(all_stationinfo %>% select(masterid,latitude,longitude), by = c("Site1" = "masterid")) %>% 
    # identify as test site data
    rename(test_site = Site1,test_lat = latitude, test_long = longitude) %>% 
    # add comparator site information (latitude, longitude, county, comparator csci)
    left_join(all_stationinfo, by = c("Site2" = "masterid_extra")) %>% 
    # identify as comparator site data
    rename(comp_site = masterid, comp_lat = latitude, comp_long = longitude, comp_county = county) %>% 
    # drop extra columns
    # select(-Site1, -Site_x)
    # do not retain matching test and comp sites
    filter(test_site != comp_site) %>% 
    # filter out by BC dist
    filter(bc_dist <= Max.BC) %>% 
    # rate comparator quality 
    mutate(comparator_quality =(case_when(bc_dist<=0.05~"Great",
                                          bc_dist>0.05&bc_dist<=0.1~"Good",
                                          bc_dist>0.1&bc_dist<=0.2~"Mediocre",
                                          bc_dist>0.2~"Poor" ))) %>% 
    # remove any NAs for now
    na.omit() %>%
    select(test_site, test_lat, test_long, comp_site, bc_dist, comparator_quality, comp_lat, comp_long, comp_county)
  
  # Join comparator sites with channel engineering data (if available)
  if ("channel_engineering_class" %in% colnames(import_sites)) {
    comparator_sites <- comparator_sites %>%
      inner_join(chansum_df %>% select(comp_site, Class), by = "comp_site") %>%
      filter(Class == test_site_class)
  } 
  
  # Return final dataset
  tibble(comparator_sites)
}
