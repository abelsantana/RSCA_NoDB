# Script to check test data (CSCI score in particular)

# take input site, see if there is a corresponding csci score
# if there is a corresponding csci score, compare with scape qt10 prediction (or csci = 0.79)
# when csci > qt10/0.79, then "passing csci score"
# otherwise, continue onto RSCA analysis

# will use this output to incorporate into other functions
# so then csci score is passing, other functions return "Passing CSCI score" instead of running analyis

# CSCI thresold can be default (0.79, or scape (which would be scape qt10 value))
# TestID <- "903S02145" 

CSCI_check_fun <- function(TestID, CSCIthreshold = "default"){
  
  # list of all stations with CSCI data
  
  csci_stations <- unique(csci_base_df$masterid)
  
  # if TestID has CSCI data, then check CSCI performance
  if (TestID %in% csci_stations){
    
    csci_check <- csci_base_df %>% 
      mutate(csci_check = "yes", CSCIthresh = CSCIthreshold) %>% 
      filter(masterid == TestID) %>% 
      select(masterid, sampledate, collectionmethodcode, fieldreplicate, csci, csci_check, CSCIthresh) %>% 
      # add comid, so can join scape data
      left_join(station_base_df %>% select(masterid, comid, latitude,longitude), by = "masterid") %>% 
      # add scape data
      left_join(scape %>% select(comid, qt10, qt50, qt90) %>% mutate(comid = as.character(comid)), by = c("comid" = "comid")) %>%
      #mutate(CSCI_category_scape = case_when(csci >= qt10 ~ "Within or Above Expectation"
      # TODO added a third category vs the original 2 categories and then rename the qt columns to add scape_ to the front of the names
      mutate(CSCI_category_scape = case_when(csci >= qt90 ~"Above Expectation",
                                             csci >= qt10 ~"Within Expectation",
                                             csci < qt10 ~"Below Expectation")) %>% 
      mutate(CSCI_category = case_when(csci >= 0.79 ~ "≥ 0.79",
                                       csci < 0.79 ~ "< 0.79"),
             check_value = case_when(CSCIthresh == "default" ~ 0.79,
                                     CSCIthresh == "scape" ~ qt10)) %>%
      rename(scape_qt10 = qt10, scape_qt50 = qt50, scape_qt90 = qt90) %>%
      # final columns
      select(masterid, sampledate, collectionmethodcode, fieldreplicate, csci, scape_qt10, scape_qt50, scape_qt90, comid, CSCI_category_scape, CSCI_category, csci_check, CSCIthresh, check_value)
    
    # if TestID doesn't have CSCI data, can't run RSCA functions and return a message
    
  } else {
    
    csci_check <- data.frame(csci_check = "no")
    
    # print(paste(TestID, ": No CSCI data", sep = " "))
    
    
  }
  
  
}




