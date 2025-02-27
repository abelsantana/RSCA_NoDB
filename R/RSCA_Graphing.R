# RSCA Graphing
# create graphs just for SMC sites/deliverables, not part of dashboard 

# part 1:
# 1) Csci vs time
# + ribbon in background showing expected range (q10, q90) and median expectation q50
# + dots coded for above (up triangle), below (down triangle), and within (circle) expectations
# 2) Module vs time
# + tile color coded by outcome for that date

# part 2
# Lines of analysis vs time
# + tile color coded by loe score (strengthening, indeterminate, weakening, passing csci, no data)
# also analyte vs time, colored by loe score

# part 3
# detail graphics of comparator data behind each LOE result
# graphs of observed test results for lines of evidence

# required packages
library(tidyverse)
library(lubridate)
library(ggpubr)

site_csci_module_plotter <- function(my_site, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # prepping data
  # retain one site at a time, create Performance metrics for csci scores
  csci_module_data <- LOE_mod_sum_df %>% 
    filter(test_site == my_site) %>%     #edited by rachel 
    mutate(Year = year(sampledate)) %>% 
    mutate(Year = as.numeric(Year)) %>% 
    mutate(Performance = case_when(csci > qt90 ~ "Over Scoring",
                                   csci <= qt90 & csci >= qt10 ~ "Expected",
                                   csci < qt10 ~ "Under Scoring",
                                   TRUE ~ "Indeterminate")) %>% 
    ### changing text in table to match newer versions requested by Rafi and David - edited by rachel 1/6
    mutate(conductivity = ifelse(conductivity == "Passing CSCI", "CSCI > 0.79", conductivity)) %>%
    mutate(eutrophication = ifelse(eutrophication == "Passing CSCI", "CSCI > 0.79", eutrophication)) %>%
    mutate(habitat = ifelse(habitat == "Passing CSCI", "CSCI > 0.79", habitat)) %>%
    mutate(temperature = ifelse(temperature == "Passing CSCI", "CSCI > 0.79", temperature)) %>%
    # ordering categorical data
    mutate(Performance = factor(Performance, levels = c("Over Scoring", "Expected", "Under Scoring", "Indeterminate"))) 
  
  # plotting csci over time for each site
  site_csci_plot <- ggplot(data = csci_module_data,
                           aes(x = Year, y = csci))+
    # graphing 10th and 90th percentile range SCAPE predictions
    geom_rect(aes(ymin = qt10, ymax = qt90, xmin = -Inf, xmax = Inf), fill = 'grey92')+
    # adding reference line, qt50
    geom_hline(yintercept = csci_module_data$qt50[1], linetype = "dashed", size = 1, color = 'grey', alpha = 0.8)+
    # add slightly larger points behind, so get black outlines
    geom_point(aes(shape = Performance, fill = Performance), size = 4, color = "transparent")+  # changed from 5 to 4 
    # # adding reference line, CSCI = 0.79
    geom_hline(yintercept = 0.79, linetype = "dashed", size = 0.5)+
    
    # setting Expected data range, will change if add new data
    # date range is geneally 2000-2019
    # will later want to make date range not manually set
    scale_x_discrete(limits = c(2000:2025))+
    # setting csci breaks and limits
    scale_y_continuous(limits = c(0, 1.2), breaks = c(0, 0.4, 0.8, 1.2))+
    expand_limits(x = c(2000, 2025))+
    # shape and color specifications
    # blue and red color choices, looked up some color-blind friendly palette
    scale_fill_manual(name = "Performance", values = setNames(c('#1F78B4', '#1F78B4', '#E31A1C', 'azure4'),
                                                              c("Over Scoring", "Expected", "Under Scoring", "Indeterminate")))+
    scale_shape_manual(name = "Performance", values = setNames(c(24, 21, 25, 21),
                                                               c("Over Scoring", "Expected", "Under Scoring", 'Indeterminate')))+
    ylab("CSCI Score")+
    theme_pubr(legend = 'right')+
    theme(axis.title.x = element_blank())
  
  # want long format for module results
  # to deal with replicates: only display results for max csci score per sampledate
  csci_module_data_2 <- LOE_mod_sum_df %>% 
    filter(test_site == my_site) %>%   # edited by Rachel 10/24
    mutate(year = year(sampledate)) %>% 
    # keep only max value result per year
    group_by(year) %>% 
    slice(which.max(csci)) %>% 
    ungroup() %>%
    transmute(test_site, year, `Elevated Conductivity` = conductivity, Eutrophication = eutrophication, `Altered Habitat` = habitat, `Elevated Temperature` = temperature) %>%
    # select(test_site, year, Conductivity, Eutrophication, Habitat, Temperature) %>% 
    pivot_longer(-c(test_site, year), names_to = "Module", values_to = "Result") %>%
    ### changing text in table to match newer versions requested by Rafi and David - edited by rachel 1/6
    mutate(Result = ifelse(Result == "Passing CSCI", "CSCI > 0.79", Result))
  
  # ordering module results
  csci_module_data_2$Result <- factor(csci_module_data_2$Result, c("Likely Cause", "Unlikely Cause", "Indeterminate Cause", "CSCI > 0.79", "Cannot be Evaluated"))
  
  # colorblind friendly palette, matching module results to colors
  # so all levels will appear in legend with the same color matches, even if don't all appear in each dataset
  # cbPalette <- c("Likely Cause"= "#E69F00", "Unlikely Cause"= "#56B4E9", "Indeterminate Cause"= "#009E73","NA" = "#999999")
  cbPalette <- c("Likely Cause"= "#E31A1C", "Unlikely Cause"= "#1F78B4", "Indeterminate Cause"= "#009c00","CSCI > 0.79" = "#d6ac38", "Cannot be Evaluated" = "#c9c9c9")
  
  # plotting module summaries over time for each site
  site_module_plot <-ggplot(data = csci_module_data_2,
                            aes(x = year, y = Module))+
    geom_tile(aes(fill = Result), color = 'white', width = 0.8, height = 0.8)+
    # setting expected data range, will change if add new data
    # date range
    scale_x_discrete(limits = c(2000:2025))+
    expand_limits(x = c(2000, 2025))+
    # using colorblind friendly palette instead, with matched module results set above in cbPalette
    scale_fill_manual(values = cbPalette, limits = names(cbPalette))+
    # scale_fill_brewer(palette = "Pastel1")+
    ylab("")+ # edited by Rachel 1/11/23
    xlab("Year")+
    labs(fill = "Module Summary")+
    theme_pubr(legend = 'right')+
    # manually set legend sizes so consistent across all plots
    # theme(legend.title = element_text(size = 20),
    #       legend.text = element_text(size = 16))+
    theme(axis.title.x = element_blank())
  
  combined_plot <- ggarrange(site_csci_plot, site_module_plot,
                             nrow = 2,
                             align = "hv")
  print(combined_plot)
  
  # Save plot in the correct folder
  ggsave(
    file = file.path(output_dir, paste0("1.0_Module_Summary_", my_site, ".jpeg")),
    plot = site_csci_plot,
    width = 14, height = 9, units = "in"
  )
}

site_csci_module_plotter_out <- purrr::map(my_input_test_sites, 
                                           ~site_csci_module_plotter(.x, file.path(output_base_dir, .x))
) ## mapping a function, applies a function across multiple different items within a list

#### part 2, LOE Summary per Module ####

site_loe_plotter <- function(my_site, output_dir) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  
  # prepping data
  loe_module_data <- LOE_sum_df %>% 
    filter(test_site == my_site) %>% 
    mutate(year = year(sampledate), Module = case_when(
      module == "Conductivity"~"Elevated Conductivity", 
      module == "Habitat"~"Altered Habitat", 
      module == "Temperature"~"Elevated Temperature", T ~ module)) %>% 
    # retain results for just max csci replicates
    # so need to add csci data
    left_join(csci_base_df %>% select(masterid,sampledate, collectionmethodcode, fieldreplicate, csci),
              by = c("test_site" = "masterid", "sampledate" = "sampledate", "collectionmethodcode" = "collectionmethodcode", "fieldreplicate" = "fieldreplicate")) %>% 
    group_by(year,Module,loe) %>%  
    slice(which.max(csci)) %>% 
    ungroup() %>% 
    rename(LOE = loe, Score = score) %>%
    ### changing text in table to match newer versions requested by Rafi and David - edited by rachel 1/6
    mutate(Score = ifelse(Score == "Passing CSCI", "CSCI > 0.79", Score))
  # pivot_longer(c("Spatial.Temporal", "Stressor.Response", "Reference.Condition"), names_to = "LOE", values_to = "Score") %>% 
  # mutate(LOE = case_when(LOE == "Spatial.Temporal" ~ "Spatial Co-Occurrence ",
  #                        LOE == "Stressor.Response" ~ "Stressor Response",
  #                        LOE == "Reference.Condition" ~ "Reference Condition"))
  
  # ordering module loe results
  loe_module_data$Score <- factor(loe_module_data$Score, c("Supporting", "Weakening", "Indeterminate", "CSCI > 0.79", "No Test Data", "No Evidence"))
  # colorblind friendly palette, matching module results to colors
  # so all levels will appear in legend with the same color matches, even if don't all appear in each dataset
  cbPalette_v2 <- c("Supporting"= "#E31A1C", "Weakening"= "#1F78B4", "Indeterminate"= "#009c00", "CSCI > 0.79" = "#d6ac38", "No Test Data" = "#c9c9c9", "No Evidence" = "#969696")
  
  # iterate through modules 
  # create the list of modules to iterate through
  my_module_list <- c("Elevated Conductivity", "Eutrophication", "Altered Habitat", "Elevated Temperature")
  
  # individual plot per module per site, as function
  my_module <- "Elevated Temperature" # for testing code 
  module_plotter <- function(my_module){
    
    # retain module of interest
    module_data <- loe_module_data %>% 
      filter(Module == my_module)
    
    # plot lines of evidence results over time
    ggplot(data = module_data,
           aes(x = year, y = LOE))+
      geom_tile(aes(fill = Score), color = 'white', width = 0.8, height = 0.8)+
      # setting expected data range, will change if add new data
      scale_x_discrete(limits = c(2000:2025))+
      expand_limits(x = c(2000, 2025))+
      # using colorblind friendly palette instead, with matched module results set above in cbPalette
      scale_fill_manual(values = cbPalette_v2, limits = names(cbPalette_v2))+
      # scale_fill_brewer(palette = "Pastel1")+
      xlab("Year")+
      ylab("Line of Analysis")+
      labs(fill = "Score")+
      # manually set legend sizes so consistent across all plots
      # theme(legend.title = element_text(size = 20),
      #       legend.text = element_text(size = 16))+
      theme_pubr(legend = 'right')+
      # no longer want axis label for yeae
      theme(axis.title.x = element_blank())
  }
  
  # iterate function across list of modules, per site
  module_plotter_out <- purrr::map(.x = my_module_list, ~module_plotter(.x))
  
  
  # Save plots in the correct folder
  ggsave(module_plotter_out[[1]], file = file.path(output_dir, paste0("1.1_Module_LOAs_Conductivity_", my_site, ".jpeg")), width = 14, height = 4, units = "in")
  ggsave(module_plotter_out[[2]], file = file.path(output_dir, paste0("1.1_Module_LOAs_Eutrophication_", my_site, ".jpeg")), width = 14, height = 4, units = "in")
  ggsave(module_plotter_out[[3]], file = file.path(output_dir, paste0("1.1_Module_LOAs_Habitat_", my_site, ".jpeg")), width = 14, height = 4, units = "in")
  ggsave(module_plotter_out[[4]], file = file.path(output_dir, paste0("1.1_Module_LOAs_Temperature_", my_site, ".jpeg")), width = 14, height = 4, units = "in")
}

site_loe_plotter_out <- purrr::map(my_input_test_sites, 
                                   ~site_loe_plotter(.x, file.path(output_base_dir, .x))
)

