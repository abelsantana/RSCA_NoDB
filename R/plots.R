# Secondary plotting script - need for complete data generation - gets pulled in by functions in plumber 
# Functions and loops to generate additional datasets, needed for Dashboard - but also for SMC data 
# These functions require 0.2_RSCA_Core.R to already be run, on same input test sites - creates the functions and data to be generated into tables in SMC database 
# ^ To make tables run main.R

LOE_palette <- c("Supporting" = "#E31A1C", "Weakening"="#1F78B4", "Indeterminate"="#009c00", "CSCI > 0.79"="#d6ac38",
                 "No Test Data"="#c9c9c9", "No Evidence" = "grey60") # legend
 

#### SPATIAL TEMPORAL LOE GRAPHIC ####

# boxplots of stressor data, per site/sample/module

# SR_LOE_df
spatial.co.plot <- function(sco_dat, sco_loe, sampleid, mod){
  # assigning colors based on final SR categories: Supporting, Weakening, No Test Data, Indeterminate, CSCI > 0.79
  
  ggplot(
    data = sco_dat,
    mapping = aes(y = comp_result))+
    geom_boxplot(width = 0.2)+
    geom_hline(data = sco_loe, 
               aes(yintercept = test_result, color = sco_score),        ## colored = test_result and the color of the line is based on "score" 
               key_glyph = draw_key_label, size = 1, linetype = 2)+ 
    scale_color_manual(values = LOE_palette, limits = names(LOE_palette))+
    facet_wrap(~analytename, scales = "free")+
    labs(y = "Result")+
    labs(color = "Spatial Co-Occurence Score")+
    theme_bw()+
    guides(
      shape = guide_legend(override.aes = list(size = 3)),
      color = guide_legend(override.aes = list(size = 3))
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      legend.position = "right",
      legend.title = element_text(size=7),
      legend.text = element_text(size=5)
    )
}


#### REFERENCE CONDITION COMPARISON LOE GRAPHIC ####

# boxplots of stressor data, per site/sample/module
# RCC_LOE_df

ref.cond.plot <- function(rcc_dat, rcc_loe, sampleid, mod) {
  ggplot(
    data = rcc_dat,
    mapping = aes(y = comp_result)
  ) +
    geom_boxplot(width = 0.2)+
    geom_hline(data = rcc_loe, 
               aes(yintercept = test_result, color = rcc_score), 
               key_glyph = draw_key_label, size = 1, linetype = 2) +
    scale_color_manual(values = LOE_palette, limits = names(LOE_palette)) +
    facet_wrap(~analytename, scales = "free") +
    labs(y = "Result") +
    labs(color = "Reference Condition Score")+
    theme_bw()+
    guides(
      shape = guide_legend(override.aes = list(size = 3)),
      color = guide_legend(override.aes = list(size = 3))
    ) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "right",
      legend.title = element_text(size=7),
      legend.text = element_text(size=5)
    )
}



#### STRESSOR RESPONSE-LOGISTIC LOE GRAPHIC ####

# SR_log_LOE_df
stress.resp.plot <- function(sr_log_dat, sr_log_loe, sampleid, mod){
  ggplot(data = sr_log_dat, aes(x = comp_result, y = cond2_ref_y))+
    geom_smooth(formula = y ~ x, method = "glm", method.args=list(family="binomial"), se=T, level=0.8, fill="#99CCFF")+
    geom_point(shape=21, size=1.5, fill="#666666")+
    facet_wrap(~analytename, scales = "free_x", nrow = 3)+
    labs(y="Probability of Poor CSCI Score", x="Stressor Value")+
    geom_vline(data = sr_log_loe, 
               aes(xintercept=test_result*1, color = sr_score), key_glyph = draw_key_label, linetype=2)+
    geom_hline(yintercept = 0.6, color="#8c8c8c", linetype=1)+
    geom_hline(yintercept=0.4, color="#8c8c8c", linetype=1)+
    scale_color_manual(values = LOE_palette, limits = names(LOE_palette))+
    scale_y_continuous(limits = c(0,1), breaks=c(0, 0.5, 1))+
    labs(color = "Stressor Response Score")+
    theme_bw()+
    guides(
      shape = guide_legend(override.aes = list(size = 3)),
      color = guide_legend(override.aes = list(size = 3))
    ) +
    theme(
      panel.grid=element_blank(),
      legend.position = "right",
      legend.title = element_text(size=7),
      legend.text = element_text(size=5)
    ) 
}

