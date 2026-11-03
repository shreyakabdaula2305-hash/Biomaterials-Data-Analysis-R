#===============================================================
# Project      : Biomaterials Data Analysis in R
# Analysis     : Swelling Analysis
# Author       : Shreya Kabodula
#
# Description:
# This script performs statistical analysis and visualization 
#of swelling behaviour in hydrogel and cryogel biomaterials.
#
# Workflow:
# 1. Import experimental data
# 2. Calculate summary statistics
# 3. Generate publication-quality figures
# 4. Perform Welch's t-tests
# 5. Perform two-way ANOVA
# 6. Export figures and statistical results
#==============================================================================

# -----------------------------------------------------------------
# NOTE:
# The experimental dataset is not included in this repository
# because it forms part of ongoing unpublished research.
# The script demonstrates the complete analysis workflow and
# can be reproduced using datasets with the same structure.
# -----------------------------------------------------------------

#=========================Loading Packages====================================
# Install these packages once if required:
# install.packages(c("readxl","dplyr","ggplot2","tidyr","writexl"))

library(readxl)
library(dplyr)
library(ggplot2)
library(grid)
library(readr)
library(writexl)

#========================Import Data======================================
swelling <- read_excel("data/swelling.xlsx")

#========================Summary Statistics===============================

# Calculate the mean, standard deviation (SD), sample size (n),
# and standard error of the mean (SEM) for each material at each time point.

swelling_summary <- swelling |>
  group_by(Material,Time_hr) |>
  summarise(
    Mean_Swelling = mean(Swelling_percent),
    SD = sd(Swelling_percent),
    n = n(),
    SEM = SD / sqrt(n)
  )

#========================Data Visualization===============================
# Generate a publication-quality line graph showing the swelling
# behaviour of hydrogel and cryogel over time.

swelling_plot <- ggplot(
  data = swelling_summary,
  aes(x= Time_hr,
      y = Mean_Swelling,
      color = Material)
  ) +
  geom_point(size = 3) +
  geom_line(linewidth = 1) + 
  geom_errorbar(
    aes(
      ymin = Mean_Swelling - SEM,
      ymax = Mean_Swelling + SEM
    ),
    width = 0.3
  ) +
  labs(
    title = "Swelling Behaviour",
    x = "Time (hours)",
    y = "Swelling (%)",
    color = "Material"
  ) +
  scale_y_continuous(
    breaks = seq(200,1800, by = 200)
  )+
  scale_colour_manual(
    values = c(
      "Cryogel" = "#0072B2",
      "Hydrogel" = "#D55E00"
    ) 
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      size = 16,
      face = "bold"),
    
    axis.title = element_text(
      size = 14,
      face = "bold"),
    
    axis.text = element_text(
      size = 12,
      colour = "black"),
    
    legend.title = element_text(
      size = 12,
      face = "bold"),
    
    legend.text = element_text(size = 11),
    
    legend.position = "top",
    
    axis.line = element_line(linewidth = 1),
    
    axis.ticks = element_line(linewidth=1),
    
    axis.ticks.length = unit(0.25,"cm")
    )

#=======================Export Figure==============================
# Save the graph as a high-resolution PNG image.

ggsave(
  filename="figures/swelling_plot.png",
  plot = swelling_plot,
  width = 8,
  height = 6,
  dpi = 300)

#=====================Welch's t-test============================== 
# Create an empty table to store statistical results from each individual time point.

swelling_ttest_results <- data.frame()

# Define the experimental time points for comparison.
time_points <- c(1,2,4,6,8,24)

# Perform an independent Welch's t-test at each time point to compare swelling between cryogel and hydrogel.
for(current_time in time_points){
  
  # Extract data corresponding to the current experimental time point.
  swelling_current <- swelling |>
    filter(Time_hr == current_time)
  
  # Compare swelling percentages between the two biomaterials.
  swelling_ttest <- t.test(
    Swelling_percent ~ Material,
    data = swelling_current
  )
  
  # Calculate the mean swelling for each material.
  swelling_mean_cryogel <- mean(
    swelling_current$Swelling_percent[
      swelling_current$Material == "Cryogel"
    ]
  )
  
  swelling_mean_hydrogel <- mean(
    swelling_current$Swelling_percent[
      swelling_current$Material == "Hydrogel"
    ]
  )
  
  # Store the statistical results for the current time point.
  swelling_new_row <- data.frame(
    Time_hr = current_time,
    Mean_Cryogel = round(swelling_mean_cryogel, 2),
    Mean_Hydrogel = round(swelling_mean_hydrogel, 2),
    p_value = round(swelling_ttest$p.value, 4),
    Significance = ifelse(
      swelling_ttest$p.value < 0.0001, "****",
    ifelse(swelling_ttest$p.value < 0.001, "***",
           ifelse(swelling_ttest$p.value < 0.01, "**",
                  ifelse(swelling_ttest$p.value < 0.05, "*", "ns")))),
    Interpretation = ifelse(
      swelling_ttest$p.value < 0.0001, "Extremely Significant",
      ifelse(swelling_ttest$p.value < 0.001, "Highly Significant",
             ifelse(swelling_ttest$p.value < 0.01, "Very Significant",
                    ifelse(swelling_ttest$p.value < 0.05, "Significant", "Not Significant")))
    ))
  swelling_ttest_results <- rbind(swelling_ttest_results, swelling_new_row)
}

#===========================================ANOVA test=========================================
# Convert time into a categorical variable and evaluate the effects of material, time, and their interaction.

swelling$Time_hr <- as.factor(swelling$Time_hr)

# Evaluate the effects of material, time,and their interaction on swelling behaviour.
swelling_anova <- aov(Swelling_percent ~ Material * Time_hr,
                       data = swelling)
swelling_anova_summary <- summary(swelling_anova)

# Organize the ANOVA results into a publication-ready summary table.
swelling_anova_results <- data.frame(
  Effect = rownames(swelling_anova_summary[[1]]),
  Df = swelling_anova_summary[[1]][,"Df"],
  Sum_sq = round(swelling_anova_summary[[1]][,"Sum Sq"],2),
  Mean_sq = round(swelling_anova_summary[[1]][,"Mean Sq"],2),
  F_value = round(swelling_anova_summary[[1]][, "F value"],3),
  p_value = swelling_anova_summary[[1]][,"Pr(>F)"])
  
swelling_anova_results$Significance <- ifelse(
  is.na(swelling_anova_results$p_value), NA,
  ifelse(swelling_anova_results$p_value < 0.0001, "****",
         ifelse(swelling_anova_results$p_value < 0.001, "***",
                ifelse(swelling_anova_results$p_value < 0.01, "**",
                       ifelse(swelling_anova_results$p_value < 0.05, "*", "ns")
                )
         )
  ))
swelling_anova_results$Interpretation <- ifelse(
  is.na(swelling_anova_results$p_value), NA,
  ifelse(
    swelling_anova_results$p_value < 0.05,
    "Significant",
    "Not Significant"
  )
)
#===================================Export Results==============================
# Export processed data and statistical summaries to Excel.
write_xlsx(
  list(
    "Data" = swelling,
    "Summary" = swelling_summary,
    "T_Test_Results" = swelling_ttest_results,
    "ANOVA_Results" = swelling_anova_results
  ),
  path = "results/swelling_analysis.xlsx"
)

