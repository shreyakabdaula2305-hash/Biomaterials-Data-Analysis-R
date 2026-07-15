# =============================================================================
# Project      : Biomaterials Data Analysis in R
# Analysis     : Porosity Analysis
# Author       : Shreya Kabdaula
#
# Description:
# This script performs statistical analysis and visualization
# of scaffold porosity for hydrogel and cryogel biomaterials.
#
# Workflow:
# 1. Import experimental data
# 2. Calculate scaffold porosity
# 3. Generate publication-quality figure
# 4. Perform Welch's t-test
# 5. Export processed results
# =============================================================================

# -----------------------------------------------------------------
# NOTE:
# The experimental dataset is not included in this repository
# because it forms part of ongoing unpublished research.
# The script demonstrates the complete analysis workflow and
# can be reproduced using datasets with the same structure.
# -----------------------------------------------------------------

#=========================Loading Packages====================================
# Install these packages once if required:
# install.packages(c("readxl","dplyr","ggplot2","writexl"))

library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)

#=====================Import Data==========================================
porosity <- read_excel("data/porosity.xlsx")

#=====================Data Processing===========================================

# Calculate scaffold volume, pore volume, and porosity percentage for each sample.
porosity <- porosity |>
  mutate(
    Radius_mm = Diameter_mm / 2,
    Scaffold_Volume_mm3 = pi * Radius_mm^2 * Height_mm,
    Pore_Volume_mm3 = (Wet_Weight_mg - Dry_Weight_mg) / 0.789,
    Porosity_Percent = (Pore_Volume_mm3 / Scaffold_Volume_mm3) * 100)

#======================Summary Statistics=====================================
# Calculate the mean, standard deviation (SD),sample size (n), and standard error (SEM)
porosity_summary <- porosity |>
  group_by(Material) |>
  summarise(
    Mean_Porosity = mean(Porosity_Percent),
    SD = sd(Porosity_Percent),
    n=n(),
    SEM= SD / sqrt(n)
  )

#===================Data Visualization======================================
# Generate a publication-quality bar graph showing the mean porosity 
porosity_plot <- ggplot(
  porosity_summary,
  aes(
    x = Material,
    y = Mean_Porosity,
    fill = Material)
  )+
  geom_col(width = 0.3)+
  geom_errorbar(
    aes(
      ymin = Mean_Porosity - SEM,
      ymax = Mean_Porosity + SEM),
    width = 0.2,
    linewidth = 0.8
  ) +
  geom_point(
    data = porosity,
    aes(
      x = Material,
      y = Porosity_Percent
    ),
    size = 2,
    colour = "black",
    inherit.aes = FALSE
  )+
  scale_fill_manual(
    values = c(
      "Cryogel" = "#0072B2",
      "Hydrogel" = "#D55E00"
    )
  )+
  labs(
    title = "Scaffold Porosity",
    x = "Material",
    y = "Porosity %",
    fill = "Material"
  )+
  scale_y_continuous(
    breaks = seq(0,70, by = 5)
  )+
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
      colour = "black",
      face = "bold"),
    
    legend.position = "none",
    
    axis.line = element_line(linewidth = 1),
    
    axis.ticks = element_line(linewidth=0.5),
    
    axis.ticks.length = unit(0.25,"cm")
  )

#==========================Export Figure=================================
# Save the graph as a high-resolution PNG image.
ggsave(
  filename="figures/porosity_plot.png",
  plot = porosity_plot,
  width = 8,
  height = 6,
  dpi = 300)

#=========================Welch's t-test=======================================
# Compare scaffold porosity between cryogel and hydrogel
porosity_ttest <- t.test(
  Porosity_Percent ~ Material,
  data = porosity
)

# Organize the t-test results into a summary table.
porosity_statistics <- data.frame(
  parameter = c(
    "Statistical Test", 
    "t statistic", 
    "Degrees of Freedom", 
    "p-value", 
    "95% CI Lower", 
    "95% CI Upper"),
  Value = c(
  "Welch's Two Sample t-test",
  round(porosity_ttest$statistic, 4),
  round(porosity_ttest$parameter,4),
  round(porosity_ttest$p.value,4),
  round(porosity_ttest$conf.int[1],2),
  round(porosity_ttest$conf.int[2],2)
))

# Generate a concise interpretation of the statistical results
porosity_report <- data.frame(
  Parameter = c(
    "Statistical Test",
    "Mean Cryogel (%)",
    "Mean Hydrogel (%)",
    "p-value",
    "Significant",
    "Conclusion"
  ),
  Result = c("Welch's Two Sample t-test",
             round(
               porosity_summary$Mean_Porosity[
                 porosity_summary$Material == "Cryogel"],2),
             round(
               porosity_summary$Mean_Porosity[
               porosity_summary$Material == "Hydrogel"],2),
             round(porosity_ttest$p.value,4),
             ifelse(
               porosity_ttest$p.value < 0.05,
               "Yes",
               "No"
             ),
             ifelse(
               porosity_ttest$p.value < 0.05,
               "Cryogel showed significantly higher porosity than Hydrogel",
               "No statistically significant difference"
             ))
)

#=========================Export Results============================
# Export processed data and statistical summaries to Excel
write_xlsx(
  list(
    "Processed_Data" = porosity,
    "Summary" = porosity_summary,
    "Statistics" = porosity_statistics,
    "Report" = porosity_report
  ),
  path = "results/porosity_analysis.xlsx"
)


