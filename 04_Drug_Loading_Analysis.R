# =============================================================================
# Project      : Biomaterials Data Analysis in R
# Analysis     : Drug Loading Efficiency
# Author       : Shreya Kabodula
#
# Description:
# This script performs statistical analysis and visualization
# of drug loading efficiency in hydrogel and cryogel biomaterials.
#
# Workflow:
# 1. Import experimental data
# 2. Generate standard curve
# 3. Calculate drug loading efficiency
# 4. Generate publication-quality figures
# 5. Perform Welch's t-tests
# 6. Perform Two-Way ANOVA
# 7. Export processed results
# =============================================================================

# -----------------------------------------------------------------
# NOTE:
# The experimental dataset is not included in this repository
# because it forms part of ongoing unpublished research.
# The script demonstrates the complete analysis workflow and
# can be reproduced using datasets with the same structure.
# -----------------------------------------------------------------

#==============================Load Packages===================================
# Install these packages once if required:
# install.packages(c("readxl","ggplot2","writexl","dplyr"))
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)

#==========================Import Data===========================
# Import the experimental drug loading dataset and
# the corresponding standard curve data.

drug_loading <- read_excel(
  "data/Drug_Loading.xlsx",
  sheet = "drug_loading"
)

standard_curve <- read_excel(
  "data/Drug_Loading.xlsx",
  sheet = "standard_curve"
)
#========================Standard Curve==========================
# Fit a linear regression model to generate the standard curve used for concentration calculations.

standard_model <- lm(
  Absorbance_238nm ~ Standard_Concentration_mg_mL,
  data = standard_curve
)

intercept <- coef(standard_model)[1]
slope <- coef(standard_model)[2]
R_squared <- summary(standard_model)$r.squared

# Create the regression equation and R² text for the figure

equation <- paste0(
  "y = ",
  round(slope, 4),
  "x ",
  ifelse(intercept < 0, "-", "+"),
  " ",
  abs(round(intercept, 4))
)

r2_text <- paste0(
  "R² = ",
  round(R_squared, 4)
)

# Generate the publication-quality standard curve.

standard_curve_plot <- ggplot(
  standard_curve,
  aes(
    x = Standard_Concentration_mg_mL,
    y = Absorbance_238nm
  )
) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  annotate(
    "text",
    x = 3,
    y = 0.15,
    label = paste(equation, r2_text, sep = "\n"),
    hjust = 0,
    size = 5
  ) +
  labs(
    title = "Standard Curve",
    x = "Standard Concentration (mg/mL)",
    y = "Absorbance at 238 nm"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Export the standard curve figure
ggsave(
  filename = "figures/standard_curve.png",
  plot = standard_curve_plot,
  width = 6,
  height = 5,
  dpi = 600
)

# Store the regression parameters obtained from the standard curve
standard_curve_results <- data.frame(
  Parameters = c(
    "Slope",
    "Intercept",
    "R_squared"
  ),
  Value = c(
    round(slope,4),
    round(intercept,4),
    round(R_squared,4)
  )
)

#==================Drug Loading Calculation======================
# Calculate the amount of drug added,remaining drug, drug loaded,and loading efficiency for each sample

supernatant_volume <- 3

drug_loading_analysis <- drug_loading |>
  mutate(
    Drug_Added_mg =
      Drug_Concentration_mg_ml * supernatant_volume,
    Remaining_Concentration_mg_mL =
      (Absorbance_238nm - intercept) / slope,
    Remaining_Drug_mg = 
      Remaining_Concentration_mg_mL * supernatant_volume,
    Drug_Loaded_mg =
      Drug_Added_mg - Remaining_Drug_mg,
    Loading_Efficiency_percent =
      (Drug_Loaded_mg / Drug_Added_mg) * 100
      )

drug_loading_analysis <- drug_loading_analysis |>
  select(
    Material,
    Drug_Concentration_mg_ml,
    Sample_ID,
    Dry_Weight_mg,
    Wet_Weight_mg,
    Absorbance_238nm,
    Drug_Added_mg,
    Drug_Loaded_mg,
    Remaining_Drug_mg,
    Remaining_Concentration_mg_mL,
    Loading_Efficiency_percent
  )

#=====================Summary Statistics=========================
# Calculate summary statistics for each material and drug concentration
drug_loading_summary <- drug_loading_analysis|>
  group_by(
    Material,
    Drug_Concentration_mg_ml
  ) |>
  summarise(
    Mean_Loading = mean(Loading_Efficiency_percent),
    SD = sd(Loading_Efficiency_percent),
    n = n(),
    SEM = SD / sqrt(n)
  )

#=====================Data Visualization=========================
# Generate a publication-quality graph showing drug loading efficiency
drug_loading_graph <- ggplot(
  drug_loading_summary,
  aes(
    x = Drug_Concentration_mg_ml,
    y = Mean_Loading,
    fill = Material
  )
) +
  geom_col(
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = Mean_Loading - SEM,
      ymax = Mean_Loading + SEM
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  )+
  geom_point(
    data = drug_loading_analysis,
    aes(
      x = Drug_Concentration_mg_ml,
      y = Loading_Efficiency_percent,
      fill = Material
    ),
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.8
    ),
    shape = 21,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Drug Loading Efficiency",
    x = "Drug Concentration (mg/ml)",
    y = "Loading Efficiency (%)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(
      size = 12,
      colour = "black"
    ),
    legend.title = element_blank()
  )

# Export the drug loading efficiency figure.
ggsave(
  filename = "figures/drug_loading_graph.png",
  plot = drug_loading_graph,
  width = 6,
  height = 5,
  dpi = 600
)

#=======================Welch's t-test===========================
# Compare drug loading efficiency between cryogel and hydrogel at each concentration.

drug_concentrations <- c(1,2.5,5)

drug_ttest_results <- data.frame()

for (current_concentration in drug_concentrations){
  drug_current <- drug_loading_analysis|>
    filter(Drug_Concentration_mg_ml == current_concentration)
 
  drug_ttest <- t.test(
    Loading_Efficiency_percent ~ Material,
    data = drug_current)
  
  drug_mean_cryogel <- mean(
    drug_current$Loading_Efficiency_percent[
      drug_current$Material == "Cryogel"
    ]
  )
  
  drug_mean_hydrogel <- mean(
    drug_current$Loading_Efficiency_percent[
      drug_current$Material == "Hydrogel"
    ]
  )
  
  drug_new_row <- data.frame(
    Drug_Concentration = current_concentration,
    Mean_Cryogel = round(drug_mean_cryogel,2),
    Mean_Hydrogel = round(drug_mean_hydrogel,2),
    p_value = round(drug_ttest$p.value, 4),
    Significance = ifelse(
      drug_ttest$p.value < 0.0001, "****",
      ifelse(
        drug_ttest$p.value < 0.001, "***",
        ifelse(
          drug_ttest$p.value < 0.01, "**",
          ifelse(
            drug_ttest$p.value < 0.05, "*", "ns"
          )
        )
      )
    ),
    
    Interpretation = ifelse(
      drug_ttest$p.value < 0.0001, "Extremely significant",
      ifelse(
        drug_ttest$p.value < 0.001, "Highly significant",
        ifelse(
          drug_ttest$p.value < 0.01, "Very significant",
          ifelse(
            drug_ttest$p.value < 0.05, "Significant",
            "Not significant"
          )
        )
      )
    )
  )
  drug_ttest_results <- rbind(drug_ttest_results, drug_new_row)
}

drug_loading_analysis <- drug_loading_analysis |>
  mutate(
    Drug_Concentration_mg_ml =
      as.factor(Drug_Concentration_mg_ml)
  )

#=======================Two-Way ANOVA============================

# Evaluate the effects of biomaterial type,drug concentration,and their interaction.

drug_anova <- aov(
  Loading_Efficiency_percent ~ Material * Drug_Concentration_mg_ml,
  data = drug_loading_analysis
)

drug_anova_summary <- summary(drug_anova)

drug_anova_results <- data.frame(
  Effect = rownames(drug_anova_summary[[1]]),
  Df = drug_anova_summary[[1]][, "Df"],
  Sum_sq = round(drug_anova_summary[[1]][, "Sum Sq"], 2),
  Mean_sq = round(drug_anova_summary[[1]][, "Mean Sq"], 2),
  F_value = round(drug_anova_summary[[1]][, "F value"], 3),
  p_value = drug_anova_summary[[1]][, "Pr(>F)"],
  
  Significance = ifelse(
    drug_anova_summary[[1]][, "Pr(>F)"] < 0.0001, "****",
    ifelse(
      drug_anova_summary[[1]][, "Pr(>F)"] < 0.001, "***",
      ifelse(
        drug_anova_summary[[1]][, "Pr(>F)"] < 0.01, "**",
        ifelse(
          drug_anova_summary[[1]][, "Pr(>F)"] < 0.05,
          "*",
          "ns"
        )
      )
    )
  ),
  
  Interpretation = ifelse(
    drug_anova_summary[[1]][, "Pr(>F)"] < 0.0001,
    "Extremely significant",
    ifelse(
      drug_anova_summary[[1]][, "Pr(>F)"] < 0.001,
      "Highly significant",
      ifelse(
        drug_anova_summary[[1]][, "Pr(>F)"] < 0.01,
        "Very significant",
        ifelse(
          drug_anova_summary[[1]][, "Pr(>F)"] < 0.05,
          "Significant",
          "Not significant"
        )
      )
    )
  )
)

#========================Export Results==========================

# Export processed datasets and statistical summaries to an Excel 

write_xlsx(
  list(
    "Drug_Loading_rawdata" = drug_eff,
    "Standard_curve_data" = standard_curve,
    "Standard_curve_results" = standard_curve_results,
    "Drug_Loading_analysis" = drug_loading_analysis,
    "Summary" = drug_loading_summary,
    "T-test" = drug_ttest_results,
    "Anova" = drug_anova_results
    ),
  path = "results/Drug_Loading_Efficiency_Analysis.xlsx"
)

#==============================Load Packages===================================
library(readxl)
library(dplyr)
library(ggplot2)
library(writexl)

#==========================Import Data===========================

# Import the experimental drug loading dataset and
# the corresponding standard curve data.

drug_loading <- read_excel(
  "data/Drug_Loading.xlsx",
  sheet = "drug_loading"
)

standard_curve <- read_excel(
  "data/Drug_Loading.xlsx",
  sheet = "standard_curve"
)
#========================Standard Curve==========================
# Fit a linear regression model to generate the standard curve used for concentration calculations.

standard_model <- lm(
  Absorbance_238nm ~ Standard_Concentration_mg_mL,
  data = standard_curve
)

intercept <- coef(standard_model)[1]
slope <- coef(standard_model)[2]
R_squared <- summary(standard_model)$r.squared

# Create the regression equation and R² text for the figure

equation <- paste0(
  "y = ",
  round(slope, 4),
  "x ",
  ifelse(intercept < 0, "-", "+"),
  " ",
  abs(round(intercept, 4))
)

r2_text <- paste0(
  "R² = ",
  round(R_squared, 4)
)

# Generate the publication-quality standard curve.

standard_curve_plot <- ggplot(
  standard_curve,
  aes(
    x = Standard_Concentration_mg_mL,
    y = Absorbance_238nm
  )
) +
  geom_point(size = 3) +
  geom_smooth(
    method = "lm",
    se = FALSE
  ) +
  annotate(
    "text",
    x = 3,
    y = 0.15,
    label = paste(equation, r2_text, sep = "\n"),
    hjust = 0,
    size = 5
  ) +
  labs(
    title = "Standard Curve",
    x = "Standard Concentration (mg/mL)",
    y = "Absorbance at 238 nm"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12)
  )

# Export the standard curve figure
ggsave(
  filename = "figures/standard_curve.png",
  plot = standard_curve_plot,
  width = 6,
  height = 5,
  dpi = 600
)

standard_curve_results <- data.frame(
  Parameters = c(
    "Slope",
    "Intercept",
    "R_squared"
  ),
  Value = c(
    round(slope,4),
    round(intercept,4),
    round(R_squared,4)
  )
)

#==================Drug Loading Calculation======================
# Calculate the amount of drug added,remaining drug, drug loaded,and loading efficiency for each sample

supernatant_volume <- 3

drug_loading_analysis <- drug_loading |>
  mutate(
    Drug_Added_mg =
      Drug_Concentration_mg_ml * supernatant_volume,
    Remaining_Concentration_mg_mL =
      (Absorbance_238nm - intercept)/ slope,
    Remaining_Drug_mg = 
      Remaining_Concentration_mg_mL* supernatant_volume,
    Drug_Loaded_mg =
      Drug_Added_mg - Remaining_Drug_mg,
    Loading_Efficiency_percent =
      (Drug_Loaded_mg / Drug_Added_mg)* 100
      )

drug_loading_analysis <- drug_loading_analysis |>
  select(
    Material,
    Drug_Concentration_mg_ml,
    Sample_ID,
    Dry_Weight_mg,
    Wet_Weight_mg,
    Absorbance_238nm,
    Drug_Added_mg,
    Drug_Loaded_mg,
    Remaining_Drug_mg,
    Remaining_Concentration_mg_mL,
    Loading_Efficiency_percent
  )

#=====================Summary Statistics=========================
# Calculate summary statistics for each material and drug concentration
drug_loading_summary <- drug_loading_analysis|>
  group_by(
    Material,
    Drug_Concentration_mg_ml
  ) |>
  summarise(
    Mean_Loading = mean(Loading_Efficiency_percent),
    SD = sd(Loading_Efficiency_percent),
    n = n(),
    SEM = SD / sqrt(n)
  )

#=====================Data Visualization=========================
# Generate a publication-quality graph showing drug loading efficiency
drug_loading_graph <- ggplot(
  drug_loading_summary,
  aes(
    x = Drug_Concentration_mg_ml,
    y = Mean_Loading,
    fill = Material
  )
) +
  geom_col(
    position = position_dodge(width=0.8),
    width = 0.7
  ) +
  geom_errorbar(
    aes(
      ymin = Mean_Loading - SEM,
      ymax = Mean_Loading + SEM
    ),
    position = position_dodge(width = 0.8),
    width = 0.2
  )+
  geom_point(
    data = drug_loading_analysis,
    aes(
      x = Drug_Concentration_mg_ml,
      y = Loading_Efficiency_percent,
      fill = Material
    ),
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.8
    ),
    shape = 21,
    size = 2,
    inherit.aes = FALSE
  ) +
  labs(
    title = "Drug Loading Efficiency",
    x = "Drug Concentration (mg/ml)",
    y = "Loading Efficiency (%)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(
      hjust = 0.5,
      face = "bold",
      size = 16
    ),
    axis.title = element_text(size = 14),
    axis.text = element_text(
      size = 12,
      colour = "black"
    ),
    legend.title = element_blank()
  )

# Export the drug loading efficiency figure.
ggsave(
  filename = "figures/drug_loading_graph.png",
  plot = drug_loading_graph,
  width = 6,
  height = 5,
  dpi = 600
)

#=======================Welch's t-test===========================
# Compare drug loading efficiency between cryogel and hydrogel at each concentration.

drug_concentrations <- c(1,2.5,5)

drug_ttest_results <- data.frame()

for (current_concentration in drug_concentrations){
  drug_current <- drug_loading_analysis|>
    filter(Drug_Concentration_mg_ml == current_concentration)
 
  drug_ttest <- t.test(
    Loading_Efficiency_percent ~ Material,
    data = drug_current)
  
  drug_mean_cryogel <- mean(
    drug_current$Loading_Efficiency_percent[
      drug_current$Material == "Cryogel"
    ]
  )
  
  drug_mean_hydrogel <- mean(
    drug_current$Loading_Efficiency_percent[
      drug_current$Material == "Hydrogel"
    ]
  )
  
  drug_new_row <- data.frame(
    Drug_Concentration = current_concentration,
    Mean_Cryogel = round(drug_mean_cryogel,2),
    Mean_Hydrogel = round(drug_mean_hydrogel,2),
    p_value = round(drug_ttest$p.value, 4),
    Significance = ifelse(
      drug_ttest$p.value < 0.0001, "****",
      ifelse(
        drug_ttest$p.value < 0.001, "***",
        ifelse(
          drug_ttest$p.value < 0.01, "**",
          ifelse(
            drug_ttest$p.value < 0.05, "*", "ns"
          )
        )
      )
    ),
    
    Interpretation = ifelse(
      drug_ttest$p.value < 0.0001, "Extremely significant",
      ifelse(
        drug_ttest$p.value < 0.001, "Highly significant",
        ifelse(
          drug_ttest$p.value < 0.01, "Very significant",
          ifelse(
            drug_ttest$p.value < 0.05, "Significant",
            "Not significant"
          )
        )
      )
    )
  )
  drug_ttest_results <- rbind(drug_ttest_results, drug_new_row)
}

drug_loading_analysis <- drug_loading_analysis |>
  mutate(
    Drug_Concentration_mg_ml =
      as.factor(Drug_Concentration_mg_ml)
  )

#=======================Two-Way ANOVA============================

# Evaluate the effects of biomaterial type,drug concentration,and their interaction.

drug_anova <- aov(
  Loading_Efficiency_percent ~ Material * Drug_Concentration_mg_ml,
  data = drug_loading_analysis
)

drug_anova_summary <- summary(drug_anova)

drug_anova_results <- data.frame(
  Effect = rownames(drug_anova_summary[[1]]),
  Df = drug_anova_summary[[1]][, "Df"],
  Sum_sq = round(drug_anova_summary[[1]][, "Sum Sq"], 2),
  Mean_sq = round(drug_anova_summary[[1]][, "Mean Sq"], 2),
  F_value = round(drug_anova_summary[[1]][, "F value"], 3),
  p_value = drug_anova_summary[[1]][, "Pr(>F)"],
  
  Significance = ifelse(
    drug_anova_summary[[1]][, "Pr(>F)"] < 0.0001, "****",
    ifelse(
      drug_anova_summary[[1]][, "Pr(>F)"] < 0.001, "***",
      ifelse(
        drug_anova_summary[[1]][, "Pr(>F)"] < 0.01, "**",
        ifelse(
          drug_anova_summary[[1]][, "Pr(>F)"] < 0.05,
          "*",
          "ns"
        )
      )
    )
  ),
  
  Interpretation = ifelse(
    drug_anova_summary[[1]][, "Pr(>F)"] < 0.0001,
    "Extremely significant",
    ifelse(
      drug_anova_summary[[1]][, "Pr(>F)"] < 0.001,
      "Highly significant",
      ifelse(
        drug_anova_summary[[1]][, "Pr(>F)"] < 0.01,
        "Very significant",
        ifelse(
          drug_anova_summary[[1]][, "Pr(>F)"] < 0.05,
          "Significant",
          "Not significant"
        )
      )
    )
  )
)

#========================Export Results==========================

# Export processed datasets and statistical summaries to an Excel 

write_xlsx(
  list(
    "Drug_Loading_rawdata" = drug_loading,
    "Standard_curve_data" = standard_curve,
    "Standard_curve_results" = standard_curve_results,
    "Drug_Loading_analysis" = drug_loading_analysis,
    "Summary" = drug_loading_summary,
    "T-test" = drug_ttest_results,
    "Anova" = drug_anova_results
    ),
  path = "results/Drug_Loading_Efficiency_Analysis.xlsx"
)
