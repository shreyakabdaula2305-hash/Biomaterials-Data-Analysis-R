#===============================================================
# Project      : Biomaterials Data Analysis in R
# Analysis     : Rheological Characterization
# Author       : Shreya Kabodula
#
# Description:
# This script generates publication-quality frequency sweep
# plots for hydrogel and cryogel biomaterials using rheological measurements
# 
#Workflow:
# 1. Import experimental data
# 2. Generate storage modulus plot
# 3. Generate loss modulus plot
# 4. Generate combined frequency sweep plot
# 5. Export figures
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
# install.packages(c("readxl","ggplot2","tidyr",))
library(readxl)
library(ggplot2)
library(tidyr)

#==========================Import Data===========================
# Import the experimental rheology dataset
rheology <- read_excel("data/rheology.xlsx")

#====================Storage Modulus Plot========================
# Generate a publication-quality graph showing the
# storage modulus (G') as a function of angular frequency
storage_graph <- ggplot(
  rheology,
  aes(
    x = Angular_Frequency_rad_s,
    y = Storage_Modulus_Pa,
    colour = Material,
    group = Material
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Storage Modulus (G')",
    x = "Angular Frequency (rad/s)",
    y = "Storage Modulus (Pa)"
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

# Export the storage modulus figure
ggsave(
  "figures/storage_modulus_graph.png",
  plot = storage_graph,
  width = 6,
  height = 5,
  dpi = 600
)

#===============================Loss Modulus Plot=============================
# Generate a publication-quality graph showing the
# loss modulus (G'') as a function of angular frequency.
loss_graph <- ggplot(
  rheology,
  aes(
    x = Angular_Frequency_rad_s,
    y = Loss_Modulus_Pa,
    colour = Material,
    group = Material
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_x_log10() +
  scale_y_log10() +
  labs(
    title = "Loss Modulus (G'')",
    x = "Angular Frequency (rad/s)",
    y = "Loss Modulus (Pa)"
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

# Export the loss modulus figure
ggsave(
  "figures/loss_modulus_graph.png",
  plot = loss_graph,
  width = 6,
  height = 5,
  dpi = 600
)

#======================Data Reshaping============================
# Convert the dataset from wide to long format
# to facilitate plotting of multiple modulus types
rheology_long <- rheology |>
  pivot_longer(
    cols = c(Storage_Modulus_Pa, Loss_Modulus_Pa),
    names_to = "Modulus_Type",
    values_to = "Modulus_Pa"
  )

#===================Combined Frequency Sweep=====================
# Generate a combined frequency sweep graph displaying
# storage and loss modulus for both biomaterials

combined_rheology_graph <- ggplot(
  rheology_long,
  aes(
    x = Angular_Frequency_rad_s,
    y = Modulus_Pa,
    colour = Material,
    linetype = Modulus_Type,
    group = interaction(Material, Modulus_Type)
  )
) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Frequency Sweep",
    x = "Angular Frequency (rad/s)",
    y = "Modulus (Pa)"
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

# Export the combined rheology figure.
ggsave(
  "figures/rheology_graph.png",
  plot = combined_rheology_graph,
  width = 6,
  height = 5,
  dpi = 600
)

# -----------------------------------------------------------------
# NOTE:
# This analysis focuses on graphical visualization because
# replicate rheological measurements were not available for
# inferential statistical analysis.
# -----------------------------------------------------------------
