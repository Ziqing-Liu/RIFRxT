library(tidyverse)
library(grow96)
library(rstatix)
library(outliers)
library(dunn.test)
library(car)

source("R/Code for Functions.R")

filter <- dplyr::filter

data <- processODData(specPath="specs", dataPath="data")

qcODData(data, path = "qc")

data <- blankODs(data, method = "fixed", value = 0.05)

growthAnalysis <- analyseODData(data, h = 20)


### Functions

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Heat map showing maxOD and mumax

plot_heatmap(growthAnalysis, "plots/max_od_plot.pdf", metric = "max_od")

plot_heatmap(growthAnalysis, "plots/mumax_plot.pdf", metric = "mumax")

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Individual heat map 

# LB only
plot_heatmap_individual(growthAnalysis, "plots/LB_plot.pdf", metric = "max_od", medium = "LB")

# M9gluc only
plot_heatmap_individual(growthAnalysis, "plots/M9_plot.pdf", metric = "max_od", medium = "M9gluc")

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Big facet plots showing the growth curve using OD for LB and M9 

plot_OD_facet(data, "plots/LB_plot.pdf", growth_media = "LB")

plot_OD_facet(data, "plots/M9_plot.pdf", growth_media = "M9gluc")

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Facet wrap plots looking at the OD level of each mutant at each temperature 

plot_facet_wrap(growthAnalysis, "plots/LB_OD_wrap.pdf", growth_media = "LB", label_map = my_labels)

plot_facet_wrap(growthAnalysis, "plots/M9_OD_wrap.pdf", growth_media = "M9gluc", label_map = my_labels)

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Facet wrap plots looking at the mumax of each mutant at each temperature 

plot_facet_wrap(growthAnalysis, "plots/LB_mumax_plot.pdf", growth_media = "LB", metric = "mumax", label_map = my_labels)

plot_facet_wrap(growthAnalysis, "plots/M9_mumax_plot.pdf", growth_media = "M9", metric = "mumax", label_map = my_labels)

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Two-group comparison (key: M1, LB/M9gluc, 30/45)
compareTwoGroups(
  growthData = growthAnalysis$pars,
  param      = "maxOD",
  group1 = list(strain = "WT",  medium = "LB", temp = 45),
  group2 = list(strain = "M12", medium = "LB", temp = 45)
)

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Multi_group comparison (all 406) (key: M1, LB/M9gluc, 30/45)

#Compare strains at a fixed temperature and medium
compareMultipleGroups(
  growthData       = growthAnalysis$pars,
  param            = "maxOD",
  strainsToCompare = c(NULL),
  media            = "LB",
  temperatures     = 36,
  groupBy          = "strain"
)

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Comparing 1 mutant with the other 28 mutant (just 28)

compare1with28(
  growthAnalysis$pars,
  groupBy          = "strain",
  reference_strain = "WT",
  media            = "LB",
  temperatures     = 45
)

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Compare one strain across temperatures in one medium
compareMultipleGroups(
  growthData       = growthAnalysis$pars,
  param            = "maxOD",
  strainsToCompare = "M3",
  media            = "LB",
  temperatures     = c(30, 32, 34),
  groupBy          = "temperature"
)

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Custom group comparison (strain, media and temperature are all different)

compareCustomGroups(
  growthData = growthAnalysis$pars,
  param      = "maxOD",
  groups     = list(
    list(strain = "WT", medium = "LB",     temp = 30),
    list(strain = "M1", medium = "M9gluc", temp = 32),
    list(strain = "M2", medium = "LB",     temp = 42)
  )
)


### view

shinyPlate(data)

view(growthAnalysis)

view(data)

names(growthAnalysis$pars)









