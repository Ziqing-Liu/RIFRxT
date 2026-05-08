library(tidyverse)
library(grow96)
library(rstatix)
library(outliers)

source("R/Code for Functions.R")

filter <- dplyr::filter

data <- processODData(specPath="specs", dataPath="data")

qcODData(data, path = "qc")

data <- blankODs(data, method = "fixed", value = 0.05)

growthAnalysis <- analyseODData(data, h = 20)


###Functions

#maxOD and mumax heatmap 
plot_heatmap(growthAnalysis, "plots/max_od_plot.pdf", metric = "max_od")

plot_heatmap(growthAnalysis, "plots/mumax_plot.pdf", metric = "mumax")

#Growth curve measured with OD for LB and M9
plot_OD_facet(data, "plots/LB_plot.pdf", growth_media = "LB")

plot_OD_facet(data, "plots/M9_plot.pdf", growth_media = "M9gluc")

#Facet wrap looking at the OD level of each mutant at each temperature 

plot_facet_wrap(growthAnalysis, "plots/LB_OD_wrap.pdf", growth_media = "LB", label_map = my_labels)

plot_facet_wrap(growthAnalysis, "plots/M9_OD_wrap.pdf", growth_media = "M9gluc", label_map = my_labels)

#Facet wrap looking at the mumax of each mutant at each temperature 
plot_facet_wrap(growthAnalysis, "plots/LB_mumax_plot.pdf", growth_media = "LB", metric = "mumax", label_map = my_labels)

plot_facet_wrap(growthAnalysis, "plots/M9_mumax_plot.pdf", growth_media = "M9", metric = "mumax", label_map = my_labels)

#Sig test across multiple mutants at the same temperature  
results <- compareMutants(
  growthData       = growthAnalysis$pars,
  param            = "maxOD",
  strainsToCompare = c("M12","M1"),  
  media            = c("LB"),
  temperatures     = c(45),
  removeOutliers   = 
)

# sig test for LB (change test strains to NULL to include all mutants)(change maxOD to mumax based on need)print(results_all_LB, n = Inf)
results_all_LB <- compareGrowthGroups(
  growthData        = growthAnalysis$pars,
  param             = "maxOD",
  baselineStrain    = "M1",
  baselineMedium    = "LB",
  baselineTemp      = 45,
  testMedium        = "LB",
  testTemp          = 45,
  testStrains       = "M12",
  testType          = "parametric",  
  pAdjMethod        = "BH"
)

# Sig test for M9_gluc (change test strains to NULL to include all mutants)(change maxOD to mumax based on need)
results_all_M9_gluc <- compareGrowthGroups(
  growthData        = growthAnalysis$pars,
  param             = "maxOD",
  baselineStrain    = "M12",
  baselineMedium    = "LB",
  baselineTemp      = 45,
  testMedium        = "LB",
  testTemp          = 45,
  testStrains       = NULL,
  testType          = "parametric",  
  pAdjMethod        = "BH"
)

grubbs.test(c(0.7001, 0.8187))


### view

results_maxOD$sig_mutants
results_maxOD$dunn_results
results_maxOD$effect_size

shinyPlate(data)

view(growthAnalysis)

view(data)

names(growthAnalysis$pars)

filter <- dplyr::filter







