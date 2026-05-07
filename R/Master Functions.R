library(tidyverse)
library(grow96)
library(rstatix)

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

#Test max OD 
results_maxOD <- analyse_pars_by_temperature(
  growthAnalysis = growthAnalysis,
  parameter      = "maxOD"
)

# Test growth rate
results_mumax <- analyse_pars_by_temperature(
  growthAnalysis = growthAnalysis,
  parameter      = "mumax"
)

# Test lag time, LB only
results_lag <- analyse_pars_by_temperature(
  growthAnalysis = growthAnalysis,
  parameter      = "lag",
)

### view

results_maxOD$sig_mutants
results_maxOD$dunn_results
results_maxOD$effect_size

shinyPlate(data)

view(growthAnalysis)

view(data)

names(growthAnalysis$pars)

filter <- dplyr::filter







