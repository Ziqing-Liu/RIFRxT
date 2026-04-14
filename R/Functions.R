library(tidyverse)
library(grow96)
library(lme4)
library(lmerTest)
library(rstatix)
library(dunn.test)

filter <- dplyr::filter

data <- processODData(specPath="specs", dataPath="data")

qcODData(data, path = "qc")

data <- blankODs(data, method = "fixed", value = 0.05)

growthAnalysis <- analyseODData(data)

shinyPlate(data)

view(growthAnalysis)

view(data)

names(growthAnalysis)

source("plot.R")

###Functions

#maxOD and mumax heatmap 
plot_heatmap(growthAnalysis, "plots/max_od_plot.pdf", metric = "max_od")

plot_heatmap(growthAnalysis, "plots/mumax_plot.pdf", metric = "mumax")

plot_OD_facet(data, "plots/max_od_plot.pdf", metric = "LB")

plot_OD_facet(data, "plots/max_od_plot.pdf", metric = "M9gluc")

### Bar plot with error bar
plot_data <- growthAnalysis$means %>%
  left_join(growthAnalysis$SEs, by = c("mutant_ID", "growth_medium"), 
            suffix = c("_mean", "_se"))

ggplot(plot_data, aes(x = mutant_ID, y = mumax_mean, fill = growth_medium)) +
  geom_col(position = "dodge") +
  geom_errorbar(aes(ymin = mumax_mean - mumax_se, 
                    ymax = mumax_mean + mumax_se),
                position = position_dodge(0.9), width = 0.25) +
  labs(title = "Maximum Growth Rate by Mutant",
       x = "Mutant", y = "mumax (per min)", fill = "Media") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#faceted plot looking at all 4 attribute at once 
growthAnalysis$means %>%
  pivot_longer(cols = c(mumax, lag, r2, maxOD), 
               names_to = "parameter", values_to = "value") %>%
  ggplot(aes(x = mutant_ID, y = value, colour = growth_medium, group = growth_medium)) +
  geom_point() +
  geom_line() +
  facet_wrap(~parameter, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#mutant growth vs wildtype 
wt_means <- growthAnalysis$means %>% filter(mutant_ID == "WT")

growthAnalysis$means %>%
  left_join(wt_means, by = "growth_medium", suffix = c("", "_WT")) %>%
  mutate(relative_mumax = mumax / mumax_WT) %>%
  ggplot(aes(x = mutant_ID, y = relative_mumax, fill = growth_medium)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = "red") +
  labs(title = "Growth Rate Relative to WT",
       y = "mumax / mumax_WT", x = "Mutant") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(growthAnalysis$means, aes(x = mutant_ID, y = r2, fill = growth_medium)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0.9, linetype = "dashed", colour = "red") +
  labs(title = "Model Fit (R²) by Mutant and Media",
       x = "Mutant", y = "R²", fill = "Media") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






