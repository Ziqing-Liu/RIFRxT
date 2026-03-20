library(tidyverse)
library(grow96)


data <- processODData(specPath="specs", dataPath="data")

qcODData(data, path = "qc")

data <- blankODs(data, method = "fixed", value = 0.05)

shinyPlate(data)

growthAnalysis <- analyseODData(data)

growAnalysis

view(growthAnalysis)

head(growthAnalysis)

# Bar plot with error bar
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


### looks really good 
data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(mutant_ID = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
         SetTemperature = paste0(SetTemperature, "°C")) %>%
  ggplot(aes(x = Time_h, y = blankedOD,
             colour = SetTemperature,
             linetype = growth_medium,
             group = interaction(SetTemperature, growth_medium))) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "ribbon",
               aes(fill = SetTemperature), alpha = 0.15, colour = NA) +
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  scale_fill_manual(values  = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  facet_wrap(~mutant_ID, ncol = 6) +
  labs(title = "Growth Curves at 42°C and 45°C",
       x = "Time (h)", y = "OD (blanked)",
       colour = "Temperature", linetype = "Media", fill = "Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

#nice heatmap 
p1_heatmap <- data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45"),
         !is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  group_by(mutant_ID, SetTemperature, growth_medium) %>%
  summarise(max_OD = max(blankedOD, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = mutant_ID, y = SetTemperature, fill = max_OD)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(max_OD, 2)), size = 2.3, colour = "grey20") +
  scale_fill_gradient(low = "#f7f7f7", high = "firebrick", name = "Max OD") +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(
    title    = "Max growth OD — all mutants and conditions",
    subtitle = "Darker red = higher yield",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid      = element_blank(),
    strip.text      = element_text(face = "bold"),
    legend.position = "right"
  )

print(p1_heatmap)

plot_od_heatmap <- function(data, plates = c("RIFxT42", "RIFxT45")) {
  
  data %>%
    filter(Plate %in% plates, !is.na(mutant_ID), mutant_ID != "") %>%
    mutate(
      mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
      SetTemperature = paste0(SetTemperature, "°C")
    ) %>%
    group_by(mutant_ID, SetTemperature, growth_medium) %>%
    summarise(max_OD = max(blankedOD, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = mutant_ID, y = SetTemperature, fill = max_OD)) +
    geom_tile(colour = "white", linewidth = 0.5) +
    geom_text(aes(label = round(max_OD, 2)), size = 2.3, colour = "grey20") +
    scale_fill_gradient(low = "#f7f7f7", high = "firebrick", name = "Max OD") +
    facet_wrap(~growth_medium, ncol = 1) +
    labs(title = "Max growth OD — all mutants and conditions",
         subtitle = "Darker red = higher yield",
         x = NULL, y = NULL) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid      = element_blank(),
      strip.text      = element_text(face = "bold"),
      legend.position = "right"
    )
}

plot_od_heatmap(data)



