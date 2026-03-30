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
plates_to_include <- c("RIFxT42", "RIFxT45", "RIFxT48")

# Build a named colour palette automatically from the data
temp_colours <- setNames(
  colorRampPalette(c("steelblue", "firebrick", "darkorchid"))(length(plates_to_include)),
  paste0(sort(unique(data$SetTemperature[data$Plate %in% plates_to_include])), "°C")
)

scatter <- data %>%
  filter(Plate %in% plates_to_include) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  ggplot(aes(x = Time_h, y = blankedOD,
             colour = SetTemperature,
             linetype = growth_medium,
             group = interaction(SetTemperature, growth_medium))) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "ribbon",
               aes(fill = SetTemperature), alpha = 0.15, colour = NA) +
  scale_colour_manual(values = temp_colours) +
  scale_fill_manual(values = temp_colours) +
  facet_wrap(~mutant_ID, ncol = 6) +
  labs(
    title = paste("Growth Curves at", paste(names(temp_colours), collapse = ", ")),
    x = "Time (h)", y = "OD (blanked)",
    colour = "Temperature", linetype = "Media", fill = "Temperature"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(scatter)

### mumax 
plates_to_include <- c("RIFxT42", "RIFxT45", "RIFxT48")

temp_colours <- setNames(
  colorRampPalette(c("steelblue", "firebrick", "darkorchid"))(length(plates_to_include)),
  paste0(sort(unique(data$SetTemperature[data$Plate %in% plates_to_include])), "°C")
)

# Run growthAnalysis per plate to retain temperature info
mumax_data <- plates_to_include %>%
  lapply(function(plate) {
    subset <- data %>% filter(Plate == plate)
    analyseODData(subset)$means %>%
      mutate(SetTemperature = paste0(unique(subset$SetTemperature), "°C"))
  }) %>%
  bind_rows() %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(mutant_ID = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))))

scatter_mumax <- mumax_data %>%
  ggplot(aes(x = mutant_ID, y = mumax,
             colour = SetTemperature,
             linetype = growth_medium,
             group = interaction(SetTemperature, growth_medium))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_colour_manual(values = temp_colours) +
  labs(
    title    = paste("µmax at", paste(names(temp_colours), collapse = ", ")),
    x        = NULL, y = "µmax (per h)",
    colour   = "Temperature", linetype = "Media"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(scatter_mumax)

### heatmap 

plates_to_include <- c("RIFxT42", "RIFxT45", "RIFxT48")

p1_heatmap <- data %>%
  filter(Plate %in% plates_to_include,
         !is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  group_by(mutant_ID, SetTemperature, growth_medium) %>%
  summarise(max_OD = max(blankedOD, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = mutant_ID, y = SetTemperature, fill = max_OD)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("#f7f7f7", "steelblue", "firebrick"))(100),
    name = "Max OD"
  ) +
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


###OD heatmap 

plates_to_include <- c("RIFxT42", "RIFxT45", "RIFxT48")

# Run growthAnalysis per plate so temperature is preserved
mumax_data <- plates_to_include %>%
  lapply(function(plate) {
    subset <- data %>% filter(Plate == plate)
    result <- analyseODData(subset)
    result$means %>%
      mutate(Plate = plate,
             SetTemperature = paste0(unique(subset$SetTemperature), "°C"))
  }) %>%
  bind_rows() %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(mutant_ID = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))))

# Plot
p1_heatmap_mumax <- mumax_data %>%
  ggplot(aes(x = mutant_ID, y = SetTemperature, fill = mumax)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("#f7f7f7", "steelblue", "firebrick"))(100),
    name = "µmax"
  ) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(
    title    = "Maximum growth rate (µmax) — all mutants and conditions",
    subtitle = "Darker red = faster growth",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    panel.grid      = element_blank(),
    strip.text      = element_text(face = "bold"),
    legend.position = "right"
  )

print(p1_heatmap_mumax)



