library(tidyverse)
library(grow96)

filter <- dplyr::filter


data <- processODData(specPath="specs", dataPath="data")

qcODData(data, path = "qc")

data <- blankODs(data, method = "fixed", value = 0.05)

shinyPlate(data)

growthAnalysis <- analyseODData(data)

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


###mumax

plates_to_include <- c("RIFxT42", "RIFxT45", "RIFxT48")

p1_heatmap_mumax <- growthAnalysis$pars %>%
  filter(Plate %in% plates_to_include,
         !is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  group_by(mutant_ID, SetTemperature, growth_medium) %>%
  summarise(mean_mumax = mean(mumax, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = mutant_ID, y = SetTemperature, fill = mean_mumax)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("#f7f7f7", "steelblue", "firebrick"))(100),
    name    = expression(mu[max]~(h^-1))
  ) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(
    title    = "Mean µmax — all mutants and conditions",
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
