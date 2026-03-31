library(tidyverse)
library(grow96)
filter <- dplyr::filter

data <- processODData(specPath="specs", dataPath="data")

qcODData(data, path = "qc")

data <- blankODs(data, method = "fixed", value = 0.05)

shinyPlate(data)

growthAnalysis <- analyseODData(data)


#LB OD 

data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45", "RIFxT48")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  filter(growth_medium == "LB") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  ggplot(aes(
    x      = Time_h,
    y      = blankedOD,
    colour = SetTemperature,
    fill   = SetTemperature,
    group  = interaction(SetTemperature, Well)
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.8) +
  stat_summary(
    fun.data = mean_se, geom = "ribbon",
    alpha = 0.15, colour = NA
  ) +
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  scale_fill_manual(values   = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  facet_grid(SetTemperature ~ mutant_ID) +
  labs(
    title  = "OD Growth Curves at 42°C vs 45°C vs 48°C in LB",
    x      = "Time (h)",
    y      = "OD (blanked)",
    colour = "Temperature",
    fill   = "Temperature"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
    strip.text      = element_text(size = 8, face = "bold"),
    panel.spacing   = unit(0.3, "lines"),
    legend.position = "none"
  )

# LB mumax 

growthAnalysis$pars %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45", "RIFxT48")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  filter(growth_medium == "LB") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  ggplot(aes(
    x      = mutant_ID,
    y      = mumax,
    colour = SetTemperature,
    fill   = SetTemperature
  )) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, linewidth = 0.6, fatten = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, linewidth = 0.6) +
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  scale_fill_manual(values   = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  facet_wrap(~ SetTemperature, ncol = 1) +
  labs(
    title  = "Maximum Growth Rate (µmax) at 42°C, 45°C & 48°C in LB",
    x      = "Mutant",
    y      = expression(mu[max]~(h^-1)),
    colour = "Temperature",
    fill   = "Temperature"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
    strip.text      = element_text(size = 8, face = "bold"),
    panel.spacing   = unit(0.3, "lines"),
    legend.position = "none"
  )

#M9 OD

data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45", "RIFxT48")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  filter(growth_medium == "M9gluc") %>%                                  # ← LB → M9
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  ggplot(aes(
    x      = Time_h,
    y      = blankedOD,
    colour = SetTemperature,
    fill   = SetTemperature,
    group  = interaction(SetTemperature, Well)
  )) +
  stat_summary(fun = mean, geom = "line", linewidth = 0.8) +
  stat_summary(
    fun.data = mean_se, geom = "ribbon",
    alpha = 0.15, colour = NA
  ) +
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  scale_fill_manual(values   = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  facet_grid(SetTemperature ~ mutant_ID) +
  labs(
    title  = "OD Growth Curves at 42°C vs 45°C vs 48°C in M9",      # ← LB → M9
    x      = "Time (h)",
    y      = "OD (blanked)",
    colour = "Temperature",
    fill   = "Temperature"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
    strip.text      = element_text(size = 8, face = "bold"),
    panel.spacing   = unit(0.3, "lines"),
    legend.position = "none"
  )

#m9 mumax 

growthAnalysis$pars %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45", "RIFxT48")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  filter(growth_medium == "M9gluc") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  ggplot(aes(
    x      = mutant_ID,
    y      = mumax,
    colour = SetTemperature,
    fill   = SetTemperature
  )) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, linewidth = 0.6, fatten = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, linewidth = 0.6) +
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  scale_fill_manual(values   = c("42°C" = "steelblue", "45°C" = "firebrick", "48°C" = "darkorange")) +
  facet_wrap(~ SetTemperature, ncol = 1) +
  labs(
    title  = "Maximum Growth Rate (µmax) at 42°C, 45°C & 48°C in M9gluc",
    x      = "Mutant",
    y      = expression(mu[max]~(h^-1)),
    colour = "Temperature",
    fill   = "Temperature"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
    strip.text      = element_text(size = 8, face = "bold"),
    panel.spacing   = unit(0.3, "lines"),
    legend.position = "none"
  )
