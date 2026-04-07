library(tidyverse)
library(grow96)
filter <- dplyr::filter

data <- processODData(specPath="specs", dataPath="data")

qcODData(data, path = "qc")

data <- blankODs(data, method = "fixed", value = 0.05)

shinyPlate(data)

growthAnalysis <- analyseODData(data)

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


### mumax scatter 

plates_to_include <- c("RIFxT42", "RIFxT45", "RIFxT48")

# Build named colour palette from pars instead of data
temp_colours <- setNames(
  colorRampPalette(c("steelblue", "firebrick", "darkorchid"))(length(plates_to_include)),
  paste0(sort(unique(growthAnalysis$pars$SetTemperature[
    growthAnalysis$pars$Plate %in% plates_to_include])), "°C")
)

scatter_mumax <- growthAnalysis$pars %>%
  filter(Plate %in% plates_to_include) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  ggplot(aes(
    x      = SetTemperature,
    y      = mumax,
    colour = SetTemperature,
    fill   = SetTemperature,
    shape  = growth_medium
  )) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 1.2) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.5, linewidth = 0.6, fatten = 1.5) +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.25, linewidth = 0.6) +
  scale_colour_manual(values = temp_colours) +
  scale_fill_manual(values = temp_colours) +
  facet_wrap(~mutant_ID, ncol = 6) +
  labs(
    title  = paste("µmax at", paste(names(temp_colours), collapse = ", ")),
    x      = "Temperature",
    y      = expression(mu[max]~(h^-1)),
    colour = "Temperature",
    fill   = "Temperature",
    shape  = "Media"
  ) +
  theme_minimal() +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1),
    legend.position = "bottom"
  )

print(scatter_mumax)
