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

###experiemnt 

data_files <- list.files("data")
temperatures <- data_files %>%
  str_extract("(?<=RIFxT)\\d+") %>%  # pulls the number after "RIFxT"
  unique() %>%
  na.omit()


print(temperatures)

all_analyses <- map(temperatures, function(temp) {
  
  # Create temporary folders for this temperature
  tmp_data <- paste0("tmp_data_", temp)
  tmp_spec <- paste0("tmp_spec_", temp)
  dir.create(tmp_data, showWarnings = FALSE)
  dir.create(tmp_spec, showWarnings = FALSE)
  
  # Copy only the matching files into the temp folders
  data_match <- list.files("data", pattern = paste0("T", temp, "_"), full.names = TRUE)
  spec_match  <- list.files("specs", pattern = paste0("T", temp, "_"), full.names = TRUE)
  file.copy(data_match, tmp_data)
  file.copy(spec_match, tmp_spec)
  
  # Run the analysis
  d <- processODData(specPath = tmp_spec, dataPath = tmp_data)
  d <- blankODs(d, method = "fixed", value = 0.05)
  result <- analyseODData(d)
  
  # Clean up temp folders
  unlink(tmp_data, recursive = TRUE)
  unlink(tmp_spec, recursive = TRUE)
  
  return(result)
}) %>%
  set_names(temperatures) 

combined_means <- map_dfr(all_analyses, ~ .x$means, .id = "temperature")
combined_SEs   <- map_dfr(all_analyses, ~ .x$SEs,   .id = "temperature")

plot_data <- combined_means %>%
  left_join(combined_SEs, by = c("mutant_ID", "growth_medium", "temperature"),
            suffix = c("_mean", "_se"))

data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(mutant_ID = factor(mutant_ID, 
                            levels = c("WT", paste0("M", 1:28))),
         SetTemperature = paste0(SetTemperature, "°C")) %>%
  ggplot(aes(x = Time_h, y = blankedOD, 
             colour = interaction(SetTemperature, growth_medium),
             group = interaction(SetTemperature, growth_medium, Replicate))) +
  geom_line(alpha = 0.3) +
  stat_summary(aes(group = interaction(SetTemperature, growth_medium)), 
               fun = mean, geom = "line", linewidth = 1) +
  facet_wrap(~mutant_ID) +
  labs(title = "Growth Curves at 42°C and 45°C",
       x = "Time (h)", y = "OD (blanked)", 
       colour = "Temperature & Media") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")


combined_means %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(mutant_ID = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
         temperature = paste0(temperature, "°C")) %>%
  ggplot(aes(x = temperature, y = mutant_ID, fill = mumax)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  facet_wrap(~growth_medium) +
  scale_fill_gradient2(low = "steelblue", mid = "white", 
                       high = "firebrick", midpoint = median(combined_means$mumax)) +
  labs(title = "Growth Rate Across Temperatures",
       x = "Temperature", y = "Mutant", fill = "mumax") +
  theme_minimal()

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

wt_summary <- data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45")) %>%
  filter(mutant_ID == "WT") %>%
  mutate(SetTemperature = paste0(SetTemperature, "°C")) %>%
  group_by(SetTemperature, growth_medium, Time_h) %>%
  summarise(wt_OD = mean(blankedOD, na.rm = TRUE), .groups = "drop")

data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(mutant_ID = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
         SetTemperature = paste0(SetTemperature, "°C")) %>%
  left_join(wt_summary, by = c("SetTemperature", "growth_medium", "Time_h")) %>%
  mutate(diff_from_wt = blankedOD - wt_OD) %>%
  ggplot(aes(x = Time_h, y = diff_from_wt,
             colour = SetTemperature,
             linetype = growth_medium,
             group = interaction(SetTemperature, growth_medium))) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "ribbon",
               aes(fill = SetTemperature), alpha = 0.15, colour = NA) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "black", linewidth = 0.5) +
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  scale_fill_manual(values  = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  facet_wrap(~mutant_ID, ncol = 6) +
  labs(title = "OD Difference from WT at 42°C and 45°C",
       x = "Time (h)", y = "OD difference from WT",
       colour = "Temperature", linetype = "Media", fill = "Temperature") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

