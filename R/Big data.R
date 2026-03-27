data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45")) %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  filter(growth_medium == "LB") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = paste0(SetTemperature, "°C")
  ) %>%
  group_by(mutant_ID, SetTemperature, Well) %>%
  summarise(maxOD = max(blankedOD, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = SetTemperature, y = maxOD)) +
  stat_summary(fun = mean, geom = "crossbar", width = 0.4,
               colour = "steelblue", linewidth = 0.6) +
  stat_summary(fun.data = mean_se, geom = "errorbar",
               width = 0.2, colour = "steelblue") +
  geom_jitter(width = 0.1, alpha = 0.4, size = 1, colour = "grey40") +
  facet_grid(mutant_ID ~ SetTemperature, scales = "free_x", switch = "y") +
  labs(
    title = "Max OD in LB — 42°C vs 45°C",
    x     = NULL,
    y     = "Max OD (blanked)"
  ) +
  theme_minimal(base_size = 9) +
  theme(
    strip.text.y.left = element_text(size = 7, angle = 0),  # mutant names on left
    strip.text.x      = element_text(size = 9, face = "bold"),
    axis.text.x       = element_blank(),
    axis.ticks.x      = element_blank(),
    panel.spacing     = unit(0.2, "lines"),
    legend.position   = "none"
  )


data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45")) %>%
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
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  scale_fill_manual(values   = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  facet_grid(SetTemperature ~ mutant_ID) +
  labs(
    title  = "OD Growth Curves at 42°C vs 45°C in LB",
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

data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45")) %>%
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
  scale_colour_manual(values = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  scale_fill_manual(values   = c("42°C" = "steelblue", "45°C" = "firebrick")) +
  facet_grid(mutant_ID ~ SetTemperature) +
  labs(
    title  = "OD Growth Curves at 42°C vs 45°C in LB",
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
