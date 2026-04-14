# Various plotting functions

# plotting maxOD or mumax as a heatmap:
plot_heatmap <- function(growthAnalysis, file_name, plates_to_include = NULL, metric = c("mumax", "max_od")) {
  #plates_to_include <- c("RIFxT37", "RIFxT42", "RIFxT45", "RIFxT48")
  metric <- match.arg(metric)
  if (metric == "mumax") {
    # logic for mumax plot
  } else {
  }
  if (is.null(plates_to_include)) {
    plates_to_include <- growthAnalysis$pars |>
      pull(Plate) |>
      unique()
  }
  dat_plot <- growthAnalysis |>
    pluck("pars") |>
    filter(Plate %in% plates_to_include) |>
    mutate(
      mutant_ID      = fct(mutant_ID, levels = c("WT", paste0("M", 1:28))),
      SetTemperature = paste0(SetTemperature, "°C")
    ) |>
    group_by(mutant_ID, SetTemperature, growth_medium) |>
    summarise(
      max_OD = mean(maxOD, na.rm = TRUE),
      mumax = mean(mumax, na.rm = TRUE),
      .groups = "drop"
    )
    
  p <- ggplot(dat_plot, aes(x = mutant_ID, y = SetTemperature, fill = .data[[if (metric == "mumax") "mumax" else "max_OD"]])) +
    geom_tile(colour = "white", linewidth = 0.5) +
    scale_fill_gradientn(
      colours = colorRampPalette(c("#f7f7f7", "steelblue", "firebrick"))(100),
      name = if (metric == "mumax") "(mumax)" else "max(OD)"
    ) +
    facet_wrap(~growth_medium, ncol = 1, scales = "free") +
    labs(
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
      panel.grid      = element_blank(),
      strip.text      = element_text(face = "bold"),
      legend.position = "right"
    )
  ggsave(file_name, p, height = 8, width = 8)
  return(p)
}

#facet OD

plot_OD_facet <- function(data, file_name, plates_to_include = NULL, metric = c("LB", "M9gluc")) {
  #plates_to_include <- c("RIFxT37", "RIFxT42", "RIFxT45", "RIFxT48")
  metric <- match.arg(metric, choices = c("LB", "M9gluc"))
  if (metric %in% c("LB", "M9gluc")) {
  }
  if (is.null(plates_to_include)) {
    plates_to_include <- data |>
      pull(Plate) |>
      unique()
  }
  
  dat_facet <- data |>
    filter(Plate %in% plates_to_include) |>
    filter(str_detect(growth_medium, regex(paste0("^", metric, "$"), ignore_case = TRUE))) |>
    mutate(
      mutant_ID      = fct(mutant_ID, levels = c("WT", paste0("M", 1:28))),
      SetTemperature = paste0(SetTemperature, "°C")
    ) |>
    filter(!is.na(mutant_ID)) |>
    group_by(mutant_ID, SetTemperature, growth_medium, Time_h) |>
    summarise(
      max_OD = mean(blankedOD, na.rm = TRUE),
      .groups = "drop"
    )
  
  q <- ggplot(dat_facet, aes(
    x      = Time_h,
    y      = max_OD,
    group  = mutant_ID,
  )) +
    geom_line() +
    facet_grid(SetTemperature ~ mutant_ID) + 
    labs(
      x     = NULL,
      y     = "OD (blanked)",
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      strip.text      = element_text(size = 8, face = "bold"),
      panel.spacing   = unit(0.3, "lines"),
      legend.position = "none"
    )
  ggsave(file_name, q, height = 8, width = 8)
  return(q)
}



