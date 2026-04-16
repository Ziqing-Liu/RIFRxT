# Various plotting functions

# plotting maxOD or mumax as a heatmap:
plot_heatmap <- function(growthAnalysis, file_name, plates_to_include = NULL, metric = c("mumax", "max_od")) {
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
  metric <- match.arg(metric, choices = c("LB", "M9gluc"))
  
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
    filter(!is.na(mutant_ID))
  
  q <- ggplot(dat_facet, aes(
    x     = Time_h,
    y     = blankedOD,                        # ← raw OD column
    group = interaction(Well, Plate),          # ← one line per well per plate
  )) +
    geom_line(linewidth = 0.3, alpha = 0.5) +   # ← thin + transparent for replicates
    facet_grid(SetTemperature ~ mutant_ID) +
    labs(
      x = "Time (h)",
      y = "OD (blanked)",
    ) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      strip.text      = element_text(size = 8, face = "bold"),
      panel.spacing   = unit(0.3, "lines"),
      legend.position = "none"
    )
  
  ggsave(file_name, q, height = 8, width = 16, create.dir = TRUE)
  return(q)
}


#facet wrap plot 

plot_facet_wrap <- function(growthAnalysis, file_name, plates_to_include = NULL, 
                            metric = c("LB", "M9gluc"),
                            response = c("maxOD", "mumax")) {
  metric <- match.arg(metric, choices = c("LB", "M9gluc"))
  response <- match.arg(response)
  
  if (is.null(plates_to_include)) {
    plates_to_include <- growthAnalysis$par |>
      pull(Plate) |>
      unique()
  }
  
  dat_wrap <- growthAnalysis |>
    pluck("pars") |>
    filter(Plate %in% plates_to_include) |>
    filter(growth_medium == metric) |>
    mutate(
      mutant_ID      = fct(mutant_ID, levels = c("WT", paste0("M", 1:28))),
      SetTemperature = as.numeric(SetTemperature)
    ) |>
    group_by(mutant_ID, Replicate, SetTemperature) |>
    summarise(
      maxOD = mean(maxOD, na.rm = TRUE),
      mumax = mean(mumax, na.rm = TRUE),
      .groups = "drop"
    )
  
  y_lab <- if (response == "maxOD") "OD (blanked)" else "Maximum growth rate (mumax)"
  
  r <- ggplot(dat_wrap, aes(
    x     = SetTemperature,
    y     = .data[[response]],
    group = interaction(Replicate, mutant_ID)
  )) +
    geom_line(linewidth = 0.3, alpha = 0.5) +
    facet_wrap(~ mutant_ID) +          # one panel per mutant, temps on x-axis
    labs(x = "Temperature (°C)", y = y_lab) +
    theme_minimal(base_size = 9) +
    theme(
      axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
      strip.text      = element_text(size = 8, face = "bold"),
      panel.spacing   = unit(0.3, "lines"),
      legend.position = "none"
    )
  ggsave(file_name, r, height = 8, width = 16, create.dir = TRUE)
  return(r)
}
  









