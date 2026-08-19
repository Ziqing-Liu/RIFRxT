# Various plotting functions

summariseGrowth <- function(growthAnalysis, temps = NULL, media = NULL,
                            strain_col = "mutant_ID",
                            temp_col   = "SetTemperature",
                            medium_col = "growth_medium") {
  
  df <- growthAnalysis$pars
  
  missing_cols <- setdiff(c(strain_col, temp_col, medium_col), names(df))
  if (length(missing_cols) > 0) {
    stop("Column(s) not found: ", paste(missing_cols, collapse = ", "),
         "\nAvailable columns: ", paste(names(df), collapse = ", "))
  }
  
  if (!is.null(temps)) df <- df %>% filter(.data[[temp_col]] %in% temps)
  if (!is.null(media)) df <- df %>% filter(.data[[medium_col]] %in% media)
  
  group_cols <- strain_col
  if (is.null(temps) || length(temps) > 1) group_cols <- c(temp_col, group_cols)
  if (is.null(media) || length(media) > 1) group_cols <- c(medium_col, group_cols)
  
  df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      maxOD  = mean(maxOD, na.rm = TRUE),
      mumax  = mean(mumax, na.rm = TRUE),
      n_reps = n(),
      .groups = "drop"
    ) %>%
    rename(mutant = all_of(strain_col)) %>%
    arrange(across(all_of(setdiff(group_cols, strain_col))), mutant)
}


# ══════════════════════════════════════════════════════════════════════════════
# Heat map 
# ══════════════════════════════════════════════════════════════════════════════
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

# ══════════════════════════════════════════════════════════════════════════════
# Individual heat map 
# ══════════════════════════════════════════════════════════════════════════════
plot_heatmap_individual <- function(growthAnalysis, file_name, plates_to_include = NULL, metric = c("mumax", "max_od"), medium = c("LB", "M9gluc")) {
  metric <- match.arg(metric)
  medium <- match.arg(medium, several.ok = TRUE)  # several.ok = TRUE allows both to be run if unspecified
  
  base <- tools::file_path_sans_ext(file_name)
  ext  <- tools::file_ext(file_name)
  
  if (is.null(plates_to_include)) {
    plates_to_include <- growthAnalysis$pars |>
      pull(Plate) |>
      unique()
  }
  
  plot_single_medium <- function(med) {
    dat_plot <- growthAnalysis |>
      pluck("pars") |>
      filter(Plate %in% plates_to_include, growth_medium == med) |>
      mutate(
        mutant_ID      = fct(mutant_ID, levels = c("WT", paste0("M", 1:28))),
        SetTemperature = paste0(SetTemperature, "°C")
      ) |>
      group_by(mutant_ID, SetTemperature, growth_medium) |>
      summarise(
        max_OD = mean(maxOD, na.rm = TRUE),
        mumax  = mean(mumax, na.rm = TRUE),
        .groups = "drop"
      )
    
    draw <- ggplot(dat_plot, aes(x = mutant_ID, y = SetTemperature, fill = .data[[if (metric == "mumax") "mumax" else "max_OD"]])) +
      geom_tile(colour = "white", linewidth = 0.5) +
      scale_fill_gradientn(
        colours = colorRampPalette(c("#f7f7f7", "steelblue", "firebrick"))(100),
        name    = if (metric == "mumax") "(mumax)" else "max(OD)"
      ) +
      facet_wrap(~growth_medium, ncol = 1, scales = "free") +
      labs(x = NULL, y = NULL) +
      theme_minimal(base_size = 11) +
      theme(
        axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid      = element_blank(),
        strip.text      = element_text(face = "bold"),
        legend.position = "right"
      )
    
    ggsave(paste0(base, "_", med, ".", ext), draw, height = 8, width = 8)
    print(draw)
    return(draw)
  }
  
  plots <- lapply(medium, plot_single_medium)
  names(plots) <- medium
  invisible(plots)
}

# ══════════════════════════════════════════════════════════════════════════════
# Facet OD plots 
# ══════════════════════════════════════════════════════════════════════════════

plot_OD_facet <- function(data, file_name, plates_to_include = NULL, growth_media = c("LB", "M9gluc")) {
  growth_media <- match.arg(growth_media, choices = c("LB", "M9gluc"))
  
  if (is.null(plates_to_include)) {
    plates_to_include <- data |>
      pull(Plate) |>
      unique()
  }
  
  dat_facet <- data |>
    filter(Plate %in% plates_to_include) |>
    filter(str_detect(growth_medium, regex(paste0("^", growth_media, "$"), ignore_case = TRUE))) |>
    mutate(
      mutant_ID      = fct(mutant_ID, levels = c("WT", paste0("M", 1:28))),
      SetTemperature = paste0(SetTemperature, "°C")
    ) |>
    filter(!is.na(mutant_ID))
  
  q <- ggplot(dat_facet, aes(
    x     = Time_h,
    y     = blankedOD,                        
    group = interaction(Plate, Well, Replicate),          
  )) +
    geom_line(linewidth = 0.3, alpha = 0.5) +  
    facet_grid(SetTemperature ~ mutant_ID) +
    labs(
      x = "Time (h)",
      y = paste0("OD (blanked) - ", growth_media),  
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


# ══════════════════════════════════════════════════════════════════════════════
# Facet wrap plots 
# ══════════════════════════════════════════════════════════════════════════════

plot_facet_wrap <- function(growthAnalysis, file_name, plates_to_include = NULL, 
                            growth_media = c("LB", "M9gluc"),
                            metric = c("maxOD", "mumax"),
                            label_map = c(
                              "rpoB_S522F" = "M1",
                              "rpoB_L511P" = "M2",
                              "rpoB_D516G" = "M3",
                              "rpoB_I572S" = "M4",
                              "rpoB_S512F" = "M5",
                              "rpoB_S531F" = "M6",
                              "rpoB_H526N" = "M7",
                              "rpoB_R529L" = "M8",
                              "rpoB_I572F" = "M9",
                              "rpoB_S512P" = "M10",
                              "rpoB_I572L" = "M11",
                              "rpoB_H526Y" = "M12",
                              "rpoB_L533P" = "M13",
                              "rpoB_H526Q" = "M14",
                              "rpoB_T563P" = "M15",
                              "rpoB_S509R" = "M16",
                              "rpoB_D516N" = "M17",
                              "rpoB_D516V" = "M18",
                              "rpoB_S531Y" = "M19",
                              "rpoB_V146G" = "M20",
                              "rpoB_G534C" = "M21",
                              "rpoB_R529C" = "M22",
                              "rpoB_R529G" = "M23",
                              "rpoB_G570C" = "M24",
                              "rpoB_Q513P" = "M25",
                              "rpoB_V146F" = "M26",
                              "rpoB_A532P" = "M27",
                              "rpoB_H526D" = "M28"
                            )) {
  growth_media <- match.arg(growth_media)
  metric <- match.arg(metric)
  
  if (is.null(plates_to_include)) {
    plates_to_include <- growthAnalysis$par |>
      pull(Plate) |>
      unique()
  }
  
  dat_wrap <- growthAnalysis |>
    pluck("pars") |>
    filter(Plate %in% plates_to_include) |>
    filter(growth_medium == growth_media) |>
    mutate(
      mutant_ID      = fct(mutant_ID, levels = c("WT", paste0("M", 1:28))),
      SetTemperature = as.numeric(SetTemperature),
      mutant_ID      = if (!is.null(label_map)) fct_recode(mutant_ID, !!!label_map) else mutant_ID
    ) |>
    group_by(mutant_ID, Replicate, SetTemperature) |>
    summarise(
      maxOD = mean(maxOD, na.rm = TRUE),
      mumax = mean(mumax, na.rm = TRUE),
      .groups = "drop"
    )
  
  dat_mean <- dat_wrap |>
    group_by(mutant_ID, SetTemperature) |>
    summarise(
      mean_metric = mean(.data[[metric]], na.rm = TRUE),
      sd_metric   = sd(.data[[metric]], na.rm = TRUE),
      n_metric    = sum(!is.na(.data[[metric]])),
      se_metric   = sd_metric / sqrt(n_metric),
      .groups = "drop"
    )
  
  y_lab <- if (metric == "maxOD") {
    "maximum OD"
  } else {
    "Maximum growth rate"
  }
  
  r <- ggplot(dat_wrap, aes(
    x     = SetTemperature,
    y     = .data[[metric]],
    group = interaction(Replicate, mutant_ID)
  )) +
    geom_point(
      colour  = "black",
      size    = 1.6,
      alpha   = 0.7,
      position = position_jitter(width = 0.15, height = 0)
    ) +
    geom_line(
      data = dat_mean,
      aes(x = SetTemperature, y = mean_metric, group = mutant_ID),
      linewidth = 1, colour = "red", inherit.aes = FALSE
    ) +
    facet_wrap(~ mutant_ID) +          
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

#auto correlation makes vlaues at high temperature become weird 


# ══════════════════════════════════════════════════════════════════════════════
# FUNCTION: Two-group comparison
# ══════════════════════════════════════════════════════════════════════════════

compareTwoGroups <- function(
    growthData,
    param       = "maxOD",
    strainCol   = "mutant_ID",
    mediumCol   = "growth_medium",
    tempCol     = "SetTemperature",
    group1      = list(strain = "WT",  medium = "LB", temp = 30),
    group2      = list(strain = "M1",  medium = "LB", temp = 30),
    paired      = NULL,   # NULL = auto-detect, TRUE/FALSE = force
    alpha       = 0.05,
    verbose     = TRUE
) {
  
  library(car)  # for Levene's test
  
  # ── 1. Extract the two groups ─────────────────────────────────────────────
  get_vals <- function(g) {
    growthData %>%
      rename(.strain = all_of(strainCol),
             .medium = all_of(mediumCol),
             .temp   = all_of(tempCol),
             .param  = all_of(param)) %>%
      filter(.strain == g$strain,
             .medium == g$medium,
             .temp   == g$temp,
             !is.na(.param)) %>%
      pull(.param)
  }
  
  vals1 <- get_vals(group1)
  vals2 <- get_vals(group2)
  
  if (length(vals1) < 2 | length(vals2) < 2)
    stop("Need at least 2 values in each group.")
  
  if (is.null(paired)) {
    paired <- group1$strain == group2$strain
    if (verbose)
      message(sprintf("[Auto] %s: paired = %s",
                      if (paired) "Same strain detected" else "Different strains detected",
                      paired))
  }
  
  if (paired && length(vals1) != length(vals2))
    stop("Paired test requires equal group sizes.")
  
  # ── 3. Assess parametric assumptions ─────────────────────────────────────
  if (verbose) cat("\n── Parametric Assumption Checks ──\n\n")
  
  # Normality: Shapiro-Wilk on each group
  sw1 <- if (length(vals1) >= 3) shapiro.test(vals1) else NULL
  sw2 <- if (length(vals2) >= 3) shapiro.test(vals2) else NULL
  
  normal1 <- if (is.null(sw1)) TRUE else sw1$p.value > alpha
  normal2 <- if (is.null(sw2)) TRUE else sw2$p.value > alpha
  
  if (verbose) {
    cat(sprintf("   Normality (Shapiro-Wilk):\n"))
    cat(sprintf("     Group 1 (%s × %s × %s°C): W = %.4f, p = %.4f  %s\n",
                group1$strain, group1$medium, group1$temp,
                if (!is.null(sw1)) sw1$statistic else NA,
                if (!is.null(sw1)) sw1$p.value else NA,
                if (normal1) "✓ normal" else "✗ non-normal"))
    cat(sprintf("     Group 2 (%s × %s × %s°C): W = %.4f, p = %.4f  %s\n\n",
                group2$strain, group2$medium, group2$temp,
                if (!is.null(sw2)) sw2$statistic else NA,
                if (!is.null(sw2)) sw2$p.value else NA,
                if (normal2) "✓ normal" else "✗ non-normal"))
  }
  
  # Equal variance: Levene's test (only relevant for unpaired)
  if (!paired) {
    all_vals  <- c(vals1, vals2)
    group_fac <- factor(c(rep("g1", length(vals1)), rep("g2", length(vals2))))
    lev       <- leveneTest(all_vals ~ group_fac)
    equal_var <- lev$`Pr(>F)`[1] > alpha
    
    if (verbose) {
      cat(sprintf("   Equal Variance (Levene's test): F = %.4f, p = %.4f  %s\n\n",
                  lev$`F value`[1],
                  lev$`Pr(>F)`[1],
                  if (equal_var) "✓ equal variances" else "✗ unequal variances"))
    }
  } else {
    equal_var <- TRUE  # not needed for paired
  }
  
  # ── 4. Choose and run test ────────────────────────────────────────────────
  assumptions_met <- normal1 & normal2
  
  if (assumptions_met) {
    if (paired) {
      test        <- t.test(vals1, vals2, paired = TRUE)
      method_used <- "Paired t-test"
    } else {
      # Student's if equal variance, Welch's if not
      test        <- t.test(vals1, vals2, paired = FALSE, var.equal = equal_var)
      method_used <- if (equal_var) "Student's unpaired t-test" else "Welch's unpaired t-test"
    }
  } else {
    test        <- wilcox.test(vals1, vals2, paired = paired, exact = FALSE)
    method_used <- if (paired) "Wilcoxon signed-rank test" else "Mann-Whitney U test"
  }
  
  # ── 5. Results ────────────────────────────────────────────────────────────
  sig_label <- case_when(
    test$p.value < 0.001 ~ "***",
    test$p.value < 0.01  ~ "**",
    test$p.value < 0.05  ~ "*",
    test$p.value < 0.1   ~ ".",
    TRUE                 ~ "ns"
  )
  
  if (verbose) {
    cat(sprintf("── Test: %s ──\n\n", method_used))
    cat(sprintf("   Group 1 : %s × %s × %s°C  (n=%d, mean=%.4f)\n",
                group1$strain, group1$medium, group1$temp, length(vals1), mean(vals1)))
    cat(sprintf("   Group 2 : %s × %s × %s°C  (n=%d, mean=%.4f)\n",
                group2$strain, group2$medium, group2$temp, length(vals2), mean(vals2)))
    cat(sprintf("   Fold change : %.4f\n", mean(vals1) / mean(vals2)))
    cat(sprintf("   Statistic   : %.4f\n", test$statistic))
    cat(sprintf("   p-value     : %.4f  %s\n\n", test$p.value, sig_label))
  }
  
  invisible(list(
    method      = method_used,
    statistic   = test$statistic,
    p.value     = test$p.value,
    sig_label   = sig_label,
    significant = test$p.value < alpha,
    mean1       = mean(vals1),
    mean2       = mean(vals2),
    fold_change = mean(vals1) / mean(vals2),
    paired      = paired,
    assumptions = list(normal1 = normal1, normal2 = normal2, equal_var = equal_var)
  ))
}


# ══════════════════════════════════════════════════════════════════════════════
# FUNCTION: Multi-group comparison
# ══════════════════════════════════════════════════════════════════════════════

compareMultipleGroups <- function(
    growthData,
    param            = "maxOD",
    strainCol        = "mutant_ID",
    mediumCol        = "growth_medium",
    tempCol          = "SetTemperature",
    strainsToCompare = NULL,
    media            = NULL,
    temperatures     = NULL,
    groupBy          = c("strain", "temperature", "medium"),
    alpha            = 0.05,
    verbose          = TRUE
) {
  
  library(car)
  library(dunn.test)
  library(tibble)
  library(dplyr)
  library(purrr)
  
  # safe formatter — returns "NA" string instead of crashing
  fmt <- function(x) if (is.na(x)) "NA" else sprintf("%.4f", x)
  
  # ── 1. Validate groupBy ───────────────────────────────────────────────────
  groupBy <- match.arg(groupBy, several.ok = FALSE)
  
  # ── 2. Filter data ────────────────────────────────────────────────────────
  df <- growthData %>%
    rename(
      .strain = all_of(strainCol),
      .medium = all_of(mediumCol),
      .temp   = all_of(tempCol),
      .param  = all_of(param)
    ) %>%
    filter(!is.na(.param))
  
  if (!is.null(strainsToCompare))
    df <- df %>% filter(.strain %in% strainsToCompare)
  
  if (!is.null(media))
    df <- df %>% filter(.medium %in% media)
  
  if (!is.null(temperatures))
    df <- df %>% filter(.temp %in% temperatures)
  
  # ── 3. Define grouping variable and fixed variables ───────────────────────
  if (groupBy == "strain") {
    df         <- df %>% mutate(.group = factor(.strain))
    group_lab  <- "strain"
    split_vars <- c(".medium", ".temp")
  } else if (groupBy == "temperature") {
    df         <- df %>% mutate(.group = factor(.temp))
    group_lab  <- "temperature"
    split_vars <- c(".strain", ".medium")
  } else {
    df         <- df %>% mutate(.group = factor(.medium))
    group_lab  <- "medium"
    split_vars <- c(".strain", ".temp")
  }
  
  if (length(unique(df$.group)) < 3)
    stop(sprintf(
      "Need at least 3 levels of '%s' to compare. Found: %s",
      group_lab,
      paste(unique(df$.group), collapse = ", ")
    ))
  
  # ── 4. Split into sub-analyses and run each ───────────────────────────────
  splits <- df %>%
    group_by(across(all_of(split_vars))) %>%
    group_split()
  
  split_keys <- df %>%
    group_by(across(all_of(split_vars))) %>%
    group_keys()
  
  all_results <- map2(splits, seq_len(nrow(split_keys)), function(subdf, i) {
    
    key <- split_keys[i, ]
    
    context_label <- paste(
      names(key),
      unlist(key),
      sep = " = ",
      collapse = " | "
    )
    
    if (verbose)
      cat(sprintf(
        "\n══════════════════════════════════════════════════\n Comparing by %s  [%s]\n══════════════════════════════════════════════════\n",
        group_lab, context_label
      ))
    
    if (length(unique(subdf$.group)) < 3) {
      if (verbose)
        cat(sprintf("   SKIP — fewer than 3 levels of %s found here\n", group_lab))
      return(NULL)
    }
    
    # ── Parametric assumption checks ─────────────────────────────────────────
    if (verbose) cat("\n── Parametric Assumption Checks ──\n\n")
    
    normality <- subdf %>%
      group_by(.group) %>%
      summarise(
        n       = n(),
        sw_stat = if (n() >= 3) shapiro.test(.param)$statistic else NA_real_,
        sw_p    = if (n() >= 3) shapiro.test(.param)$p.value   else NA_real_,
        normal  = if (n() >= 3) shapiro.test(.param)$p.value > alpha else TRUE,
        .groups = "drop"
      )
    
    all_normal <- all(normality$normal, na.rm = TRUE)
    
    if (verbose) {
      cat("   Normality (Shapiro-Wilk per group):\n")
      for (r in seq_len(nrow(normality))) {
        if (is.na(normality$sw_stat[r])) {
          cat(sprintf("     %s: insufficient replicates (n=%d, need ≥ 3)\n",
                      as.character(normality$.group[r]),
                      normality$n[r]))
        } else {
          cat(sprintf("     %s: W = %s, p = %s  %s\n",
                      as.character(normality$.group[r]),
                      fmt(normality$sw_stat[r]),
                      fmt(normality$sw_p[r]),
                      if (isTRUE(normality$normal[r])) "✓ normal" else "✗ non-normal"))
        }
      }
      cat("\n")
    }
    
    lev       <- leveneTest(.param ~ .group, data = subdf)
    equal_var <- lev$`Pr(>F)`[1] > alpha
    
    if (verbose)
      cat(sprintf("   Equal Variance (Levene's test): F = %s, p = %s  %s\n\n",
                  fmt(lev$`F value`[1]),
                  fmt(lev$`Pr(>F)`[1]),
                  if (equal_var) "✓ equal variances" else "✗ unequal variances"))
    
    assumptions_met <- all_normal & equal_var
    
    # ── Omnibus test ──────────────────────────────────────────────────────────
    if (assumptions_met) {
      
      aov_fit   <- aov(.param ~ .group, data = subdf)
      aov_sum   <- summary(aov_fit)
      omni_p    <- aov_sum[[1]]$`Pr(>F)`[1]
      omni_stat <- aov_sum[[1]]$`F value`[1]
      omni_name <- "One-way ANOVA"
      
      if (verbose)
        cat(sprintf("── %s: F = %s, p = %s  %s\n\n",
                    omni_name,
                    fmt(omni_stat),
                    fmt(omni_p),
                    if (omni_p < alpha) "→ significant, running Tukey HSD" else "→ not significant"))
      
      if (omni_p < alpha) {
        posthoc_raw <- TukeyHSD(aov_fit)
        posthoc_df  <- as.data.frame(posthoc_raw$.group) %>%
          rownames_to_column("comparison") %>%
          rename(mean_diff = diff, lower = lwr, upper = upr, p.value = `p adj`) %>%
          mutate(
            sig_label   = case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01  ~ "**",
              p.value < 0.05  ~ "*",
              p.value < 0.1   ~ ".",
              TRUE            ~ "ns"
            ),
            significant = p.value < alpha,
            posthoc     = "Tukey HSD"
          )
        
        if (verbose) {
          cat("── Post-hoc: Tukey HSD ──\n\n")
          print(as.data.frame(posthoc_df %>%        # ← fixed
                                select(comparison, mean_diff, p.value, sig_label, significant)))
          cat("\n")
        }
      } else {
        posthoc_df <- NULL
      }
      
    } else {
      
      kw        <- kruskal.test(.param ~ .group, data = subdf)
      omni_p    <- kw$p.value
      omni_stat <- kw$statistic
      omni_name <- "Kruskal-Wallis test"
      
      if (verbose)
        cat(sprintf("── %s: H = %s, p = %s  %s\n\n",
                    omni_name,
                    fmt(omni_stat),
                    fmt(omni_p),
                    if (omni_p < alpha) "→ significant, running Dunn's test" else "→ not significant"))
      
      if (omni_p < alpha) {
        dunn       <- dunn.test(subdf$.param, subdf$.group, method = "bh", altp = TRUE)
        posthoc_df <- tibble(
          comparison  = dunn$comparisons,
          statistic   = dunn$Z,
          p.value     = dunn$altP.adjusted
        ) %>%
          mutate(
            sig_label   = case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01  ~ "**",
              p.value < 0.05  ~ "*",
              p.value < 0.1   ~ ".",
              TRUE            ~ "ns"
            ),
            significant = p.value < alpha,
            posthoc     = "Dunn's test (BH adjusted)"
          )
        
        if (verbose) {
          cat("── Post-hoc: Dunn's Test ──\n\n")
          print(as.data.frame(posthoc_df %>%        # ← fixed
                                select(comparison, statistic, p.value, sig_label, significant)))
          cat("\n")
        }
      } else {
        posthoc_df <- NULL
      }
    }
    
    list(
      context         = as.list(key),
      groupBy         = group_lab,
      omnibus_test    = omni_name,
      omnibus_stat    = omni_stat,
      omnibus_p       = omni_p,
      assumptions_met = assumptions_met,
      normality       = normality,
      equal_variance  = equal_var,
      posthoc         = posthoc_df
    )
  })
  
  invisible(all_results)
}

# ══════════════════════════════════════════════════════════════════════════════
# FUNCTION: Comparing 1 mutant with the other 28 
# ══════════════════════════════════════════════════════════════════════════════

compare1with28 <- function(
    growthData,
    param            = "maxOD",
    strainCol        = "mutant_ID",
    mediumCol        = "growth_medium",
    tempCol          = "SetTemperature",
    strainsToCompare = NULL,
    media            = NULL,
    temperatures     = NULL,
    groupBy          = c("strain", "temperature", "medium"),
    alpha            = 0.05,
    reference_strain = NULL,
    verbose          = TRUE
) {
  
  library(car)
  library(dunn.test)
  library(tibble)
  library(dplyr)
  library(purrr)
  
  fmt <- function(x) if (is.na(x)) "NA" else sprintf("%.4f", x)
  
  groupBy <- match.arg(groupBy, several.ok = FALSE)
  
  df <- growthData %>%
    rename(
      .strain = all_of(strainCol),
      .medium = all_of(mediumCol),
      .temp   = all_of(tempCol),
      .param  = all_of(param)
    ) %>%
    filter(!is.na(.param))
  
  if (!is.null(strainsToCompare))
    df <- df %>% filter(.strain %in% strainsToCompare)
  
  if (!is.null(media))
    df <- df %>% filter(.medium %in% media)
  
  if (!is.null(temperatures))
    df <- df %>% filter(.temp %in% temperatures)
  
  if (groupBy == "strain") {
    df         <- df %>% mutate(.group = factor(.strain))
    group_lab  <- "strain"
    split_vars <- c(".medium", ".temp")
  } else if (groupBy == "temperature") {
    df         <- df %>% mutate(.group = factor(.temp))
    group_lab  <- "temperature"
    split_vars <- c(".strain", ".medium")
  } else {
    df         <- df %>% mutate(.group = factor(.medium))
    group_lab  <- "medium"
    split_vars <- c(".strain", ".temp")
  }
  
  if (length(unique(df$.group)) < 3)
    stop(sprintf(
      "Need at least 3 levels of '%s' to compare. Found: %s",
      group_lab,
      paste(unique(df$.group), collapse = ", ")
    ))
  
  if (!is.null(reference_strain) && !reference_strain %in% unique(df$.group)) {
    stop(sprintf(
      "reference_strain '%s' not found in data. Available: %s",
      reference_strain,
      paste(unique(df$.group), collapse = ", ")
    ))
  }
  
  splits <- df %>%
    group_by(across(all_of(split_vars))) %>%
    group_split()
  
  split_keys <- df %>%
    group_by(across(all_of(split_vars))) %>%
    group_keys()
  
  all_results <- map2(splits, seq_len(nrow(split_keys)), function(subdf, i) {
    
    key <- split_keys[i, ]
    
    context_label <- paste(
      names(key),
      unlist(key),
      sep = " = ",
      collapse = " | "
    )
    
    if (verbose)
      cat(sprintf(
        "\n══════════════════════════════════════════════════\n Comparing by %s  [%s]\n══════════════════════════════════════════════════\n",
        group_lab, context_label
      ))
    
    if (length(unique(subdf$.group)) < 3) {
      if (verbose)
        cat(sprintf("   SKIP — fewer than 3 levels of %s found here\n", group_lab))
      return(NULL)
    }
    
    if (verbose) cat("\n── Parametric Assumption Checks ──\n\n")
    
    normality <- subdf %>%
      group_by(.group) %>%
      summarise(
        n       = n(),
        sw_stat = if (n() >= 3) shapiro.test(.param)$statistic else NA_real_,
        sw_p    = if (n() >= 3) shapiro.test(.param)$p.value   else NA_real_,
        normal  = if (n() >= 3) shapiro.test(.param)$p.value > alpha else TRUE,
        .groups = "drop"
      )
    
    all_normal <- all(normality$normal, na.rm = TRUE)
    
    if (verbose) {
      cat("   Normality (Shapiro-Wilk per group):\n")
      for (r in seq_len(nrow(normality))) {
        if (is.na(normality$sw_stat[r])) {
          cat(sprintf("     %s: insufficient replicates (n=%d, need ≥ 3)\n",
                      as.character(normality$.group[r]),
                      normality$n[r]))
        } else {
          cat(sprintf("     %s: W = %s, p = %s  %s\n",
                      as.character(normality$.group[r]),
                      fmt(normality$sw_stat[r]),
                      fmt(normality$sw_p[r]),
                      if (isTRUE(normality$normal[r])) "✓ normal" else "✗ non-normal"))
        }
      }
      cat("\n")
    }
    
    lev       <- leveneTest(.param ~ .group, data = subdf)
    equal_var <- lev$`Pr(>F)`[1] > alpha
    
    if (verbose)
      cat(sprintf("   Equal Variance (Levene's test): F = %s, p = %s  %s\n\n",
                  fmt(lev$`F value`[1]),
                  fmt(lev$`Pr(>F)`[1]),
                  if (equal_var) "✓ equal variances" else "✗ unequal variances"))
    
    assumptions_met <- all_normal & equal_var
    
    if (assumptions_met) {
      
      aov_fit   <- aov(.param ~ .group, data = subdf)
      aov_sum   <- summary(aov_fit)
      omni_p    <- aov_sum[[1]]$`Pr(>F)`[1]
      omni_stat <- aov_sum[[1]]$`F value`[1]
      omni_name <- "One-way ANOVA"
      
      if (verbose)
        cat(sprintf("── %s: F = %s, p = %s  %s\n\n",
                    omni_name,
                    fmt(omni_stat),
                    fmt(omni_p),
                    if (omni_p < alpha) "→ significant, running Tukey HSD" else "→ not significant"))
      
      if (omni_p < alpha) {
        posthoc_raw <- TukeyHSD(aov_fit)
        posthoc_df  <- as.data.frame(posthoc_raw$.group) %>%
          rownames_to_column("comparison") %>%
          rename(mean_diff = diff, lower = lwr, upper = upr, p.value = `p adj`) %>%
          mutate(
            sig_label   = case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01  ~ "**",
              p.value < 0.05  ~ "*",
              p.value < 0.1   ~ ".",
              TRUE            ~ "ns"
            ),
            significant = p.value < alpha,
            posthoc     = "Tukey HSD"
          )
        
        if (!is.null(reference_strain)) {
          if (verbose)
            cat(sprintf("── Filtering to '%s' comparisons only and re-applying BH correction ──\n\n", reference_strain))
          posthoc_df <- posthoc_df %>%
            filter(
              startsWith(comparison, paste0(reference_strain, " - ")) |
                endsWith(comparison, paste0(" - ", reference_strain))
            ) %>%
            mutate(
              p.value     = p.adjust(p.value, method = "BH"),
              sig_label   = case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01  ~ "**",
                p.value < 0.05  ~ "*",
                p.value < 0.1   ~ ".",
                TRUE            ~ "ns"
              ),
              significant = p.value < alpha
            )
        }
        
        if (verbose) {
          cat("── Post-hoc: Tukey HSD ──\n\n")
          print(as.data.frame(posthoc_df %>%
                                select(comparison, mean_diff, p.value, sig_label, significant)))
          cat("\n")
        }
      } else {
        posthoc_df <- NULL
      }
      
    } else {
      
      kw        <- kruskal.test(.param ~ .group, data = subdf)
      omni_p    <- kw$p.value
      omni_stat <- kw$statistic
      omni_name <- "Kruskal-Wallis test"
      
      if (verbose)
        cat(sprintf("── %s: H = %s, p = %s  %s\n\n",
                    omni_name,
                    fmt(omni_stat),
                    fmt(omni_p),
                    if (omni_p < alpha) "→ significant, running Dunn's test" else "→ not significant"))
      
      if (omni_p < alpha) {
        # ── FIX 1: store raw p-values alongside adjusted ──
        dunn       <- dunn.test(subdf$.param, subdf$.group, method = "bh", altp = TRUE)
        posthoc_df <- tibble(
          comparison = dunn$comparisons,
          statistic  = dunn$Z,
          p.raw      = dunn$altP,           # raw unadjusted p-values
          p.value    = dunn$altP.adjusted   # BH adjusted across all comparisons
        ) %>%
          mutate(
            sig_label   = case_when(
              p.value < 0.001 ~ "***",
              p.value < 0.01  ~ "**",
              p.value < 0.05  ~ "*",
              p.value < 0.1   ~ ".",
              TRUE            ~ "ns"
            ),
            significant = p.value < alpha,
            posthoc     = "Dunn's test (BH adjusted)"
          )
        
        if (!is.null(reference_strain)) {
          if (verbose)
            cat(sprintf("── Filtering to '%s' comparisons only and re-applying BH correction ──\n\n", reference_strain))
          posthoc_df <- posthoc_df %>%
            # ── FIX 2: exact match using startsWith/endsWith ──
            filter(
              startsWith(comparison, paste0(reference_strain, " - ")) |
                endsWith(comparison, paste0(" - ", reference_strain))
            ) %>%
            # ── FIX 3: re-adjust from RAW p-values, not already-adjusted ones ──
            mutate(
              p.value     = p.adjust(p.raw, method = "BH"),
              sig_label   = case_when(
                p.value < 0.001 ~ "***",
                p.value < 0.01  ~ "**",
                p.value < 0.05  ~ "*",
                p.value < 0.1   ~ ".",
                TRUE            ~ "ns"
              ),
              significant = p.value < alpha
            ) %>%
            select(-p.raw)
        }
        
        if (verbose) {
          cat("── Post-hoc: Dunn's Test ──\n\n")
          print(as.data.frame(posthoc_df %>%
                                select(comparison, statistic, p.value, sig_label, significant)))
          cat("\n")
        }
      } else {
        posthoc_df <- NULL
      }
    }
    
    list(
      context         = as.list(key),
      groupBy         = group_lab,
      omnibus_test    = omni_name,
      omnibus_stat    = omni_stat,
      omnibus_p       = omni_p,
      assumptions_met = assumptions_met,
      normality       = normality,
      equal_variance  = equal_var,
      posthoc         = posthoc_df
    )
  })
  
  invisible(all_results)
}

# ══════════════════════════════════════════════════════════════════════════════
# FUNCTION: Custom group comparison
# ══════════════════════════════════════════════════════════════════════════════

compareCustomGroups <- function(
    growthData,
    param     = "maxOD",
    strainCol = "mutant_ID",
    mediumCol = "growth_medium",
    tempCol   = "SetTemperature",
    groups,
    alpha     = 0.05,
    verbose   = TRUE
) {
  
  library(car)
  library(dunn.test)
  library(tibble)
  library(dplyr)
  library(purrr)
  
  # safe formatter
  fmt <- function(x) if (is.na(x)) "NA" else sprintf("%.4f", x)
  
  # ── 1. Validate groups ────────────────────────────────────────────────────
  if (!is.list(groups) || length(groups) < 3)
    stop("'groups' must be a list of at least 3 conditions.")
  
  required_fields <- c("strain", "medium", "temp")
  for (i in seq_along(groups)) {
    missing_fields <- setdiff(required_fields, names(groups[[i]]))
    if (length(missing_fields) > 0)
      stop(sprintf("Group %d is missing: %s", i, paste(missing_fields, collapse = ", ")))
  }
  
  # ── 2. Extract data for each group ───────────────────────────────────────
  df <- growthData %>%
    rename(
      .strain = all_of(strainCol),
      .medium = all_of(mediumCol),
      .temp   = all_of(tempCol),
      .param  = all_of(param)
    ) %>%
    filter(!is.na(.param))
  
  # Build a flat data frame with one group label per row
  group_data <- map_dfr(seq_along(groups), function(i) {
    g         <- groups[[i]]
    group_lab <- sprintf("%s × %s × %s°C", g$strain, g$medium, g$temp)
    
    vals <- df %>%
      filter(
        .strain == g$strain,
        .medium == g$medium,
        .temp   == g$temp
      ) %>%
      mutate(.group = factor(group_lab))
    
    if (nrow(vals) == 0)
      warning(sprintf("No data found for group %d: %s", i, group_lab))
    
    vals
  })
  
  if (length(unique(group_data$.group)) < 3)
    stop("Need at least 3 groups with data to compare.")
  
  # ── 3. Print summary of groups ────────────────────────────────────────────
  if (verbose) {
    cat("\n══════════════════════════════════════════════════\n")
    cat(sprintf(" Custom group comparison  |  param: %s\n", param))
    cat("══════════════════════════════════════════════════\n\n")
    cat("   Groups:\n")
    for (g in groups) {
      lab  <- sprintf("%s × %s × %s°C", g$strain, g$medium, g$temp)
      n    <- nrow(filter(group_data, .group == lab))
      mean <- mean(filter(group_data, .group == lab)$.param, na.rm = TRUE)
      cat(sprintf("     %s  (n=%d, mean=%s)\n", lab, n, fmt(mean)))
    }
    cat("\n")
  }
  
  # ── 4. Parametric assumption checks ──────────────────────────────────────
  if (verbose) cat("── Parametric Assumption Checks ──\n\n")
  
  normality <- group_data %>%
    group_by(.group) %>%
    summarise(
      n       = n(),
      sw_stat = if (n() >= 3) shapiro.test(.param)$statistic else NA_real_,
      sw_p    = if (n() >= 3) shapiro.test(.param)$p.value   else NA_real_,
      normal  = if (n() >= 3) shapiro.test(.param)$p.value > alpha else TRUE,
      .groups = "drop"
    )
  
  all_normal <- all(normality$normal, na.rm = TRUE)
  
  if (verbose) {
    cat("   Normality (Shapiro-Wilk per group):\n")
    for (r in seq_len(nrow(normality))) {
      if (is.na(normality$sw_stat[r])) {
        cat(sprintf("     %s: insufficient replicates (n=%d, need ≥ 3)\n",
                    as.character(normality$.group[r]),
                    normality$n[r]))
      } else {
        cat(sprintf("     %s: W = %s, p = %s  %s\n",
                    as.character(normality$.group[r]),
                    fmt(normality$sw_stat[r]),
                    fmt(normality$sw_p[r]),
                    if (isTRUE(normality$normal[r])) "✓ normal" else "✗ non-normal"))
      }
    }
    cat("\n")
  }
  
  lev       <- leveneTest(.param ~ .group, data = group_data)
  equal_var <- lev$`Pr(>F)`[1] > alpha
  
  if (verbose)
    cat(sprintf("   Equal Variance (Levene's test): F = %s, p = %s  %s\n\n",
                fmt(lev$`F value`[1]),
                fmt(lev$`Pr(>F)`[1]),
                if (equal_var) "✓ equal variances" else "✗ unequal variances"))
  
  assumptions_met <- all_normal & equal_var
  
  # ── 5. Omnibus test ───────────────────────────────────────────────────────
  if (assumptions_met) {
    
    aov_fit   <- aov(.param ~ .group, data = group_data)
    aov_sum   <- summary(aov_fit)
    omni_p    <- aov_sum[[1]]$`Pr(>F)`[1]
    omni_stat <- aov_sum[[1]]$`F value`[1]
    omni_name <- "One-way ANOVA"
    
    if (verbose)
      cat(sprintf("── %s: F = %s, p = %s  %s\n\n",
                  omni_name,
                  fmt(omni_stat),
                  fmt(omni_p),
                  if (omni_p < alpha) "→ significant, running Tukey HSD" else "→ not significant"))
    
    if (omni_p < alpha) {
      posthoc_raw <- TukeyHSD(aov_fit)
      posthoc_df  <- as.data.frame(posthoc_raw$.group) %>%
        rownames_to_column("comparison") %>%
        rename(mean_diff = diff, lower = lwr, upper = upr, p.value = `p adj`) %>%
        mutate(
          sig_label   = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01  ~ "**",
            p.value < 0.05  ~ "*",
            p.value < 0.1   ~ ".",
            TRUE            ~ "ns"
          ),
          significant = p.value < alpha,
          posthoc     = "Tukey HSD"
        )
      
      if (verbose) {
        cat("── Post-hoc: Tukey HSD ──\n\n")
        print(as.data.frame(posthoc_df %>%
                              select(comparison, mean_diff, p.value, sig_label, significant)))
        cat("\n")
      }
    } else {
      posthoc_df <- NULL
    }
    
  } else {
    
    kw        <- kruskal.test(.param ~ .group, data = group_data)
    omni_p    <- kw$p.value
    omni_stat <- kw$statistic
    omni_name <- "Kruskal-Wallis test"
    
    if (verbose)
      cat(sprintf("── %s: H = %s, p = %s  %s\n\n",
                  omni_name,
                  fmt(omni_stat),
                  fmt(omni_p),
                  if (omni_p < alpha) "→ significant, running Dunn's test" else "→ not significant"))
    
    if (omni_p < alpha) {
      dunn       <- dunn.test(group_data$.param, group_data$.group, method = "bh", altp = TRUE)
      posthoc_df <- tibble(
        comparison = dunn$comparisons,
        statistic  = dunn$Z,
        p.value    = dunn$altP.adjusted
      ) %>%
        mutate(
          sig_label   = case_when(
            p.value < 0.001 ~ "***",
            p.value < 0.01  ~ "**",
            p.value < 0.05  ~ "*",
            p.value < 0.1   ~ ".",
            TRUE            ~ "ns"
          ),
          significant = p.value < alpha,
          posthoc     = "Dunn's test (BH adjusted)"
        )
      
      if (verbose) {
        cat("── Post-hoc: Dunn's Test ──\n\n")
        print(as.data.frame(posthoc_df %>%
                              select(comparison, statistic, p.value, sig_label, significant)))
        cat("\n")
      }
    } else {
      posthoc_df <- NULL
    }
  }
  
  invisible(list(
    omnibus_test    = omni_name,
    omnibus_stat    = omni_stat,
    omnibus_p       = omni_p,
    assumptions_met = assumptions_met,
    normality       = normality,
    equal_variance  = equal_var,
    posthoc         = posthoc_df
  ))
}

