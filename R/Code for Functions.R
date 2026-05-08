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


#facet wrap plot 

plot_facet_wrap <- function(growthAnalysis, file_name, plates_to_include = NULL, 
                            growth_media = c("LB", "M9gluc"),
                            metric = c("maxOD", "mumax"),
                            label_map = NULL) {
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
      .groups = "drop"
    )
  
  y_lab <- if (metric == "maxOD") {
    paste0("OD (blanked) - ", growth_media)
  } else {
    paste0("Maximum growth rate (mumax) - ", growth_media)
  }
  
  r <- ggplot(dat_wrap, aes(
    x     = SetTemperature,
    y     = .data[[metric]],
    group = interaction(Replicate, mutant_ID)
  )) +
    geom_line(linewidth = 0.3, alpha = 0.5) +
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
  
labels_df <- read.csv("design/mutation_names.csv")
my_labels <- setNames(labels_df$old_name, labels_df$new_name)


#auto correlation 


#Significance test between same mutant accross different temp 
compareMutants <- function(
    growthData,
    param            = "maxOD",
    strainCol        = "mutant_ID",
    mediumCol        = "growth_medium",
    tempCol          = "SetTemperature",
    strainsToCompare = NULL,
    media            = c("LB", "M9gluc"),
    temperatures     = NULL,
    pAdjMethod       = "BH",
    alpha            = 0.05,
    removeOutliers   = TRUE,   # TRUE = auto-remove Grubbs outliers
    grubbsAlpha      = 0.05,   # significance threshold for Grubbs test
    verbose          = TRUE
) {
  
  library(outliers)
  
  # ── 1. Validate ──────────────────────────────────────────────────────────────
  required_cols <- c(strainCol, mediumCol, tempCol, param)
  missing_cols  <- setdiff(required_cols, names(growthData))
  if (length(missing_cols) > 0)
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  
  df <- growthData %>%
    rename(
      .strain = all_of(strainCol),
      .medium = all_of(mediumCol),
      .temp   = all_of(tempCol),
      .param  = all_of(param)
    ) %>%
    filter(!is.na(.param), .medium %in% media)
  
  if (!is.null(temperatures))
    df <- df %>% filter(.temp %in% temperatures)
  
  if (!is.null(strainsToCompare)) {
    missing_strains <- setdiff(strainsToCompare, unique(df$.strain))
    if (length(missing_strains) > 0)
      warning(paste("Strains not found:", paste(missing_strains, collapse = ", ")))
    df <- df %>% filter(.strain %in% strainsToCompare)
  }
  
  available_strains <- unique(df$.strain)
  if (length(available_strains) < 2)
    stop("Need at least 2 strains to compare.")
  
  # ── 2. Grubbs outlier detection and removal ──────────────────────────────────
  if (removeOutliers) {
    if (verbose) cat("\n── Grubbs Outlier Check ──\n\n")
    
    outlier_log <- tibble()   # track what gets removed
    
    df <- df %>%
      group_by(.strain, .medium, .temp) %>%
      group_modify(~ {
        vals <- .x$.param
        
        # Need at least 3 values for Grubbs test to work
        if (length(vals) < 3) {
          if (verbose)
            cat(sprintf("   SKIP  %s × %s × %s°C — only %d replicates (need ≥ 3)\n",
                        .y$.strain, .y$.medium, .y$.temp, length(vals)))
          return(.x)
        }
        
        # Run Grubbs test
        g <- grubbs.test(vals)
        
        if (g$p.value < grubbsAlpha) {
          # Identify which value is the outlier (furthest from mean)
          outlier_val <- vals[which.max(abs(vals - mean(vals)))]
          
          if (verbose)
            cat(sprintf(
              "   OUTLIER REMOVED: %s × %s × %s°C — value %.4f (Grubbs p = %.4f)\n",
              .y$.strain, .y$.medium, .y$.temp, outlier_val, g$p.value
            ))
          
          # Remove the outlier row
          .x <- .x %>% filter(.param != outlier_val)
        } else {
          if (verbose)
            cat(sprintf(
              "   OK    %s × %s × %s°C — no outlier (Grubbs p = %.4f)\n",
              .y$.strain, .y$.medium, .y$.temp, g$p.value
            ))
        }
        return(.x)
      }) %>%
      ungroup()
    
    if (verbose) cat("\n")
  }
  
  # ── 3. All pairwise combinations ─────────────────────────────────────────────
  strain_pairs <- combn(as.character(available_strains), 2, simplify = FALSE)
  
  if (verbose) {
    cat(sprintf("── Comparing %d strains: %d pairwise combinations ──\n\n",
                length(available_strains), length(strain_pairs)))
    cat(sprintf("   Strains : %s\n", paste(available_strains, collapse = ", ")))
    cat(sprintf("   Media   : %s\n", paste(media, collapse = ", ")))
    cat(sprintf("   Temps   : %s\n",
                if (is.null(temperatures)) "all" else paste(temperatures, collapse = ", ")))
    cat(sprintf("   Param   : %s\n\n", param))
  }
  
  # ── 4. Run pairwise t-tests ──────────────────────────────────────────────────
  results <- map_dfr(strain_pairs, function(pair) {
    s1 <- pair[1]
    s2 <- pair[2]
    
    df %>%
      group_by(.medium, .temp) %>%
      group_modify(~ {
        vals1 <- .x %>% filter(.strain == s1) %>% pull(.param)
        vals2 <- .x %>% filter(.strain == s2) %>% pull(.param)
        
        if (length(vals1) < 2 | length(vals2) < 2) {
          return(tibble(
            strain1     = s1,
            strain2     = s2,
            n1          = length(vals1),
            n2          = length(vals2),
            mean1       = mean(vals1, na.rm = TRUE),
            mean2       = mean(vals2, na.rm = TRUE),
            fold_change = mean1 / mean2,
            statistic   = NA_real_,
            p.value     = NA_real_
          ))
        }
        
        test <- t.test(vals1, vals2)
        tibble(
          strain1     = s1,
          strain2     = s2,
          n1          = length(vals1),
          n2          = length(vals2),
          mean1       = mean(vals1, na.rm = TRUE),
          mean2       = mean(vals2, na.rm = TRUE),
          fold_change = mean1 / mean2,
          statistic   = test$statistic,
          p.value     = test$p.value
        )
      }) %>%
      ungroup()
  })
  
  # ── 5. Adjust p-values ───────────────────────────────────────────────────────
  results <- results %>%
    rename(growth_medium = .medium, temperature = .temp) %>%
    group_by(growth_medium, temperature) %>%
    mutate(
      p.adj       = p.adjust(p.value, method = pAdjMethod),
      significant = p.adj < alpha,
      sig_label   = case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        p.adj < 0.1   ~ ".",
        TRUE           ~ "ns"
      ),
      comparison  = paste(strain1, "vs", strain2),
      param       = param
    ) %>%
    ungroup() %>%
    select(
      comparison, strain1, strain2, growth_medium, temperature, param,
      n1, n2, mean1, mean2, fold_change,
      statistic, p.value, p.adj, sig_label, significant
    ) %>%
    arrange(growth_medium, temperature, p.adj)
  
  # ── 6. Print summary ─────────────────────────────────────────────────────────
  if (verbose) {
    cat("── Significant pairwise differences ──\n\n")
    sig <- results %>%
      filter(significant) %>%
      select(comparison, growth_medium, temperature, mean1, mean2, fold_change, p.adj, sig_label)
    
    if (nrow(sig) == 0) {
      cat("No significant differences found.\n")
    } else {
      print(sig, n = Inf)
    }
    
    cat(sprintf("\n%d of %d comparisons significant (p.adj < %.2f)\n",
                sum(results$significant, na.rm = TRUE),
                nrow(results),
                alpha))
  }
  
  invisible(results)
}

### t_test 
compareGrowthGroups <- function(
    growthData,
    param              = "mumax",
    strainCol          = "mutant_ID",
    mediumCol          = "growth_medium",
    tempCol            = "SetTemperature",
    baselineStrain     = "WT",
    baselineMedium     = "M9gluc",
    baselineTemp       = 30,
    testMedium         = "M9gluc",
    testTemp           = 45,
    testStrains        = NULL,
    testType           = "auto",
    pAdjMethod         = "BH",
    alpha              = 0.05,
    verbose            = TRUE
) {
  
  # ── 1. Validate columns ──────────────────────────────────────────────────────
  required_cols <- c(strainCol, mediumCol, tempCol, param)
  missing_cols  <- setdiff(required_cols, names(growthData))
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns in data:", paste(missing_cols, collapse = ", ")))
  }
  
  df <- growthData %>%
    rename(
      .strain = all_of(strainCol),
      .medium = all_of(mediumCol),
      .temp   = all_of(tempCol),
      .param  = all_of(param)
    ) %>%
    filter(!is.na(.param))
  
  # ── 2. Reference group (e.g. WT × M9gluc × 30°C) ───────────────────────────
  ref_data <- df %>%
    filter(
      .strain == baselineStrain,
      .medium == baselineMedium,
      .temp   == baselineTemp
    )
  
  if (nrow(ref_data) == 0) {
    stop(sprintf(
      "No data found for baseline: %s × %s × %s°C",
      baselineStrain, baselineMedium, baselineTemp
    ))
  }
  
  # ── 3. Determine strains to test ─────────────────────────────────────────────
  available_strains <- unique(df$.strain)
  
  if (is.null(testStrains)) {
    testStrains <- available_strains
  } else {
    missing_strains <- setdiff(testStrains, available_strains)
    if (length(missing_strains) > 0)
      warning(paste("Strains not found:", paste(missing_strains, collapse = ", ")))
    testStrains <- intersect(testStrains, available_strains)
  }
  
  # ── 4. Test groups (all selected strains × testMedium × testTemp) ────────────
  test_groups <- df %>%
    filter(
      .strain %in% testStrains,
      .medium == testMedium,
      .temp   == testTemp
    )
  
  if (nrow(test_groups) == 0) {
    stop(sprintf(
      "No data found for: %s × %s°C in selected strains.",
      testMedium, testTemp
    ))
  }
  
  # ── 5. Auto test selection via Shapiro-Wilk ──────────────────────────────────
  if (testType == "auto") {
    sw       <- shapiro.test(ref_data$.param)
    testType <- if (sw$p.value > 0.05) "parametric" else "nonparametric"
    if (verbose)
      message(sprintf("[Auto] Shapiro-Wilk p = %.3f → using %s test",
                      sw$p.value, testType))
  }
  
  # ── 6. Pairwise tests vs reference ──────────────────────────────────────────
  results <- test_groups %>%
    group_by(.strain, .medium, .temp) %>%
    group_modify(~ {
      group_vals <- .x$.param
      
      if (length(group_vals) < 2) {
        return(tibble(
          n_test    = length(group_vals),
          n_ref     = nrow(ref_data),
          mean_test = mean(group_vals, na.rm = TRUE),
          mean_ref  = mean(ref_data$.param, na.rm = TRUE),
          statistic = NA_real_,
          p.value   = NA_real_,
          method    = "insufficient replicates"
        ))
      }
      
      if (testType == "parametric") {
        res    <- t.test(group_vals, ref_data$.param)
        method <- "Welch t-test"
      } else {
        res    <- wilcox.test(group_vals, ref_data$.param, exact = FALSE)
        method <- "Wilcoxon rank-sum"
      }
      
      tibble(
        n_test    = length(group_vals),
        n_ref     = nrow(ref_data),
        mean_test = mean(group_vals, na.rm = TRUE),
        mean_ref  = mean(ref_data$.param, na.rm = TRUE),
        statistic = res$statistic,
        p.value   = res$p.value,
        method    = method
      )
    }) %>%
    ungroup()
  
  # ── 7. Adjust p-values and label significance ────────────────────────────────
  results <- results %>%
    mutate(
      p.adj       = p.adjust(p.value, method = pAdjMethod),
      significant = p.adj < alpha,
      sig_label   = case_when(
        p.adj < 0.001 ~ "***",
        p.adj < 0.01  ~ "**",
        p.adj < 0.05  ~ "*",
        p.adj < 0.1   ~ ".",
        TRUE           ~ "ns"
      ),
      reference   = sprintf("%s × %s × %s°C", baselineStrain, baselineMedium, baselineTemp),
      comparison  = sprintf("%s × %s × %s°C", .strain, .medium, .temp),
      param       = param,
      fold_change = mean_test / mean_ref
    ) %>%
    select(
      comparison, reference, param,
      n_test, n_ref,
      mean_test, mean_ref, fold_change,
      statistic, p.value, p.adj,
      sig_label, significant, method
    ) %>%
    arrange(p.adj)
  
  # ── 8. Console summary ───────────────────────────────────────────────────────
  if (verbose) {
    cat("\n══════════════════════════════════════════════════\n")
    cat(sprintf(" Parameter  : %s\n", param))
    cat(sprintf(" Reference  : %s × %s × %s°C  (n=%d)\n",
                baselineStrain, baselineMedium, baselineTemp, nrow(ref_data)))
    cat(sprintf(" Test group : %s × %s°C\n", testMedium, testTemp))
    cat(sprintf(" Test       : %s | p-adj: %s | α = %.2f\n",
                unique(results$method)[1], pAdjMethod, alpha))
    cat("══════════════════════════════════════════════════\n\n")
    print(results %>%
            select(comparison, mean_test, mean_ref, fold_change, p.adj, sig_label))
    cat("\n")
  }
  
  invisible(results)
}


### autocorrelation 
good_curves <- dat_facet |>
  group_by(mutant_ID, Temperature, Replicate, growth_medium) |>
  summarise(
    lag1_acf = if (n() < 3) NA_real_ else acf(OD, lag.max = 1, plot = FALSE)$acf[2, 1, 1],
    .groups = "drop"
  ) |>
  filter(!is.na(lag1_acf), lag1_acf >= 0.90)

dat_facet <- dat_facet |>
  semi_join(good_curves, by = c("mutant_ID", "Temperature", "Replicate", "growth_medium"))