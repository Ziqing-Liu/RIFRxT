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

### test
library(lme4)
library(lmerTest)  # gives p-values for lmer

# Extract per-replicate growth parameters linked to temperature
# (assuming growthAnalysis$data or similar has individual well estimates)

stat_data <- data %>%
  filter(Plate %in% c("RIFxT42", "RIFxT45"),
         !is.na(mutant_ID), mutant_ID != "") %>%
  group_by(mutant_ID, SetTemperature, growth_medium, Plate, Well) %>%
  summarise(maxOD = max(blankedOD, na.rm = TRUE), .groups = "drop")

# Linear mixed model
model <- lmer(maxOD ~ SetTemperature * growth_medium + (1 | mutant_ID) + (1 | Plate),
              data = stat_data)

summary(model)
anova(model)

# ============================================================================
# STATISTICAL ANALYSIS: Temperature Effect on Max OD (42°C vs 45°C)
# ============================================================================

library(lme4)
library(lmerTest)    # install.packages("lmerTest") — adds p-values to lmer
library(emmeans)     # install.packages("emmeans")  — post-hoc comparisons
library(broom.mixed) # install.packages("broom.mixed") — tidy model output


# ── 1. Build per-well summary (if not already done) ─────────────────────────
stat_data <- od_data %>%
  dplyr::filter(Plate %in% c("RIFxT42", "RIFxT45"),
                !is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = factor(SetTemperature)   # 42 vs 45
  ) %>%
  group_by(mutant_ID, SetTemperature, growth_medium, Plate, Well) %>%
  summarise(maxOD = max(OD, na.rm = TRUE), .groups = "drop")


# ── 2. Sanity check ──────────────────────────────────────────────────────────
cat("\n--- Data summary ---\n")
print(summary(stat_data))

cat("\n--- Sample sizes per temperature ---\n")
print(stat_data %>% count(SetTemperature, growth_medium))


# ── 3. Quick check: normality of maxOD ──────────────────────────────────────
# If p > 0.05, data is approximately normal → LMM is appropriate
shapiro_result <- shapiro.test(stat_data$maxOD)
cat("\n--- Shapiro-Wilk normality test on maxOD ---\n")
print(shapiro_result)
# Note: LMM is robust to mild non-normality with large n


# ============================================================================
# ANALYSIS A — Linear Mixed Model (primary, recommended for publication)
# Fixed effects:  SetTemperature, growth_medium, and their interaction
# Random effects: mutant_ID (generalise across mutants), Plate (block effect)
# ============================================================================

cat("\n\n=== LINEAR MIXED MODEL ===\n")

model <- lmer(
  maxOD ~ SetTemperature * growth_medium + (1 | mutant_ID) + (1 | Plate),
  data = stat_data
)

cat("\n--- Model summary ---\n")
print(summary(model))

cat("\n--- ANOVA table (F-tests for fixed effects) ---\n")
print(anova(model))

# Variance explained by random effects
cat("\n--- Random effect variances ---\n")
print(VarCorr(model))


# ── Post-hoc: pairwise temperature comparison within each medium ─────────────
cat("\n--- Post-hoc: 42°C vs 45°C within each growth medium ---\n")
emm <- emmeans(model, pairwise ~ SetTemperature | growth_medium)
print(emm$contrasts)

# Overall temperature effect collapsed across media
cat("\n--- Overall temperature effect (collapsed across media) ---\n")
emm_temp <- emmeans(model, pairwise ~ SetTemperature)
print(emm_temp$contrasts)

# Tidy model output for easy reporting
tidy_model <- tidy(model, effects = "fixed", conf.int = TRUE)
cat("\n--- Tidy fixed effects with 95% CI ---\n")
print(tidy_model)


# ============================================================================
# ANALYSIS B — Paired / per-mutant Wilcoxon (quick, non-parametric check)
# Use this as a supplement or when normality assumption is violated
# ============================================================================

cat("\n\n=== WILCOXON SIGNED-RANK TEST (per mutant, collapsed across media) ===\n")

wilcox_data <- stat_data %>%
  group_by(mutant_ID, SetTemperature) %>%
  summarise(mean_maxOD = mean(maxOD, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SetTemperature,
              values_from = mean_maxOD,
              names_prefix = "temp_")

wilcox_result <- wilcox.test(wilcox_data$temp_42, wilcox_data$temp_45,
                             paired = TRUE, exact = FALSE)
cat("\n--- Wilcoxon result ---\n")
print(wilcox_result)


# ── Per-mutant t-tests with FDR correction ───────────────────────────────────
cat("\n--- Per-mutant t-tests (FDR corrected) ---\n")

per_mutant_tests <- stat_data %>%
  group_by(mutant_ID, growth_medium) %>%
  group_modify(~ {
    d42 <- dplyr::filter(.x, SetTemperature == "42")$maxOD
    d45 <- dplyr::filter(.x, SetTemperature == "45")$maxOD
    
    if (length(d42) < 2 | length(d45) < 2) {
      return(tibble(p.value = NA, estimate = NA))
    }
    
    t_res <- t.test(d42, d45, var.equal = FALSE)
    tibble(
      mean_42  = mean(d42),
      mean_45  = mean(d45),
      delta_OD = mean(d45) - mean(d42),
      p.value  = t_res$p.value
    )
  }) %>%
  ungroup() %>%
  mutate(p.adjusted = p.adjust(p.value, method = "BH")) %>%   # Benjamini-Hochberg FDR
  arrange(p.adjusted)

cat("\n--- Top significant mutants (FDR < 0.05) ---\n")
print(per_mutant_tests %>% dplyr::filter(p.adjusted < 0.05))

cat("\n--- Full per-mutant results ---\n")
print(per_mutant_tests, n = Inf)


# ── Save results to CSV ──────────────────────────────────────────────────────
write_csv(per_mutant_tests, "per_mutant_temperature_tests.csv")
write_csv(tidy_model,       "lmm_fixed_effects.csv")

cat("\n✓ Results saved to per_mutant_temperature_tests.csv and lmm_fixed_effects.csv\n")


# ============================================================================
# SUMMARY TABLE — print to console for quick reporting
# ============================================================================

cat("\n\n=== SUMMARY FOR REPORTING ===\n")
cat("Model: maxOD ~ Temperature × Media + (1|mutant_ID) + (1|Plate)\n\n")

anova_tbl <- anova(model)
cat(sprintf("Temperature effect:          F(%.0f,%.1f) = %.2f, p = %.4f\n",
            anova_tbl["SetTemperature", "NumDF"],
            anova_tbl["SetTemperature", "DenDF"],
            anova_tbl["SetTemperature", "F value"],
            anova_tbl["SetTemperature", "Pr(>F)"]))

cat(sprintf("Media effect:                F(%.0f,%.1f) = %.2f, p = %.4f\n",
            anova_tbl["growth_medium", "NumDF"],
            anova_tbl["growth_medium", "DenDF"],
            anova_tbl["growth_medium", "F value"],
            anova_tbl["growth_medium", "Pr(>F)"]))

cat(sprintf("Temperature × Media interaction: F(%.0f,%.1f) = %.2f, p = %.4f\n",
            anova_tbl["SetTemperature:growth_medium", "NumDF"],
            anova_tbl["SetTemperature:growth_medium", "DenDF"],
            anova_tbl["SetTemperature:growth_medium", "F value"],
            anova_tbl["SetTemperature:growth_medium", "Pr(>F)"]))
