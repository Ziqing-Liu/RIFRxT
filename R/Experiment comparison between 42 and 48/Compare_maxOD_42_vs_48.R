# ============================================================================
# STATISTICAL COMPARISON: maxOD at 42°C vs 48°C
# Same approach as mumax comparison
# ============================================================================

library(tidyverse)
library(grow96)

# ── Plates of interest ───────────────────────────────────────────────────────
plates_to_include <- c("RIFxT42", "RIFxT48")
two_temps_colours <- c("42°C" = "steelblue", "48°C" = "firebrick")


# ============================================================================
# STEP 1 — Extract maxOD means per mutant × medium from each plate
# ============================================================================

od_data <- plates_to_include %>%
  lapply(function(plate) {
    subset <- data %>% filter(Plate == plate)
    analyseODData(subset)$means %>%
      mutate(SetTemperature = paste0(unique(subset$SetTemperature), "°C"))
  }) %>%
  bind_rows() %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = factor(SetTemperature)
  )

cat("\n--- Data preview ---\n")
print(head(od_data))

cat("\n--- Sample sizes ---\n")
print(od_data %>% count(SetTemperature, growth_medium))


# ============================================================================
# STEP 2 — Visualisations
# ============================================================================

# ── 2a. Line + point plot ────────────────────────────────────────────────────
plot_line <- od_data %>%
  ggplot(aes(x = mutant_ID, y = maxOD,
             colour = SetTemperature,
             linetype = growth_medium,
             group = interaction(SetTemperature, growth_medium))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = two_temps_colours) +
  labs(
    title    = "Max OD at 42°C vs 48°C — per mutant",
    x        = NULL, y = "Max OD",
    colour   = "Temperature", linetype = "Media"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(plot_line)


# ── 2b. Bar chart ────────────────────────────────────────────────────────────
plot_bar <- od_data %>%
  ggplot(aes(x = mutant_ID, y = maxOD,
             fill = SetTemperature)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  scale_fill_manual(values = two_temps_colours) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(
    title = "Max OD at 42°C vs 48°C",
    x = NULL, y = "Max OD", fill = "Temperature"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(plot_bar)


# ── 2c. Heatmap ──────────────────────────────────────────────────────────────
plot_heatmap <- od_data %>%
  ggplot(aes(x = mutant_ID, y = SetTemperature, fill = maxOD)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("#f7f7f7", "steelblue", "firebrick"))(100),
    name = "Max OD"
  ) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(title = "Max OD heatmap — 42°C vs 48°C",
       subtitle = "Darker red = higher yield",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid   = element_blank(),
        strip.text   = element_text(face = "bold"),
        legend.position = "right")

print(plot_heatmap)


# ── 2d. ΔmaxOD (48°C − 42°C) ────────────────────────────────────────────────
delta_data <- od_data %>%
  pivot_wider(names_from = SetTemperature, values_from = maxOD) %>%
  mutate(delta_maxOD = `48°C` - `42°C`)

plot_delta <- delta_data %>%
  ggplot(aes(x = mutant_ID, y = delta_maxOD, fill = delta_maxOD > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
    labels = c("TRUE" = "Higher OD at 48°C", "FALSE" = "Lower OD at 48°C")
  ) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(title = "ΔMax OD (48°C − 42°C) per mutant",
       x = NULL, y = "ΔMax OD", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(plot_delta)


# ============================================================================
# STEP 3 — Paired comparisons (42°C vs 48°C across mutants)
# ============================================================================

cat("\n\n=== PAIRED COMPARISONS: maxOD at 42°C vs 48°C ===\n")

wide_od <- od_data %>%
  group_by(mutant_ID, SetTemperature) %>%
  summarise(mean_maxOD = mean(maxOD, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SetTemperature, values_from = mean_maxOD)

cat("\n--- Paired Wilcoxon signed-rank test (across all mutants) ---\n")
wilcox_result <- wilcox.test(wide_od$`42°C`, wide_od$`48°C`,
                             paired = TRUE, exact = FALSE)
print(wilcox_result)

cat("\n--- Paired t-test (across all mutants) ---\n")
ttest_result <- t.test(wide_od$`42°C`, wide_od$`48°C`,
                       paired = TRUE)
print(ttest_result)


# ── Per-mutant delta ranked by magnitude ─────────────────────────────────────
per_mutant_results <- od_data %>%
  pivot_wider(names_from = SetTemperature,
              values_from = maxOD,
              names_prefix = "maxOD_") %>%
  rename(maxOD_42 = `maxOD_42°C`, maxOD_48 = `maxOD_48°C`) %>%
  mutate(delta_maxOD = maxOD_48 - maxOD_42) %>%   # positive = higher yield at 48°C
  arrange(desc(abs(delta_maxOD)))

cat("\n--- Full per-mutant delta maxOD ---\n")
print(per_mutant_results, n = Inf)


# ============================================================================
# STEP 4 — Save outputs
# ============================================================================

write_csv(per_mutant_results, "per_mutant_maxOD_42vs48.csv")
write_csv(delta_data,         "delta_maxOD_42vs48.csv")

cat("\n✓ Files saved:\n")
cat("  • per_mutant_maxOD_42vs48.csv\n")
cat("  • delta_maxOD_42vs48.csv\n")


# ============================================================================
# STEP 5 — Console summary
# ============================================================================

cat("\n\n=== SUMMARY ===\n")
cat(sprintf("Paired Wilcoxon: W = %.0f, p = %.4f\n",
            wilcox_result$statistic, wilcox_result$p.value))
cat(sprintf("Paired t-test:   t = %.2f, df = %.1f, p = %.4f\n",
            ttest_result$statistic,
            ttest_result$parameter,
            ttest_result$p.value))

cat("\nTop 5 mutants with biggest maxOD INCREASE at 48°C:\n")
print(per_mutant_results %>%
        filter(delta_maxOD > 0) %>%
        arrange(desc(delta_maxOD)) %>%
        slice_head(n = 5))

cat("\nTop 5 mutants with biggest maxOD DECREASE at 48°C:\n")
print(per_mutant_results %>%
        filter(delta_maxOD < 0) %>%
        arrange(delta_maxOD) %>%
        slice_head(n = 5))
