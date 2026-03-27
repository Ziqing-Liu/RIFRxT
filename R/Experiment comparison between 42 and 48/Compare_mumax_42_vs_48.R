# ============================================================================
# STATISTICAL COMPARISON: µmax at 42°C vs 48°C
# Fix: uses analyseODData()$means (not $data which does not exist)
# ============================================================================

library(tidyverse)
library(grow96)
library(broom)

# ── Plates of interest ───────────────────────────────────────────────────────
plates_to_include <- c("RIFxT42", "RIFxT48")
two_temps_colours <- c("42°C" = "steelblue", "48°C" = "firebrick")


# ============================================================================
# STEP 1 — Extract µmax means per mutant × medium from each plate
# ============================================================================

mumax_data <- plates_to_include %>%
  lapply(function(plate) {
    subset <- data %>% filter(Plate == plate)
    analyseODData(subset)$means %>%                          # $means exists
      mutate(SetTemperature = paste0(unique(subset$SetTemperature), "°C"))
  }) %>%
  bind_rows() %>%
  filter(!is.na(mutant_ID), mutant_ID != "") %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = factor(SetTemperature)
  )

cat("\n--- Data preview ---\n")
print(head(mumax_data))

cat("\n--- Sample sizes ---\n")
print(mumax_data %>% count(SetTemperature, growth_medium))


# ============================================================================
# STEP 2 — Visualisations
# ============================================================================

# ── 2a. Line + point plot ────────────────────────────────────────────────────
plot_line <- mumax_data %>%
  ggplot(aes(x = mutant_ID, y = mumax,
             colour = SetTemperature,
             linetype = growth_medium,
             group = interaction(SetTemperature, growth_medium))) +
  geom_line(linewidth = 1) +
  geom_point(size = 2.5) +
  scale_colour_manual(values = two_temps_colours) +
  labs(
    title    = "µmax at 42°C vs 48°C — per mutant",
    x        = NULL, y = "µmax (per h)",
    colour   = "Temperature", linetype = "Media"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(plot_line)


# ── 2b. Bar chart ────────────────────────────────────────────────────────────
plot_bar <- mumax_data %>%
  ggplot(aes(x = mutant_ID, y = mumax,
             fill = SetTemperature)) +
  geom_col(position = position_dodge(0.8), width = 0.75) +
  scale_fill_manual(values = two_temps_colours) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(
    title = "µmax at 42°C vs 48°C",
    x = NULL, y = "µmax (per h)", fill = "Temperature"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(plot_bar)


# ── 2c. Heatmap ──────────────────────────────────────────────────────────────
plot_heatmap <- mumax_data %>%
  ggplot(aes(x = mutant_ID, y = SetTemperature, fill = mumax)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    colours = colorRampPalette(c("#f7f7f7", "steelblue", "firebrick"))(100),
    name = "µmax"
  ) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(title = "µmax heatmap — 42°C vs 48°C",
       subtitle = "Darker red = faster growth",
       x = NULL, y = NULL) +
  theme_minimal(base_size = 11) +
  theme(axis.text.x  = element_text(angle = 45, hjust = 1, size = 8),
        panel.grid   = element_blank(),
        strip.text   = element_text(face = "bold"),
        legend.position = "right")

print(plot_heatmap)


# ── 2d. Δµmax (48°C − 42°C) ─────────────────────────────────────────────────
delta_data <- mumax_data %>%
  pivot_wider(names_from = SetTemperature, values_from = mumax) %>%
  mutate(delta_mumax = `48°C` - `42°C`)

plot_delta <- delta_data %>%
  ggplot(aes(x = mutant_ID, y = delta_mumax, fill = delta_mumax > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "firebrick"),
    labels = c("TRUE" = "Faster at 48°C", "FALSE" = "Slower at 48°C")
  ) +
  facet_wrap(~growth_medium, ncol = 1) +
  labs(title = "Δµmax (48°C − 42°C) per mutant",
       x = NULL, y = "Δµmax (per h)", fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom")

print(plot_delta)


# ============================================================================
# STEP 3 — Paired comparisons (42°C vs 48°C across mutants)
# ============================================================================

cat("\n\n=== PAIRED COMPARISONS: 42°C vs 48°C ===\n")

wide_mumax <- mumax_data %>%
  group_by(mutant_ID, SetTemperature) %>%
  summarise(mean_mumax = mean(mumax, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = SetTemperature, values_from = mean_mumax)

cat("\n--- Paired Wilcoxon signed-rank test (across all mutants) ---\n")
wilcox_result <- wilcox.test(wide_mumax$`42°C`, wide_mumax$`48°C`,
                             paired = TRUE, exact = FALSE)
print(wilcox_result)

cat("\n--- Paired t-test (across all mutants) ---\n")
ttest_result <- t.test(wide_mumax$`42°C`, wide_mumax$`48°C`,
                       paired = TRUE)
print(ttest_result)


# ── Per-mutant delta ranked by magnitude ─────────────────────────────────────
per_mutant_results <- mumax_data %>%
  pivot_wider(names_from = SetTemperature,
              values_from = mumax,
              names_prefix = "mumax_") %>%
  rename(mumax_42 = `mumax_42°C`, mumax_48 = `mumax_48°C`) %>%
  mutate(delta_mumax = mumax_48 - mumax_42) %>%   # positive = faster at 48°C
  arrange(desc(abs(delta_mumax)))

cat("\n--- Full per-mutant delta µmax ---\n")
print(per_mutant_results, n = Inf)


# ============================================================================
# STEP 4 — Save outputs
# ============================================================================

write_csv(per_mutant_results, "per_mutant_mumax_42vs48.csv")
write_csv(delta_data,         "delta_mumax_42vs48.csv")

cat("\n✓ Files saved:\n")
cat("  • per_mutant_mumax_42vs48.csv\n")
cat("  • delta_mumax_42vs48.csv\n")


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

cat("\nTop 5 mutants with biggest µmax INCREASE at 48°C:\n")
print(per_mutant_results %>%
        filter(delta_mumax > 0) %>%
        arrange(desc(delta_mumax)) %>%
        slice_head(n = 5))

cat("\nTop 5 mutants with biggest µmax DECREASE at 48°C:\n")
print(per_mutant_results %>%
        filter(delta_mumax < 0) %>%
        arrange(delta_mumax) %>%
        slice_head(n = 5))
