library(tidyverse)
library(grow96)
library(rstatix)
library(dunn.test)
library(ggpubr)

filter <- dplyr::filter

# ── 1. Load & pre-process data ────────────────────────────────────────────────
data <- processODData(specPath = "specs", dataPath = "data")
data <- blankODs(data, method = "fixed", value = 0.05)

# ── 2. Extract max OD per well replicate at the three temperatures ────────────
#    Each unique combination of mutant_ID × SetTemperature × growth_medium × well
#    gives one max-OD observation, keeping individual replicates for the test.
maxOD_data <- data %>%
  filter(
    Plate %in% c("RIFxT42", "RIFxT45", "RIFxT48"),
    !is.na(mutant_ID), mutant_ID != "",
    !is.na(blankedOD)
  ) %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = factor(SetTemperature)          # 42 / 45 / 48
  ) %>%
  group_by(mutant_ID, SetTemperature, growth_medium, Well) %>%
  summarise(max_OD = max(blankedOD, na.rm = TRUE), .groups = "drop")

# Quick look
glimpse(maxOD_data)

# ── 3. Kruskal-Wallis test — does temperature affect max OD? ──────────────────
#    Run separately for each growth medium so media doesn't confound the result.
kw_results <- maxOD_data %>%
  group_by(growth_medium) %>%
  kruskal_test(max_OD ~ SetTemperature)

print(kw_results)

# ── 4. Dunn's post-hoc pairwise comparisons (Bonferroni correction) ───────────
dunn_results <- maxOD_data %>%
  group_by(growth_medium) %>%
  dunn_test(max_OD ~ SetTemperature, p.adjust.method = "bonferroni")

print(dunn_results)

# ── 5. Effect size — epsilon-squared (non-parametric η²) ─────────────────────
effect_size <- maxOD_data %>%
  group_by(growth_medium) %>%
  kruskal_effsize(max_OD ~ SetTemperature)

print(effect_size)

# ── 6. Summary table of median max OD per temperature × medium ───────────────
summary_stats <- maxOD_data %>%
  group_by(growth_medium, SetTemperature) %>%
  summarise(
    n        = n(),
    median   = median(max_OD, na.rm = TRUE),
    IQR      = IQR(max_OD,    na.rm = TRUE),
    mean     = mean(max_OD,   na.rm = TRUE),
    sd       = sd(max_OD,     na.rm = TRUE),
    .groups  = "drop"
  )

print(summary_stats)

# ── 7. Visualisation — boxplot with significance brackets ─────────────────────
#    Add Dunn p-value labels onto the plot.
dunn_plot <- dunn_results %>%
  add_xy_position(x = "SetTemperature")

p_box <- ggplot(maxOD_data,
                aes(x = SetTemperature, y = max_OD, fill = SetTemperature)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.7) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.4, colour = "grey30") +
  stat_pvalue_manual(
    dunn_plot,
    label     = "p.adj.signif",   # *, **, *** or ns
    tip.length = 0.02,
    hide.ns   = FALSE
  ) +
  scale_fill_manual(values = c(
    "42" = "steelblue",
    "45" = "darkorange",
    "48" = "firebrick"
  )) +
  facet_wrap(~growth_medium) +
  labs(
    title    = "Effect of Temperature on Max OD (Kruskal-Wallis + Dunn post-hoc)",
    subtitle = "Brackets show Bonferroni-adjusted p-values from Dunn's test",
    x        = "Temperature (°C)",
    y        = "Max OD (blanked)",
    fill     = "Temp (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(face = "bold"),
    axis.text.x      = element_text(angle = 0, hjust = 0.5)
  )

print(p_box)
ggsave("temperature_maxOD_boxplot.pdf", p_box, width = 10, height = 6)

# ── 8. Per-mutant Kruskal-Wallis (optional deep-dive) ────────────────────────
#    Tests whether temperature matters *within* each mutant separately.
kw_per_mutant <- maxOD_data %>%
  group_by(mutant_ID, growth_medium) %>%
  filter(n_distinct(SetTemperature) == 3) %>%   # need all 3 temps present
  kruskal_test(max_OD ~ SetTemperature) %>%
  adjust_pvalue(method = "BH") %>%              # FDR across mutants
  add_significance()

print(kw_per_mutant)

# Flag mutants where temperature has a significant effect (FDR < 0.05)
sig_mutants <- kw_per_mutant %>%
  filter(p.adj < 0.05) %>%
  select(mutant_ID, growth_medium, statistic, p, p.adj, p.adj.signif)

cat("\n── Mutants significantly affected by temperature (FDR < 0.05) ──\n")
print(sig_mutants)
