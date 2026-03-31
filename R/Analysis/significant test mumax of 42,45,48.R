library(tidyverse)
library(grow96)
library(rstatix)
library(dunn.test)
library(ggpubr)

filter <- dplyr::filter


mumax_data <- growthAnalysis$pars %>%
  filter(
    Plate %in% c("RIFxT42", "RIFxT45", "RIFxT48"),
    !is.na(mutant_ID), mutant_ID != "",
    !is.na(mumax)
  ) %>%
  mutate(
    mutant_ID      = factor(mutant_ID, levels = c("WT", paste0("M", 1:28))),
    SetTemperature = factor(SetTemperature)          # 42 / 45 / 48
  )

# Quick look
glimpse(mumax_data)

# ── 2. Kruskal-Wallis test — does temperature affect mumax? ───────────────────
kw_results <- mumax_data %>%
  group_by(growth_medium) %>%
  kruskal_test(mumax ~ SetTemperature)

print(kw_results)

# ── 3. Dunn's post-hoc pairwise comparisons (Bonferroni correction) ───────────
dunn_results <- mumax_data %>%
  group_by(growth_medium) %>%
  dunn_test(mumax ~ SetTemperature, p.adjust.method = "bonferroni")

print(dunn_results)

# ── 4. Effect size — epsilon-squared (non-parametric η²) ─────────────────────
effect_size <- mumax_data %>%
  group_by(growth_medium) %>%
  kruskal_effsize(mumax ~ SetTemperature)

print(effect_size)

# ── 5. Summary table of median mumax per temperature × medium ─────────────────
summary_stats <- mumax_data %>%
  group_by(growth_medium, SetTemperature) %>%
  summarise(
    n       = n(),
    median  = median(mumax, na.rm = TRUE),
    IQR     = IQR(mumax,    na.rm = TRUE),
    mean    = mean(mumax,   na.rm = TRUE),
    sd      = sd(mumax,     na.rm = TRUE),
    .groups = "drop"
  )

print(summary_stats)

# ── 6. Visualisation — boxplot with significance brackets ─────────────────────
dunn_plot <- dunn_results %>%
  add_xy_position(x = "SetTemperature")

p_box <- ggplot(mumax_data,
                aes(x = SetTemperature, y = mumax, fill = SetTemperature)) +
  geom_boxplot(outlier.shape = 21, outlier.size = 1.5, alpha = 0.7) +
  geom_jitter(width = 0.15, size = 0.8, alpha = 0.4, colour = "grey30") +
  stat_pvalue_manual(
    dunn_plot,
    label      = "p.adj.signif",
    tip.length = 0.02,
    hide.ns    = FALSE
  ) +
  scale_fill_manual(values = c(
    "42" = "steelblue",
    "45" = "darkorange",
    "48" = "firebrick"
  )) +
  facet_wrap(~growth_medium) +
  labs(
    title    = "Effect of Temperature on µmax (Kruskal-Wallis + Dunn post-hoc)",
    subtitle = "Brackets show Bonferroni-adjusted p-values from Dunn's test",
    x        = "Temperature (°C)",
    y        = expression(mu[max]~(h^-1)),
    fill     = "Temp (°C)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    strip.text      = element_text(face = "bold"),
    axis.text.x     = element_text(angle = 0, hjust = 0.5)
  )

print(p_box)
ggsave("temperature_mumax_boxplot.pdf", p_box, width = 10, height = 6)

# ── 7. Per-mutant Kruskal-Wallis ──────────────────────────────────────────────
kw_per_mutant <- mumax_data %>%
  group_by(mutant_ID, growth_medium) %>%
  filter(n_distinct(SetTemperature) == 3) %>%
  kruskal_test(mumax ~ SetTemperature) %>%
  adjust_pvalue(method = "BH") %>%
  add_significance()

print(kw_per_mutant)

# Flag mutants where temperature significantly affects mumax (FDR < 0.05)
sig_mutants <- kw_per_mutant %>%
  filter(p.adj < 0.05) %>%
  select(mutant_ID, growth_medium, statistic, p, p.adj, p.adj.signif)

cat("\n── Mutants with mumax significantly affected by temperature (FDR < 0.05) ──\n")
print(sig_mutants)