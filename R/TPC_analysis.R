library(tidyverse)
library(rTPC)
library(nls.multstart)
library(broom)

### WT test 

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Take the growth rates produced by grow96
tpc_data <- growthAnalysis$pars |>
  transmute(
    strain = as.character(mutant_ID),
    medium = as.character(growth_medium),
    temp = as.numeric(as.character(SetTemperature)),
    mumax = mumax,
    r2 = r2,
    maxOD = maxOD
  )

# Check the prepared data

# Select wild type in LB
wt_lb <- tpc_data |>
  filter(
    strain == "WT",
    medium == "LB",
    is.finite(temp),
    is.finite(mumax),
    mumax >= 0
  )

# Plotting WT 
ggplot(wt_lb, aes(temp, mumax)) +
  geom_point(size = 2) +
  theme_bw() +
  labs(
    title = "WT growth across temperatures",
    x = "Temperature (°C)",
    y = "Growth rate (h⁻¹)"
  )

# Gaussian thermal performance curve
set.seed(123)

fit_wt <- quickfit_tpc(
  data = wt_lb,
  model_name = "gaussian_1987",
  temp = "temp",
  trait = "mumax",
  start_adjusts = 0.5,
  iter = 200
)

# Predict
pred_wt <- tibble(
  temp = seq(
    min(wt_lb$temp),
    max(wt_lb$temp),
    length.out = 200
  )
)

pred_wt$mumax <- predict(
  fit_wt,
  newdata = pred_wt
)

#Plot
ggplot() +
  geom_point(
    data = wt_lb,
    aes(temp, mumax),
    alpha = 0.5
  ) +
  geom_line(
    data = pred_wt,
    aes(temp, mumax),
    colour = "red",
    linewidth = 1
  ) +
  theme_bw() +
  labs(
    title = "WT thermal performance curve - LB",
    x = "Temperature (°C)",
    y = "Growth rate (h⁻¹)"
  )

# Calc 
wt_traits <- calc_params(fit_wt)

print(wt_traits)

# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════
### All curves 
# ══════════════════════════════════════════════════════════════════════════════════════════════════════════════

# Keep finite, non-negative rate estimates
fit_data <- tpc_data |>
  filter(
    is.finite(temp),
    is.finite(mumax),
    mumax >= 0
  )

# Function to fit a curve safely
fit_curve <- function(d) {
  
  if (n_distinct(d$temp) < 6) {
    return(NULL)
  }
  
  tryCatch(
    quickfit_tpc(
      data = d,
      model_name = "gaussian_1987",
      temp = "temp",
      trait = "mumax",
      start_adjusts = 0.5,
      iter = 200
    ),
    error = function(e) {
      message("Fit failed: ", conditionMessage(e))
      NULL
    }
  )
}

# Fit a curve for each strain and medium
all_fits <- fit_data |>
  group_by(strain, medium) |>
  nest() |>
  mutate(
    fit = map(data, fit_curve)
  ) |>
  ungroup()

# Count successful fits
all_fits |>
  summarise(
    successful = sum(!map_lgl(fit, is.null)),
    failed = sum(map_lgl(fit, is.null))
  )

# Prediction

all_predictions <- all_fits |>
  filter(!map_lgl(fit, is.null)) |>
  mutate(
    predictions = map2(
      data,
      fit,
      ~ {
        newdata <- tibble(
          temp = seq(
            min(.x$temp),
            max(.x$temp),
            length.out = 200
          )
        )
        
        newdata$mumax <- predict(
          .y,
          newdata = newdata
        )
        
        newdata
      }
    )
  ) |>
  select(strain, medium, predictions) |>
  unnest(predictions)

# Plot 
ggplot() +
  geom_point(
    data = fit_data,
    aes(temp, mumax),
    alpha = 0.25,
    size = 0.8
  ) +
  geom_line(
    data = all_predictions,
    aes(temp, mumax),
    colour = "red",
    linewidth = 0.8
  ) +
  facet_grid(medium ~ strain) +
  theme_bw() +
  labs(
    title = "Thermal performance curves",
    x = "Temperature (°C)",
    y = "Maximum specific growth rate (µmax)"
  )

#- gaussian_1987
#- sharpeschoolhigh_1981
#- ratkowsky_1983
#- briere2_1999
#The official Fitting many models guide explains the additional fitting procedures.

