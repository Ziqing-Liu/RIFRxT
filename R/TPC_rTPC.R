# RIFRxT: exploratory thermal performance curve analysis
# Run after the growthAnalysis object has been created by Master_T.R.
# Or run source('R/TPC_rTPC.R') from the repository root.
# Dependencies: install.packages(c('tidyverse','rTPC','nls.multstart'))
# This analysis is exploratory until OD-based growth/no-growth calls and replicate structure are verified.

required <- c('dplyr','tidyr','purrr','ggplot2','readr','tibble','rTPC','nls.multstart')
missing <- required[!vapply(required, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing)) stop('Install packages first: ', paste(missing, collapse=', '))
if (!exists('growthAnalysis') || is.null(growthAnalysis$pars)) {
  stop('Run processODData(), blankODs() and analyseODData() first; growthAnalysis$pars is missing.')
}

outdir <- 'plots/rTPC'
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Set to FALSE unless the raw OD traces confirm that maxOD < 0.1 means no growth.
# This is a sensitivity-analysis option, not a validated biological threshold.
classify_low_OD_as_zero <- FALSE
minimum_r2 <- 0.9
od_zero_threshold <- 0.1
set.seed(20261019)

required_columns <- c('mutant_ID','growth_medium','SetTemperature','mumax','r2','maxOD')
missing_columns <- setdiff(required_columns, names(growthAnalysis$pars))
if(length(missing_columns)) stop('Missing columns: ', paste(missing_columns, collapse=', '))

tpc_qc <- growthAnalysis$pars |>
  dplyr::mutate(
    temp = as.numeric(as.character(SetTemperature)),
    rate_raw = 60 * as.numeric(mumax), # grow96 Time_min -> per hour; confirm in your installed grow96
    qc_status = dplyr::case_when(
      is.na(temp) ~ 'missing_temperature',
      !is.finite(rate_raw) | !is.finite(r2) ~ 'missing_growth_fit',
      rate_raw < 0 ~ 'negative_rate',
      r2 < minimum_r2 ~ 'low_r2',
      TRUE ~ 'accepted'
    ),
    rate = dplyr::case_when(
      qc_status == 'accepted' ~ rate_raw,
      classify_low_OD_as_zero & is.finite(maxOD) & maxOD < od_zero_threshold ~ 0,
      TRUE ~ NA_real_
    ),
    qc_status = dplyr::if_else(
      classify_low_OD_as_zero & is.finite(maxOD) & maxOD < od_zero_threshold & !is.na(temp),
      'assumed_zero_growth', qc_status)
  )

readr::write_csv(tpc_qc, file.path(outdir, 'well_level_QC.csv'))
qc_summary <- tpc_qc |>
  dplyr::count(mutant_ID, growth_medium, temp, qc_status, name = 'n_wells')
readr::write_csv(qc_summary, file.path(outdir,'QC_summary.csv'))

# Keep only valid rate estimates in primary analysis; missing values are NOT zeros.
tpc_data <- tpc_qc |>
  dplyr::filter(is.finite(rate), is.finite(temp)) |>
  dplyr::select(mutant_ID, growth_medium, temp, rate, dplyr::any_of(c('Replicate','Plate','Well')))

# Identical temperature/strain/medium observations may share plate-level effects.
# AICc here is descriptive, not a correction for pseudoreplication.
# Validate all start/bound values BEFORE nls_multstart calls seq().
# Failed fits are recorded in fitting_diagnostics.csv with their reason.
fit_one <- function(d, model) {
  fail <- function(reason) list(fit=NULL, reason=reason)
  if (nrow(d) < 8L || dplyr::n_distinct(d$temp) < 6L ||
      max(d$rate) <= 0 || stats::sd(d$rate) == 0) {
    return(fail('insufficient observations/temperature variation'))
  }
  tryCatch({
    sv <- rTPC::get_start_vals(d$temp, d$rate, model_name=model)
    lo <- rTPC::get_lower_lims(d$temp, d$rate, model_name=model)
    hi <- rTPC::get_upper_lims(d$temp, d$rate, model_name=model)
    target <- if (model == 'sharpeschoolhigh_1981') c('r_tref','e','eh','th') else c('rmax','topt','a')
    # Some package versions return unnamed vectors. Keep the canonical model order.
    if (length(sv) != length(target) || length(lo) != length(target) || length(hi) != length(target))
      return(fail('wrong parameter-vector length'))
    names(sv) <- names(lo) <- names(hi) <- target
    if (any(!is.finite(lo)) || any(!is.finite(hi)) || any(lo >= hi))
      return(fail(paste('non-finite/invalid model bounds:',
                        paste(names(lo)[!is.finite(lo) | !is.finite(hi) | lo >= hi],collapse=','))))
    # Use finite bounds even if rTPC's estimated starting values are NA/Inf.
    bad <- !is.finite(sv)
    if (any(bad)) sv[bad] <- lo[bad] + (hi[bad] - lo[bad])/2
    if (any(!is.finite(sv))) return(fail('non-finite start after repair'))
    sv <- pmax(lo, pmin(hi, sv))
    # Start within the rTPC-valid parameter region; avoid +/-10 overflowing
    # tiny parameters such as activation energy or growth rate.
    span <- hi-lo
    start_lo <- pmax(lo, sv - 0.2*span)
    start_hi <- pmin(hi, sv + 0.2*span)
    # If starts sit on a bound, use a broader finite range.
    zero_width <- start_lo >= start_hi
    start_lo[zero_width] <- lo[zero_width] + 0.05*span[zero_width]
    start_hi[zero_width] <- hi[zero_width] - 0.05*span[zero_width]
    if (any(!is.finite(start_lo) | !is.finite(start_hi) | start_lo >= start_hi))
      return(fail('invalid starting search interval'))
    args <- list(data=d, iter=if (model == 'sharpeschoolhigh_1981') c(3,3,3,3) else c(4,4,4),
                 start_lower=start_lo, start_upper=start_hi,
                 lower=lo, upper=hi, supp_errors='Y', convergence_count=FALSE)
    fit <- if(model == 'sharpeschoolhigh_1981') {
      do.call(nls.multstart::nls_multstart,
              c(list(formula=rate ~ rTPC::sharpeschoolhigh_1981(
                   temp=temp, r_tref, e, eh, th, tref=30)), args))
    } else if(model == 'gaussian_1987') {
      do.call(nls.multstart::nls_multstart,
              c(list(formula=rate ~ rTPC::gaussian_1987(temp=temp, rmax, topt, a)), args))
    } else stop('Unsupported model')
    if (is.null(fit)) return(fail('no converged candidate fit'))
    list(fit=fit, reason='ok')
  }, error=function(e) fail(conditionMessage(e)))
}

# Two contrasting first-pass models; expand after checking diagnostics.
models <- c('sharpeschoolhigh_1981', 'gaussian_1987')
curves <- tpc_data |>
  dplyr::group_by(mutant_ID, growth_medium) |>
  tidyr::nest() |>
  dplyr::ungroup()
# Using a wider select and nest avoids accidental grouping by replicate.
all_fits <- tidyr::crossing(curves, model=models) |>
  dplyr::mutate(attempt=purrr::map2(data, model, fit_one),
                fit=purrr::map(attempt, 'fit'),
                fit_status=purrr::map_chr(attempt, 'reason'),
                n_observations=purrr::map_int(data,nrow),
                n_temperatures=purrr::map_int(data,~dplyr::n_distinct(.x$temp)),
                converged=!purrr::map_lgl(fit,is.null)) |>
  dplyr::select(-attempt)
readr::write_csv(all_fits |>
  dplyr::select(mutant_ID,growth_medium,model,fit_status,n_observations,
                n_temperatures,converged),
  file.path(outdir,'fitting_diagnostics.csv'))

score <- function(fit) {
  if (is.null(fit)) return(tibble::tibble(AICc=NA_real_, AIC=NA_real_, k=NA_integer_))
  n <- length(stats::residuals(fit))
  k <- attr(stats::logLik(fit),'df')
  a <- stats::AIC(fit)
  tibble::tibble(AICc=if(n>k+1) a+2*k*(k+1)/(n-k-1) else NA_real_,
                 AIC=a, k=as.integer(k))
}

results <- all_fits |>
  dplyr::mutate(score=purrr::map(fit,score)) |>
  tidyr::unnest(score) |>
  dplyr::group_by(mutant_ID,growth_medium) |>
  dplyr::mutate(delta_AICc=if (any(is.finite(AICc))) {
    AICc-min(AICc[is.finite(AICc)])
  } else rep(NA_real_,dplyr::n())) |>
  dplyr::ungroup() |>
  dplyr::mutate(delta_AICc=dplyr::if_else(is.finite(delta_AICc),delta_AICc,NA_real_))

readr::write_csv(results |>
  dplyr::select(-data,-fit), file.path(outdir,'model_comparison.csv'))

best <- results |>
  dplyr::filter(is.finite(AICc)) |>
  dplyr::group_by(mutant_ID,growth_medium) |>
  dplyr::slice_min(AICc,n=1,with_ties=FALSE) |>
  dplyr::ungroup()

get_traits <- function(fit) {
  tryCatch(rTPC::calc_params(fit), error=function(e) tibble::tibble(rmax=NA_real_,topt=NA_real_))
}
traits <- best |>
  dplyr::mutate(traits=purrr::map(fit,get_traits)) |>
  dplyr::select(mutant_ID,growth_medium,model,n_observations,n_temperatures,delta_AICc,traits) |>
  tidyr::unnest(traits)
readr::write_csv(traits,file.path(outdir,'TPC_traits_exploratory.csv'))

# Predictions stay inside the observed range; don't extrapolate CTmin or CTmax.
predict_curve <- function(fit, d) {
  grid <- tibble::tibble(temp=seq(min(d$temp),max(d$temp),length.out=200))
  tryCatch(dplyr::mutate(grid,rate=as.numeric(stats::predict(fit,newdata=grid))),
           error=function(e) tibble::tibble(temp=numeric(),rate=numeric()))
}
predictions <- best |>
  dplyr::mutate(pred=purrr::map2(fit,data,predict_curve)) |>
  dplyr::select(mutant_ID,growth_medium,model,pred) |>
  tidyr::unnest(pred)
readr::write_csv(predictions,file.path(outdir,'TPC_fitted_predictions.csv'))

# One page per medium, faceted by strain.
for (med in unique(tpc_data$growth_medium)) {
  obs <- dplyr::filter(tpc_data,growth_medium==med)
  pred <- dplyr::filter(predictions,growth_medium==med)
  p <- ggplot2::ggplot(obs,ggplot2::aes(x=temp,y=rate)) +
    ggplot2::geom_point(alpha=0.5,size=0.9,position=ggplot2::position_jitter(width=0.08,height=0)) +
    ggplot2::geom_line(data=pred,ggplot2::aes(x=temp,y=rate,group=mutant_ID),
                       inherit.aes=FALSE,colour='firebrick',linewidth=0.6) +
    ggplot2::facet_wrap(~mutant_ID,ncol=5) +
    ggplot2::labs(title=paste('Exploratory thermal performance curves:',med),
                  x='Temperature (°C)',y=expression(mu[max]~'(h'^-1*')')) +
    ggplot2::theme_bw(base_size=9)
  ggplot2::ggsave(file.path(outdir,paste0('TPC_fits_',med,'.pdf')),
                  plot=p,width=14,height=12)
}
message('Finished. Check fitting_diagnostics.csv FIRST; then model_comparison.csv and PDF plots in ',outdir)
message('No-growth calls, biological replicate structure and confidence intervals still require validation.')
