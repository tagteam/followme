### CSC.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr 23 2026 (15:33) 
## Version: 
## Last-Updated: Apr 30 2026 (10:12) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 102
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(data.table)

ipcw_estimator <- function(dt, times,baseline_variables = NULL) {
    dt <- naive_csc_data(dt, baseline_variables)
    status <- as.numeric(dt$event)
    time <- dt$time
    # Reverse status: censoring = 1, events = 0
    cens_status <- as.integer(status == 0)

    km_cens <- survfit(Surv(time, cens_status) ~ 1)

    # Function to get G(t-) (left limit)
    G_hat <- function(t) {
        summary(km_cens, times = t, extend = TRUE)$surv
    }
    # Avoid division by zero
    epsilon <- 1e-6

    weights <- 1 / pmax(G_hat(time), epsilon)

    times_grid <- sort(unique(time))

    ipcw_cif <- function(t, cause = 1) {
        mean(weights * (time <= t & status == cause))
    }

    cif_values <- sapply(times, ipcw_cif, cause = 1)
}

naive_csc_data <- function(dt, baseline_variables) {
  setkey(dt, id, time)
  base_events <- dt[
    (event == "visit" & SGLT2 == 0) | event %in% c("dropout", "mace", "death")
  ]
  base_events[(event == "visit" & SGLT2 == 0) | event == "dropout", event := 0L]
  base_events[event %in% c("mace", "death"), event := fifelse(event == "mace", 1L, 2L)]
  base_events[, event := as.numeric(event)]
  
  base_events <- base_events[, .SD[1L], by = id][, .(id, time, event)]

  if (!is.null(baseline_variables)) {
     base_events <- merge(base_events, dt[time == 0, c("id", baseline_variables), with = FALSE], by = "id", all.x = TRUE)
  }

  return(base_events[])
}

run_csc <- function(dt, baseline_variables, time_horizons, cause = 1) {
    res <- naive_csc_data(dt, baseline_variables)
    if (is.null(baseline_variables)) {
        baseline_formula <- "Hist(time, event) ~ 1"
        fit <- prodlim::prodlim(as.formula(baseline_formula), data = res)
        estimates <- predict(fit, times = time_horizons, cause = cause)
        data.table(time_horizon = time_horizons, estimate = estimates)
    } else {
        baseline_formula <- as.formula(paste0("Hist(time, event) ~ ", paste(baseline_variables, collapse = "+")))
        fit <- riskRegression::CSC(as.formula(baseline_formula), data = res)
        estimates <- colMeans(predictRisk(fit, times = time_horizons, cause = cause, newdata = res))
        data.table(time_horizon = time_horizons, estimate = estimates)
    }    
}

######################################################################
### CSC.R ends here
