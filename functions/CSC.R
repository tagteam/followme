### CSC.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr 23 2026 (15:33) 
## Version: 
## Last-Updated: Apr 27 2026 (15:17) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 69
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(data.table)

naive_csc_data <- function(dt, baseline_variables) {
  base_events <- dt[
    (event == "visit" & SGLT2 == 0) | event %in% c("dropout", "mace", "death")
  ]
  base_events[(event == "visit" & SGLT2 == 0) | event == "dropout", event := 0L]
  base_events[event %in% c("mace", "death"), event := fifelse(event == "mace", 1L, 2L)]  

  setkey(base_events, id, time)
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
    } else {
        baseline_formula <- as.formula(paste0("Hist(time, event) ~ ", paste(baseline_variables, collapse = "+")))
    }
    fit <- riskRegression::CSC(as.formula(baseline_formula), data = res)
    estimates <- colMeans(predictRisk(fit, times = time_horizons, cause = cause, newdata = res))
    data.table(time_horizon = time_horizons, estimate = estimates)
}

######################################################################
### CSC.R ends here
