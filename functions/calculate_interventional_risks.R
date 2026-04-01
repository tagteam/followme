### calculate_interventional_risks.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 18 2026 (16:33) 
## Version: 
## Last-Updated: Apr  1 2026 (15:41) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 32
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
calculate_interventional_risks <- function(n,
                                           diabetes_polypharmacy_setting,
                                           intervention,
                                           time_horizons,
                                           terminal_events = c("death", "MACE", "dropout"),
                                           primary_event) {
    out <- list()
    for (treatment in names(intervention)) {
        set_intervention <- function(X){
            X[, (treatment) := intervention[[treatment]]]
            X
        }
        if (is.null(diabetes_polypharmacy_setting$post_baseline_visit_hook)) {
            diabetes_polypharmacy_setting$post_baseline_visit_hook <- set_intervention
        } else {
            old_hook <- diabetes_polypharmacy_setting$post_baseline_visit_hook
            diabetes_polypharmacy_setting$post_baseline_visit_hook <- function(X){
                X <- old_hook(X)
                set_intervention(X)
            }
        }
        intervention_arg <- list()
        intervention_arg[[treatment]] <- intervention[[treatment]]
        treatment_dt <- do.call("simulate_cohort",
                                c(list(n = n),
                                  list(intervention = intervention_arg),
                                  diabetes_polypharmacy_setting))
        treatment_dt[,intervention := treatment]
        out[[treatment]] <- treatment_dt
    }
    d <- rbindlist(out,use.names = TRUE)
    setkeyv(d, c("intervention", "id", "time"))
    data_terminal_events <- d[event %chin% terminal_events, list(time = time[1], event = event[1]), keyby = c("id", "intervention")]
    out <- list()
    for (time_horizon in time_horizons) {
        true_values <- data_terminal_events[, .(risk = mean(time <= time_horizon & event == primary_event)), by = "intervention"]
        true_values[, time_horizon := time_horizon]
        out[[as.character(time_horizon)]] <- true_values
    }
    
    return(rbindlist(out))
}

######################################################################
### calculate_interventional_risks.R ends here
