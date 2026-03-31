### calculate_interventional_risks.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 18 2026 (16:33) 
## Version: 
## Last-Updated: Mar 30 2026 (20:33) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 22
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
            x <- data.table("dummy" = rep(intervention[[1]],NROW(X)))
            setnames(x,"dummy",treatment)
            x
        }
        diabetes_polypharmacy_setting$post_baseline_visit_hook <- set_intervention
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
