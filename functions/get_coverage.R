### get_coverage.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr  1 2026 (09:52) 
## Version: 
## Last-Updated: May  4 2026 (10:41) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 39
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_coverage <- function(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, th, remove_ipcw_glm_expit = FALSE) {
    results_rtmle <- results_rtmle[, .(Estimate, Standard_error, Lower, Upper)]
    results_ice_ipcw <- results_ice_ipcw[, .(estimate, se, lower, upper, model_pseudo_outcome)]
    results_csc <- results_csc[, .(estimate)]
    results_ice_ipcw_no_tvc <- results_ice_ipcw_no_tvc[, .(estimate, se, lower, upper, model_pseudo_outcome)]
    results_ice_ipcw_no_tmle <- results_ice_ipcw_no_tmle[, .(estimate, se, lower, upper, model_pseudo_outcome, ice_ipcw_estimate, ipw)]
    if (remove_ipcw_glm_expit) {
        results_ice_ipcw <- results_ice_ipcw[model_pseudo_outcome != "ipcw_glm_expit"]
        results_ice_ipcw_no_tvc <- results_ice_ipcw_no_tvc[model_pseudo_outcome != "ipcw_glm_expit"]
    }
    setnames(results_rtmle, c("Estimate", "Standard_error", "Lower", "Upper"), c("estimate", "se", "lower", "upper"))
    results_rtmle[, method := "RTMLE"]
    results_rtmle[, model_pseudo_outcome := "none"]
    results_ice_ipcw[, method := "ICE-IPCW (tmle)"]
    results_ice_ipcw_no_tvc[, method := "ICE-IPCW (no TVC)"]
    results_ice_ipcw_no_tmle[, method := "ICE-IPCW (one-step)"]
    results_ipw <- copy(results_ice_ipcw_no_tmle)
    results_ipw[, method := "IPW"]
    results_ipw[, estimate := ipw]
    results_ipw[, se := NA_real_]
    results_ipw[, lower := NA_real_]
    results_ipw[, upper := NA_real_]
    results_ipw[, model_pseudo_outcome := "none"]
    results_ipw[, c("ipw", "ice_ipcw_estimate") := NULL]
    results_ice <- results_ice_ipcw_no_tmle
    results_ice[, method := "ICE-IPCW"]
    results_ice[, estimate := ice_ipcw_estimate]
    results_ice[, c("ipw", "ice_ipcw_estimate") := NULL]
    results_ice[, se := NA_real_]
    results_ice[, lower := NA_real_]
    results_ice[, upper := NA_real_]
    results_csc[, method := "Cause-specific Cox"]
    results_csc[, model_pseudo_outcome := "none"]
    results_csc[, se := NA_real_]
    results_csc[, lower := NA_real_]
    results_csc[, upper := NA_real_]
    results_combined <- rbindlist(list(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, results_ipw, results_ice), fill = TRUE)
    true_value <- true_values[time_horizon == th, risk]
    results_combined[,.(coverage = mean((lower <= true_value) & (upper >= true_value)),
                        bias = mean(estimate - true_value),
                        mse = mean((estimate - true_value)^2)),
                     by = list(method, model_pseudo_outcome)]
}


######################################################################
### get_coverage.R ends here
