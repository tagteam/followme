### get_coverage.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr  1 2026 (09:52) 
## Version: 
## Last-Updated: Apr 27 2026 (10:06) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 18
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_coverage <- function(results_rtmle, results_ice_ipcw, results_csc, true_values, th, remove_ipcw_glm_expit = FALSE) {
    results_rtmle <- results_rtmle[, .(Estimate, Standard_error, Lower, Upper)]
    results_ice_ipcw <- results_ice_ipcw[, .(estimate, se, lower, upper, model_pseudo_outcome)]
    results_csc <- results_csc[, .(estimate)]
    if (remove_ipcw_glm_expit) {
        results_ice_ipcw <- results_ice_ipcw[model_pseudo_outcome != "ipcw_glm_expit"]
    }
    setnames(results_rtmle, c("Estimate", "Standard_error", "Lower", "Upper"), c("estimate", "se", "lower", "upper"))
    results_rtmle[, method := "RTMLE"]
    results_rtmle[, model_pseudo_outcome := "none"]
    results_ice_ipcw[, method := "ICE-IPCW"]
    results_csc[, method := "Cause-specific Cox"]
    results_csc[, model_pseudo_outcome := "none"]
    results_csc[, se := NA_real_]
    results_csc[, lower := NA_real_]
    results_csc[, upper := NA_real_]
    results_combined <- rbindlist(list(results_rtmle, results_ice_ipcw, results_csc), use.names = TRUE)
    true_value <- true_values[time_horizon == th, risk]
    results_combined[,.(coverage = mean((lower <= true_value) & (upper >= true_value)),
                        bias = mean(estimate - true_value),
                        mse = mean((estimate - true_value)^2)),
                     by = list(method, model_pseudo_outcome)]
}


######################################################################
### get_coverage.R ends here
