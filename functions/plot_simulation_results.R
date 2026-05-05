### plot_simulation_results.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 31 2026 (12:09) 
## Version: 
## Last-Updated: May  4 2026 (10:41) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 58
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
plot_sims <- function(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, th, remove_ipcw_glm_expit = FALSE) {
    results_rtmle <- results_rtmle[, .(Estimate)]
    results_ice_ipcw <- results_ice_ipcw[, .(estimate, model_pseudo_outcome)]
    results_csc <- results_csc[, .(estimate)]
    results_ice_ipcw_no_tvc <- results_ice_ipcw_no_tvc[, .(estimate, model_pseudo_outcome)]
    results_ice_ipcw_no_tmle <- results_ice_ipcw_no_tmle[, .(estimate, model_pseudo_outcome, ice_ipcw_estimate, ipw)]
    if (remove_ipcw_glm_expit) {
        results_ice_ipcw <- results_ice_ipcw[model_pseudo_outcome != "ipcw_glm_expit"]
    }
    setnames(results_rtmle, "Estimate", "estimate")
    results_rtmle[, method := "RTMLE"]
    results_rtmle[, model_pseudo_outcome := "none"]
    results_ice_ipcw[, method := "ICE-IPCW (tmle)"]
    results_ice_ipcw_no_tvc[, method := "ICE-IPCW (no TVC)"]
    results_ice_ipcw_no_tmle[, method := "ICE-IPCW (one-step)"]
    results_ipw <- copy(results_ice_ipcw_no_tmle)
    results_ipw[, method := "IPW"]
    results_ipw[, estimate := ipw]
    results_ipw[, model_pseudo_outcome := "none"]
    results_ipw[, c("ipw", "ice_ipcw_estimate") := NULL]
    results_ice <- results_ice_ipcw_no_tmle
    results_ice[, method := "ICE-IPCW"]
    results_ice[, estimate := ice_ipcw_estimate]
    results_ice[, c("ipw", "ice_ipcw_estimate") := NULL]
    results_csc[, method := "Cause-specific Cox"]
    results_csc[, model_pseudo_outcome := "none"]
    results_combined <- rbindlist(list(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, results_ipw, results_ice))
    true_value <- true_values[time_horizon == th, risk]
    ggplot(results_combined, aes(y = estimate, x = method, fill = model_pseudo_outcome)) +
        geom_boxplot(outliers = FALSE) +
        geom_hline(yintercept = true_value, linetype = "dashed") 
}


######################################################################
### plot_simulation_results.R ends here
