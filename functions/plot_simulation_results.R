### plot_simulation_results.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 31 2026 (12:09) 
## Version: 
## Last-Updated: Apr 27 2026 (09:56) 
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
plot_sims <- function(results_rtmle, results_ice_ipcw, results_csc, true_values, th, remove_ipcw_glm_expit = FALSE) {
    results_rtmle <- results_rtmle[, .(Estimate)]
    results_ice_ipcw <- results_ice_ipcw[, .(estimate, model_pseudo_outcome)]
    results_csc <- results_csc[, .(estimate)]
    if (remove_ipcw_glm_expit) {
        results_ice_ipcw <- results_ice_ipcw[model_pseudo_outcome != "ipcw_glm_expit"]
    }
    setnames(results_rtmle, "Estimate", "estimate")
    results_rtmle[, method := "RTMLE"]
    results_rtmle[, model_pseudo_outcome := "none"]
    results_ice_ipcw[, method := "ICE-IPCW"]
    results_csc[, method := "Cause-specific Cox"]
    results_csc[, model_pseudo_outcome := "none"]
    results_combined <- rbindlist(list(results_rtmle, results_ice_ipcw, results_csc))
    true_value <- true_values[time_horizon == th, risk]
    ggplot(results_combined, aes(y = estimate, x = method, fill = model_pseudo_outcome)) +
        geom_boxplot(outliers = FALSE) +
        geom_hline(yintercept = true_value, linetype = "dashed") 
}


######################################################################
### plot_simulation_results.R ends here
