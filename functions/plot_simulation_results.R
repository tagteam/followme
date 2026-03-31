### plot_simulation_results.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 31 2026 (12:09) 
## Version: 
## Last-Updated: Mar 31 2026 (12:12) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 6
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
plot_sims <- function(results_rtmle, results_ice_ipcw, true_values) {
    results_rtmle <- results_rtmle[, .(Estimate, Standard_error, Lower, Upper)]
    results_ice_ipcw <- results_ice_ipcw[, .(estimate, se, lower, upper)]
    setnames(results_rtmle, c("estimate", "se", "lower", "upper"))
    results_rtmle[, method := "RTMLE"]
    results_ice_ipcw[, method := "ICE-IPCW"]
    results_combined <- rbind(results_rtmle, results_ice_ipcw)
    true_value <- true_values[time_horizon == 54, risk]
    ggplot(results_combined, aes(y = estimate, x = method)) +
        geom_boxplot() +
        geom_hline(yintercept = true_value, linetype = "dashed")
}


######################################################################
### plot_simulation_results.R ends here
