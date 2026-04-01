### get_coverage.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr  1 2026 (09:52) 
## Version: 
## Last-Updated: Apr  1 2026 (12:14) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 7
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_coverage <- function(results_rtmle, results_ice_ipcw, true_values, th) {
    results_rtmle <- results_rtmle[, .(Estimate, Standard_error, Lower, Upper)]
    results_ice_ipcw <- results_ice_ipcw[, .(estimate, se, lower, upper, model_pseudo_outcome)]
    setnames(results_rtmle, c("estimate", "se", "lower", "upper"))
    results_rtmle[, method := "RTMLE"]
    results_rtmle[, model_pseudo_outcome := "none"]
    results_ice_ipcw[, method := "ICE-IPCW"]
    results_combined <- rbind(results_rtmle, results_ice_ipcw)
    true_value <- true_values[time_horizon == th, risk]
    results_combined[,.(coverage = mean((lower <= true_value) & (upper >= true_value)),
                        bias = mean(estimate - true_value),
                        mse = mean((estimate - true_value)^2)),
                     by = method]
}


######################################################################
### get_coverage.R ends here
