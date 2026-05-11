### get_coverage.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr  1 2026 (09:52) 
## Version: 
## Last-Updated: May  5 2026 (20:23) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 49
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
get_coverage <- function(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, th, return_ipw = TRUE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit")) {
    results_combined <- combine_results(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle,return_ipw, return_ice, model_pseudo_outcomes)
    true_value <- true_values[time_horizon == th, risk]
    results_combined[,.(coverage = mean((lower <= true_value) & (upper >= true_value)),
                        bias = mean(estimate - true_value),
                        mse = mean((estimate - true_value)^2),
                        sd = sd(estimate),
                        mean_se = mean(se)),
                     by = list(method, model_pseudo_outcome)]
}


######################################################################
### get_coverage.R ends here
