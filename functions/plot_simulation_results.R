### plot_simulation_results.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 31 2026 (12:09) 
## Version: 
## Last-Updated: May  6 2026 (14:09) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 89
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
plot_sims <- function(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, th, return_ipw = TRUE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit")) {
    results_combined <- combine_results(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, return_ipw, return_ice, model_pseudo_outcomes)
    true_value <- true_values[time_horizon == th, risk]
    ggplot(results_combined, aes(y = estimate, x = method, fill = model_pseudo_outcome)) +
        geom_boxplot(outliers = FALSE, position = position_dodge(width = 0.75)) +
        geom_hline(yintercept = true_value, linetype = "dashed") + ## Nicer name for model_pseudo_outcome
        labs(x = "Method", y = "Estimated risk at time horizon", fill = "Model for iterative regressions") +
        scale_y_continuous(labels = scales::percent) +
        stat_summary(fun = mean, geom = "point", position = position_dodge(width = 0.75)) +
    theme_bw() 
}


######################################################################
### plot_simulation_results.R ends here
