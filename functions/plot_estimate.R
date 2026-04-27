### plot_estimate.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 26 2026 (11:16) 
## Version: 
## Last-Updated: Apr 23 2026 (17:31) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 82
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

plot_estimate <- function(estimates_rtmle,
                          estimates_ice_ipcw,
                          estimates_csc,
                          intervals = seq(0, 60, 6),
                          true_values) {

  # --- RTMLE ---
  rtmle_dt <- estimates_rtmle$estimate$Main_analysis[
    , .(Protocol, Time_horizon, Estimate, Lower, Upper)
  ]

  # Map time indices to actual intervals (skip 0)
  time_map <- intervals[-1]
  rtmle_dt[, Time_horizon := time_map[Time_horizon]]
  rtmle_dt[, Type := "RTMLE"]

  # --- ICE-IPCW ---
  ice_dt <- estimates_ice_ipcw$results[
    , .(Estimate = estimate,
        Lower = lower,
        Upper = upper,
        Time_horizon = time_horizon,
        model_pseudo_outcome = model_pseudo_outcome,
        Protocol = treatment_name)
  ]

  ice_dt[, Protocol := paste0("Always_", Protocol)]
  ice_dt[, Type := paste0("ICE-IPCW (", model_pseudo_outcome, ")")]

  # --- IPW ---
  ipw_dt <- estimates_ice_ipcw$results[, c("ipw", "time_horizon", "treatment_name"), with = FALSE]
  setnames(ipw_dt, c("ipw", "time_horizon", "treatment_name"), c("Estimate", "Time_horizon", "Protocol"))
  ipw_dt[, Protocol := paste0("Always_", Protocol)]
  ipw_dt[, Type := "IPW"]
  ipw_dt[, Lower := NA_real_]
  ipw_dt[, Upper := NA_real_]
    
  # --- True values ---
  true_dt <- copy(true_values)
  true_dt[, Protocol := paste0("Always_", intervention)]

  # --- CSC ---
  csc_dt <- estimates_csc[, .(Protocol = true_dt$Protocol[1], Time_horizon = time_horizon, Estimate = estimate)]
  csc_dt[, Type := "Naive Cause-Specific Cox"]
    
  # --- Combine ---
  plot_data <- rbindlist(list(rtmle_dt, ice_dt, ipw_dt, csc_dt), use.names = TRUE, fill = TRUE)

  # --- Plot ---
  ggplot(plot_data, aes(x = Time_horizon, y = Estimate)) +
    geom_line() +
    geom_ribbon(aes(ymin = Lower, ymax = Upper), alpha = 0.2, color = NA) +
    facet_grid(Protocol~Type) +
    theme_bw() +
    geom_line(data = true_dt, aes(x = time_horizon, y = risk), color = "red", linetype = "dashed")
}
######################################################################
### plot_estimate.R ends here
