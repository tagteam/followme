### ice_ipcw.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 16 2026 (11:52) 
## Version: 
## Last-Updated: Apr 30 2026 (13:56) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 259
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

run_ice_ipcw <- function(data,
                         time_horizons,
                         primary_event = "MACE",
                         competing_event = "death",
                         regimens = c("GLP1", "SGLT2", "DPP4"),
                         contrasts = TRUE,
                         contrasts_reference = "SGLT2",
                         penalize_treatment = TRUE,
                         baseline_confounders = c("age", "sex", "HbA1c", "U"),
                         time_confounders = "changeHbA1c",
                         exclude_variables = NULL,
                         model_pseudo_outcomes = "oipcw_expit",
                         lag_propensity = NULL,
                         lag_pseudo_outcome = NULL,
                         K = NULL,
                         verbose = FALSE, ...){  ## arguments to be passed to debias_ice_ipcw
    ## data<-tar_read(diabetes_population_complex_setting_no_percentage)
    ## time_horizons <- 12
    ## primary_event = "mace"
    ##                      competing_event = "death"
    ##                      regimens = "SGLT2"
    ##                      contrasts = TRUE
    ##                      contrasts_reference = NULL
    ##                      penalize_treatment = TRUE
    ##                      baseline_confounders = "sex"
    ##                      time_confounders = NULL
    ##                      exclude_variables = NULL
    ##                      model_pseudo_outcomes = "oipcw_expit"
    ##                      lag_propensity = NULL
    ##                      lag_pseudo_outcome = NULL
    ##                      K = NULL
    ##                      verbose = FALSE
    ## Check if contICEIPCW is installed, if not install it from GitHub
    if (!requireNamespace("contICEIPCW", quietly = TRUE)) {
        ## Check that version of "contICEIPCW" is new enough
        if (packageVersion("contICEIPCW") <= "0.0.9000") {
            requireNamespace("devtools", quietly = TRUE)
            message("Installing contICEIPCW from GitHub...")
            devtools::install_github("jsohlendorff/contICEIPCW")
        }
    }
    ## require(contICEIPCW) ##devtools::install_github("jsohlendorff/contICEIPCW")
    setkeyv(data, c("id", "time"))
    baseline_data <- data[time == 0, c("id", baseline_confounders, regimens), with = FALSE]
    setnames(baseline_data, regimens, paste0(regimens, "_0"))
    timevar_data <- data[time > 0, c("id", "time", "event", time_confounders, regimens), with = FALSE]
    ## Change labels visit, MACE, death, dropout to A, Y, D, C
    timevar_data[event == "visit", event := "A"]
    timevar_data[event == primary_event, event := "Y"]
    if (!is.null(competing_event)){
        timevar_data[event == competing_event, event := "D"]
    }
    timevar_data[event == "dropout", event := "C"]
    ## Remove events after event==Y;
    ## Only first MACE event matters for the analysis.
    ## MACE cannot occur after first event if it was not already mace?
    terminal_time <- timevar_data[event%in% c("C", "D", "Y")][, .(terminal_time = min(time)), by = "id"]
    timevar_data <- merge(timevar_data, terminal_time, by = "id", all.x = TRUE)
    timevar_data <- timevar_data[time <= terminal_time]
    timevar_data[, terminal_time:= NULL]
    timevar_data[, event := as.factor(event)]

    res <- list()
    ## Run ICE-IPCW estimator across all regimens
    for (regimen in regimens){
        other_regimens <- setdiff(regimens, regimen)
        other_regimens_baseline <- paste0(other_regimens, "_0")
        data_regimen <- copy(timevar_data)
        baseline_regimen <- copy(baseline_data)
        setnames(data_regimen, regimen, "A")
        setnames(baseline_regimen, paste0(regimen, "_0"), "A_0")
        prep_data <- prepare_data(
            data = list(baseline_data = baseline_regimen,
                        timevarying_data = data_regimen),
            time_horizons = time_horizons,
            time_covariates = c(time_confounders, "A", other_regimens),
            baseline_covariates =  c(baseline_confounders, "A_0"),
            marginal_censoring = TRUE,
            verbose = verbose,
            last_non_terminal_event = K
        )
        prop_scores <- propensity_scores(
                                        prepared_data = prep_data,
                                        model_treatment = "learn_glm_logistic",
                                        penalize_treatment = penalize_treatment,
                                        model_hazard = "learn_coxph",
                                        verbose = verbose,
                                        exclude_latest_covariate = c(other_regimens, exclude_variables),
                                        lag = lag_propensity
                                    )
        est <- list()
        for (m in model_pseudo_outcomes){
            if (verbose){
                message(paste0("Running ICE-IPCW for regimen ", regimen, " and pseudo-outcome model ", m))
            }
            out <- debias_ice_ipcw(
                                prepared_data = prop_scores,
                                model_hazard = NULL,
                                penalize_hazard = FALSE,
                                conservative = TRUE,
                                static_intervention = 1,
                                return_ic = TRUE,
                                verbose = verbose,
                                model_pseudo_outcome = m,
                                lag = lag_pseudo_outcome,
                                ...)
            out$result[, model_pseudo_outcome := m]
            out$result[, treatment_name := regimen]
            out$treatment_name <- regimen
            est[[m]] <- out
        }
        res[[regimen]] <- est
    }
    results <- rbindlist(lapply(res, function(x) rbindlist(lapply(x, function(y) y$result))))
    if (contrasts) {
        ## FIXME
        stop("Contrasts broken")
        res_contrasts <- do.call(contICEIPCW::compare_to_reference,
                                 c(lapply(res, function(x) x$result), list(reference_name = contrasts_reference)))
    } else {
        res_contrasts <- NULL
    }
    return(list(results = results, contrasts = res_contrasts))
}

######################################################################
### ice_ipcw.R ends here
