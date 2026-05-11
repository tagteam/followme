## _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: May 11 2026 (10:04) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 1653
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
### _targets.R --- 
library(targets)
library(tarchetypes)
library(crew)
tar_source("functions")

if (dir.exists("/projects/biostat01/people/snf991/followme")) {
    library(clustermq)
    ## library(crew.cluster)
    ## controller <- crew_controller_slurm(
    ##     workers = 64,
    ##     seconds_idle = 15,
    ##     options_cluster = crew_options_slurm(
    ##         partition = "long",
    ##         cpus_per_task = 1,
    ##         log_output = "crew_log_%A.txt",
    ##         log_error = "crew_log_%A.txt",
    ##         verbose = TRUE,
    ##         script_lines = c(
    ##             "export OMP_NUM_THREADS=1",
    ##             "export OPENBLAS_NUM_THREADS=1",
    ##             "export MKL_NUM_THREADS=1",
    ##             "export VECLIB_MAXIMUM_THREADS=1",
    ##             "export NUMEXPR_NUM_THREADS=1"
    ##         )
    ##     ) # Start on markov
    ## )
    options(clustermq.scheduler = "slurm",
            clustermq.template = "/projects/biostat01/people/snf991/followme/clu_temp",
            clustermq.defaults = list(log_file="slurm.log", job_name = "continuous_time_simulation"))
    controller <- NULL
} else {
    controller <- crew_controller_local(workers = 4,
                                        options_local = crew_options_local(log_directory = "log"))
}
tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach","ggplot2","plotly", "contICEIPCW", "purrr", "riskRegression", "prodlim", "dplyr", "tidyr", "gt", "paletteer"),
               controller = controller, error="null")

## install rest of packages if not already installed:
## install.packages(c("targets", "tarchetypes", "crew", "crew.cluster", "lava", "survival", "data.table", "prodlim", "foreach", "ggplot2", "plotly", "glmnet", "targets", "tarchetypes", "crew", "crew.cluster", "devtools"))
# Install contICEIPCW from GitHub if not already installed:
## devtools::install_github("jsohlendorff/contICEIPCW")
# Install RTMLE from GitHub if not already installed:
## devtools::install_github("tagteam/RTMLE")

effect_SGLT2_mace <- -0.6
effect_changeHbA1c_SGLT2 <- 0.5 ## Maybe increase?
effect_SGLT2_changeHbA1c <- 0.4

K_vary <- seq(0, 7, by = 1)
n_values <- c(200, 500, 1000, 2000)
time_horizon <- 12
intervals <- c(6, time_horizon)
cue_test <- tar_cue(mode = "never")
cue_true <- tar_cue(mode = "never")
cue_sim <- tar_cue(mode = "never")
scenario_names <- c(
    "increase_censoring", #REALLY: higher censoring
    "effect_outcome", #no censoring
    "confounding_effect_outcome", #no censoring
    "confounding_no_effect_outcome", #no censoring
    "less_visits", #no censoring
    "less_visits_sd_large", #no censoring
    "complex_setting", #
    "complex_setting_more_visits"
)
## NOTE: Dropout = right-censoring
scenarios <- tibble::tibble(
                         scenario = scenario_names,
                         modify_fn = list(
                             function(dps) {
                                 dps$parameter_values$scale_dropout <- 0.0002*20
                                 dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
                                 dps
                             },
                             function(dps) {
                                 dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
                                 dps$absorbing_events$dropout <- NULL
                                 dps
                             },
                             function(dps) {
                                 dps$parameter_values$effect_changeHbA1c_SGLT2 <- -0.15
                                 dps$parameter_values$effect_changeHbA1c_mace <- 0.5
                                 dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
                                 dps$absorbing_events$dropout <- NULL
                                 dps
                             },
                             function(dps) {
                                 dps$parameter_values$effect_changeHbA1c_SGLT2 <- -0.15
                                 dps$parameter_values$effect_changeHbA1c_mace <- 0.5
                                 dps$parameter_values$effect_SGLT2_mace <- 0
                                 dps$absorbing_events$dropout <- NULL
                                 dps
                             },
                             function(dps) {
                                 dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
                                 dps$visit_schedule$mean <- 6
                                 dps$visit_schedule$sd <- 0
                                 dps$absorbing_events$dropout <- NULL
                                 dps
                             },
                             function(dps) {
                                 dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
                                 dps$visit_schedule$mean <- 6
                                 dps$visit_schedule$sd <- 1.6
                                 dps$absorbing_events$dropout <- NULL
                                 dps
                             },
                             function(dps) {
                                 dps$parameter_values$scale_dropout <- 0.00225
                                 dps
                             },
                             function(dps) {
                                 dps$parameter_values$scale_dropout <- 0.00225
                                 dps$visit_schedule$mean <- 1.5
                                 dps
                             }
                         ),
                         complex = list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
                     )

modify_dps <- function(complex = FALSE, modify_fn) {
    dps <- get_diabetes_simulation_setting(complex = complex)
    modify_fn(dps)
}
sim_n_vary_template <- function(diabetes_polypharmacy_setting, n_values, time_horizon, complex = TRUE) {
    tmp <- list()
    
    for (n_val in n_values){
        message(paste0("Running simulation with n = ", n_val))

        # --- Data generating process ---
        diabetes_population <- do.call(
            "simulate_cohort",
            c(list(n = n_val), diabetes_polypharmacy_setting)
        )

        ice_ipcw <- tryCatch(
            {
                out <- run_ice_ipcw(
                    data = diabetes_population,
                    time_horizons = time_horizon,
                    regimens = "SGLT2",
                    model_pseudo_outcomes = c("oipcw_expit", "lm"),
                    penalize_pseudo_outcome = FALSE,
                    primary_event = "mace",
                    contrasts = FALSE,
                    competing_event = "death",
                    penalize_treatment = FALSE,
                    time_confounders = "changeHbA1c",
                    exclude_variables = NULL,
                    baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                    verbose = FALSE,
                    tmle_update = TRUE,
                    lag_propensity = 1,
                    return_ic = FALSE
                )$results
                
                out[, n := n_val]
                out
            },
            error = function(e) {
                message("Error in ice_ipcw for n = ", n_val, ": ", e$message)
                NULL
            }
        )

        ice_ipcw_no_tmle <- tryCatch(
            {
                out <- run_ice_ipcw(
                    data = diabetes_population,
                    time_horizons = time_horizon,
                    regimens = "SGLT2",
                    model_pseudo_outcomes = c("oipcw_expit", "lm"),
                    penalize_pseudo_outcome = FALSE,
                    primary_event = "mace",
                    contrasts = FALSE,
                    competing_event = "death",
                    penalize_treatment = FALSE,
                    time_confounders = "changeHbA1c",
                    exclude_variables = NULL,
                    baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                    verbose = FALSE,
                    tmle_update = FALSE,
                    return_ic = FALSE
                )$results
                
                out[, n := n_val]
                out
            },
            error = function(e) {
                message("Error in ice_ipcw_no_tmle for n = ", n_val, ": ", e$message)
                NULL
            }
        )
        
        tmp[[as.character(n_val)]] <- list(
            ice_ipcw = ice_ipcw,
            ice_ipcw_no_tmle = ice_ipcw_no_tmle
        )
    }

    ice_ipcw <- purrr::map_dfr(tmp, "ice_ipcw")
    ice_ipcw_no_tmle <- purrr::map_dfr(tmp, "ice_ipcw_no_tmle")

    list(
        ice_ipcw = ice_ipcw,
        ice_ipcw_no_tmle = ice_ipcw_no_tmle
    )
}

collect_results_n_vary <- function(sim_results) {
    results_ice_ipcw_tmle <- sim_results |>
        purrr::map(~ purrr::map(.x, "ice_ipcw")) |>  # reach ice_ipcw
        purrr::list_flatten() |>                     # remove one level
        purrr::list_flatten() |>
        rbindlist()
    results_ice_ipcw_no_tmle <- sim_results |>
        purrr::map(~ purrr::map(.x, "ice_ipcw_no_tmle")) |>  # reach ice_ipcw
        purrr::list_flatten() |>                     # remove one level
        purrr::list_flatten() |>
        rbindlist()
    results_ice_ipcw_tmle[, method := "ICE-IPCW (tmle)"]
    results_ice_ipcw_no_tmle[, method := "ICE-IPCW (one-step)"]
    results_ice_ipcw <- copy(results_ice_ipcw_no_tmle)
    results_ice_ipcw[, method := "ICE-IPCW"]
    results_ice_ipcw[, estimate := ice_ipcw_estimate]
    results_ice_ipcw[, c("se", "lower", "upper") := NA]
    results <- rbindlist(list(results_ice_ipcw_tmle, results_ice_ipcw_no_tmle, results_ice_ipcw))
    results[, ice_ipcw_estimate := NULL]
    results
}                


unconfound_true_values <- function(diabetes_polypharmacy_setting) {
    dps <- diabetes_polypharmacy_setting
    ## dps$parameter_values$effect_changeHbA1c_SGLT2 <- 0
    dps$parameter_values$effect_changeHbA1c_mace <- 0
    calculate_interventional_risks(
        n = 500000, 
        diabetes_polypharmacy_setting = dps,
        intervention = list("SGLT2" = 1),
        time_horizons = intervals,
        terminal_events = c("death", "mace", "dropout"),
        primary_event = "mace"
    )
}

## Redo n,k, complex_settings
list(
    tar_map(
        values = scenarios,
        names = scenario,

        ## -- Get the diabetes polypharmacy setting  ---
        tar_target(
            diabetes_polypharmacy_setting,
            modify_dps(complex = complex, modify_fn = modify_fn)
        ),

        ## --- Simulate a large cohort for running methods ---
        tar_target(
            diabetes_population,
            do.call(
                "simulate_cohort",
                c(list(n = 30000), diabetes_polypharmacy_setting)
            ),
            cue = cue_test
        ),

        ## --- Calculate true values for the interventional risks ---
        tar_target(
            true_values,
            calculate_interventional_risks(
                n = 5000000, 
                diabetes_polypharmacy_setting = diabetes_polypharmacy_setting,
                intervention = list("SGLT2" = 1),
                time_horizons = intervals,
                terminal_events = c("death", "mace", "dropout"),
                primary_event = "mace"
            ),
            cue = cue_true
        ),

        ## --- Calculate true values for the interventional risks ---
        tar_target(
            true_values_unconfounded,
            unconfound_true_values(diabetes_polypharmacy_setting)
        ),
        
        ## --- Plot dropout curves for the simulated population ---
        tar_target(
            dropout_plot,
            plot_dropout(diabetes_population),
            cue = cue_test
        ),

        ## --- RTMLE estimator on a single simulated dataset ---
        tar_target(
            rtmle,
            run_rtmle_diabetes_population(
                diabetes_population = diabetes_population,
                time_horizons = intervals,
                intervals = c(0, intervals),
                learner = "learn_glmnet",
                regimens = "SGLT2",
                tv_covs = c("changeHbA1c", "SGLT2"),
                exclusion_rules = NULL,
                baseline_covs = if (complex) c("sex", "HbA1c") else "HbA1c",
                name_outcome = "mace",
                name_competing = "death",
                names_intermediate = NULL,
                treatment_format = "date_value",
                method_covariate_discretization = "locf",
                method_treatment_discretization = "locf"
            ),
            cue = cue_test
        ),

        ## --- ICE-IPCW estimator on a single simulated dataset ---
        tar_target(
            ice_ipcw,
            run_ice_ipcw(
                data = diabetes_population,
                time_horizons = intervals,
                regimens = "SGLT2",
                model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit"),
                penalize_pseudo_outcome = FALSE,
                primary_event = "mace",
                contrasts = FALSE,
                competing_event = "death",
                penalize_treatment = FALSE,
                time_confounders = "changeHbA1c",
                exclude_variables = NULL,
                baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                verbose = TRUE,
                tmle_update = TRUE,
                lag_propensity = 1
            ),
            cue = cue_test
        ),
        ## --- ICE-IPCW estimator without time-varying confounders on a single simulated dataset ---
        tar_target(
            ice_ipcw_no_tvc,
            run_ice_ipcw(
                data = diabetes_population,
                time_horizons = intervals,
                regimens = "SGLT2",
                model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit"),
                penalize_pseudo_outcome = FALSE,
                primary_event = "mace",
                contrasts = FALSE,
                competing_event = "death",
                penalize_treatment = FALSE,
                time_confounders = NULL,
                exclude_variables = NULL,
                baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                verbose = TRUE,
                tmle_update = TRUE,
                lag_propensity = 1
            ),
            cue = cue_test
        ),

        ## --- ICE-IPCW estimator without TMLE update on a single simulated dataset ---
        tar_target(
            ice_ipcw_no_tmle,
            run_ice_ipcw(
                data = diabetes_population,
                time_horizons = intervals,
                regimens = "SGLT2",
                model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit"),
                penalize_pseudo_outcome = FALSE,
                primary_event = "mace",
                contrasts = FALSE,
                competing_event = "death",
                penalize_treatment = FALSE,
                time_confounders = "changeHbA1c",
                exclude_variables = NULL,
                baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                verbose = TRUE,
                tmle_update = FALSE,
                lag_propensity = 1
            ),
            cue = cue_test
        ),
        
        
        ## --- Cause-Specific Cox estimator on a single simulated dataset ---
        tar_target(
            csc,
            run_csc(
                dt = diabetes_population,
                baseline_variables = if (complex) c("sex", "HbA1c") else "HbA1c",
                time_horizons = intervals,
                cause = 1
            ),
            cue = cue_test
        ),
        
        ## --- Observational parameters
        tar_target(
            aalen_johansen,
            get_aalen_johansen(
                dt = diabetes_population,
                times = intervals,
                cause = 1
            ),
            cue = cue_test
        ),
        
        ## --- Plotting estimates against true values ---
        tar_target(
            plot,
            plot_estimate(
                estimates_rtmle = rtmle,
                estimates_ice_ipcw = ice_ipcw,
                estimates_csc = csc,
                estimates_aj = aalen_johansen,
                estimates_ice_ipcw_no_tvc = ice_ipcw_no_tvc,
                estimates_ice_ipcw_no_tmle = ice_ipcw_no_tmle,
                intervals = c(0, intervals),
                true_values = true_values)
        ),

        ## --- Observational check (how many are right-censored?)
        ## tar_target(
        ##     check_censoring,
        ##     {
        ## dps<-diabetes_polypharmacy_setting_complex_setting
        ##  dps$parameter_values$scale_dropout <- 0.0025
        ## d<- do.call(
        ##         "simulate_cohort",
        ##         c(list(n = 30000), dps)
        ##     )[event %in% c("dropout", "mace", "death")]
        ##         d[event == "dropout" & time == 15, event := 2]
        ##         d[event == "dropout"& time<15, event := 1]
        ##         d[event == "mace", event := 2]
        ##         d[event == "death", event := 2]
        ##         ## Plot the probability of event ==0 within t
        ##         plot(prodlim::prodlim(prodlim::Hist(time,event)~1,data = d),cause = 1)
        ##     }
        ## ),

        ## --- Simulations ---
        tar_rep(
            sim,
            {
                # --- Data generating process ---
                diabetes_population <- do.call(
                    "simulate_cohort",
                    c(list(n = 2000), diabetes_polypharmacy_setting)
                )

                # --- Estimators ---
                rtmle <- run_rtmle_diabetes_population(
                    diabetes_population = diabetes_population,
                    time_horizons = time_horizon,
                    intervals = c(0, intervals),
                    regimens = "SGLT2",
                    tv_covs = c("changeHbA1c", "SGLT2"),
                    exclusion_rules = NULL,
                    baseline_covs = if (complex) c("sex", "HbA1c") else "HbA1c",
                    name_outcome = "mace",
                    name_competing = "death",
                    names_intermediate = NULL,
                    treatment_discretization_scheme = "locf",
                    learner = "learn_glmnet"
                )
                rtmle <- summary(rtmle)

                ice_ipcw <- run_ice_ipcw(
                    data = diabetes_population,
                    time_horizons = time_horizon,
                    regimens = "SGLT2",
                    model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit"),
                    penalize_pseudo_outcome = FALSE,
                    primary_event = "mace",
                    contrasts = FALSE,
                    competing_event = "death",
                    penalize_treatment = FALSE,
                    time_confounders = "changeHbA1c",
                    exclude_variables = NULL,
                    baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                    verbose = TRUE,
                    tmle_update = TRUE,
                    lag_propensity = 1
                )

                csc <- run_csc(
                    dt = diabetes_population,
                    baseline_variables = if (complex) c("sex", "HbA1c") else "HbA1c",
                    time_horizons = time_horizon,
                    cause = 1
                )

                ice_ipcw_no_tvc <- run_ice_ipcw(
                    data = diabetes_population,
                    time_horizons = time_horizon,
                    regimens = "SGLT2",
                    model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit"),
                    penalize_pseudo_outcome = FALSE,
                    primary_event = "mace",
                    contrasts = FALSE,
                    competing_event = "death",
                    penalize_treatment = FALSE,
                    time_confounders = NULL,
                    exclude_variables = NULL,
                    baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                    verbose = TRUE,
                    tmle_update = TRUE,
                    lag_propensity = 1
                )

                ice_ipcw_no_tmle <- run_ice_ipcw(
                    data = diabetes_population,
                    time_horizons = time_horizon,
                    regimens = "SGLT2",
                    model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit"),
                    penalize_pseudo_outcome = FALSE,
                    primary_event = "mace",
                    contrasts = FALSE,
                    competing_event = "death",
                    penalize_treatment = FALSE,
                    time_confounders = "changeHbA1c",
                    exclude_variables = NULL,
                    baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                    verbose = TRUE,
                    tmle_update = FALSE
                )

                list(
                    rtmle = rtmle,
                    ice_ipcw = ice_ipcw,
                    csc = csc,
                    ice_ipcw_no_tvc = ice_ipcw_no_tvc,
                    ice_ipcw_no_tmle = ice_ipcw_no_tmle
                )
            },
            iteration = "list",
            reps = 100, 
            batches = 100,
            cue = cue_sim
        ),

        ## --- Summarize simulation results ---
        tar_target(
            results_rtmle,
            purrr::map_dfr(sim, ~ map_dfr(.x, "rtmle"))
        ),

        tar_target(
            results_ice_ipcw,
            purrr::map_dfr(sim, ~ map_dfr(.x, c("ice_ipcw", "results")))
        ),

        tar_target(
            results_csc,
            purrr::map_dfr(sim, ~ map_dfr(.x, "csc"))
        ),

        tar_target(
            results_ice_ipcw_no_tvc,
            purrr::map_dfr(sim, ~ map_dfr(.x, c("ice_ipcw_no_tvc", "results")))
        ),

        tar_target(
            results_ice_ipcw_no_tmle,
            purrr::map_dfr(sim, ~ map_dfr(.x, c("ice_ipcw_no_tmle", "results")))
        ),

        # --- Plot simulation results ---
        tar_target(
            plot_simulation_results, plot_sims(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, time_horizon)
        ),

        # --- Essential plot
        tar_target(
            plot_simulation_results_essential, plot_sims(results_rtmle, results_ice_ipcw, NULL, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, time_horizon, return_ipw = FALSE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm"))
        ),

        ## --- Coverage, MSE, Bias ---
        tar_target(
            summary_simulation_results,
            get_coverage(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, time_horizon)
        )
    ),
    ## --- Vary K in ICE-IPCW ---
    tar_rep(
        sim_K_vary,
        {
            complex <- TRUE
            diabetes_population <- do.call(
                "simulate_cohort",
                c(list(n = 2000), diabetes_polypharmacy_setting_complex_setting_more_visits)
            )

            results <- list()
            for (K in K_vary){
                message(paste0("Running ICE-IPCW with K = ", K))
                out <- run_ice_ipcw(
                    data = diabetes_population,
                    time_horizons = time_horizon,
                    regimens = "SGLT2",
                    model_pseudo_outcomes = c("oipcw_expit", "lm", "ipcw_glm_expit"),
                    penalize_pseudo_outcome = FALSE,
                    primary_event = "mace",
                    contrasts = FALSE,
                    competing_event = "death",
                    penalize_treatment = FALSE,
                    time_confounders = "changeHbA1c",
                    exclude_variables = NULL,
                    baseline_confounders = if (complex) c("sex", "HbA1c") else "HbA1c",
                    verbose = TRUE,
                    tmle_update = TRUE,
                    lag_propensity = 1,
                    K = K)$results
                out[, K := K]
                results[[as.character(K)]] <- out
            }
            list(
                ice_ipcw = results
            )
        },
        iteration = "list",
        reps = 100, 
        batches = 100,
        cue = cue_sim
    ),
    tar_rep(
        sim_n_vary_complex_setting,
        sim_n_vary_template(
            diabetes_polypharmacy_setting = diabetes_polypharmacy_setting_complex_setting,
            n_values = n_values,
            time_horizon = time_horizon,
            complex = TRUE
        ),
        iteration = "list",
        reps = 100, 
        batches = 100,
        cue = cue_sim
    ),
    tar_rep(
        sim_n_vary_complex_setting_more_visits,
        sim_n_vary_template(
            diabetes_polypharmacy_setting = diabetes_polypharmacy_setting_complex_setting_more_visits,
            n_values = n_values,
            time_horizon = time_horizon,
            complex = TRUE
        ),
        iteration = "list",
        reps = 100,
        batches = 100,
        cue = cue_sim
    ),
    tar_rep(
        sim_n_vary_increase_censoring,
        sim_n_vary_template(
            diabetes_polypharmacy_setting = diabetes_polypharmacy_setting_increase_censoring,
            n_values = n_values,
            time_horizon = time_horizon,
            complex = FALSE
        ),
        iteration = "list",
        reps = 100,
        batches = 100,
        cue = cue_sim
    ),
    tar_target(
        combine_K_results,
        {
            all_dfs <- sim_K_vary |>
                map(~ map(.x, "ice_ipcw")) |>  # reach ice_ipcw
                flatten() |>                   # remove one level
                flatten()                      # remove second level (dfs)
            results_ice_ipcw_K <- rbindlist(all_dfs)
            results_ice_ipcw_test <- combine_n_results_complex_setting_more_visits[method == "ICE-IPCW (tmle)" & n == 2000]
            results_ice_ipcw_test[, K := "adaptive"]
            rbind(results_ice_ipcw_K, results_ice_ipcw_test, fill = TRUE)
        }
    ),
    tar_target(
        combine_n_results_increase_censoring,
        collect_results_n_vary(sim_n_vary_increase_censoring)
    ),
    tar_target(
        combine_n_results_complex_setting_more_visits, {
            single_sim <- sim_n_vary_template(
                diabetes_polypharmacy_setting = diabetes_polypharmacy_setting_complex_setting_more_visits,
                n_values = 2000,
                time_horizon = time_horizon,
                complex = TRUE
            ) # Redo single failed simulation
            tmp <- list()
            a <- list()
            a[[1]] <- single_sim
            tmp$a <- a
            collect_results_n_vary(c(sim_n_vary_complex_setting_more_visits,tmp))
        }
        
    ),
    tar_target(
        combine_n_results_complex_setting,
        collect_results_n_vary(sim_n_vary_complex_setting)
    ),
    tar_target(
        results_n,
        {
            dt1 <- combine_n_results_increase_censoring
            dt1[, scenario := "Simple setting (high degree of censoring)"]
            dt1[, true_value := true_values_increase_censoring[time_horizon == 12, risk]]
            dt2 <- combine_n_results_complex_setting
            dt2[, scenario := "Complex setting"]
            dt2[, true_value := true_values_complex_setting[time_horizon == 12,risk]]
            dt3 <- combine_n_results_complex_setting_more_visits
            dt3[, scenario := "Complex setting (more visits)"]
            dt3[, true_value := true_values_complex_setting_more_visits[time_horizon == 12,risk]]
            #dt3 <- dt3[estimate >= 0 & estimate <= 1] # 8 estimates were outside the [0,1] for n=200 with one-step and "lm"; four were outside for "oipcw_expit"; two did not run at all for n=200 
            dt <- rbindlist(list(dt1,dt2,dt3))[n>200]
            dt <- dt[model_pseudo_outcome != "ipcw_glm_expit"]
            dt <- dt[model_pseudo_outcome != "lm" | method != "ICE-IPCW (tmle)"]
            dt[model_pseudo_outcome == "lm", model_pseudo_outcome := "Linear regression"]
            dt[model_pseudo_outcome == "oipcw_expit", model_pseudo_outcome := "OIPCW (expit)"]

            p <- ggplot(dt, aes(x = factor(n), y = estimate, fill = model_pseudo_outcome)) +
                facet_grid(scenario~method,scales = "free_y") + 
                geom_boxplot(outliers = FALSE) +
                geom_hline(aes(yintercept = true_value), linetype = "dashed") +
                labs(x = "n (sample size)", y = "Estimated risk", fill = "Model for iterative regressions") +
                theme_bw() +
                
                ## facet_wrap(method~scenario,scales = "free_y") + 
                paletteer::scale_fill_paletteer_d("khroma::bright") +
                theme(legend.position = "bottom") +
                scale_y_continuous(labels = scales::percent)
            ## p_se <- ggplot(na.omit(dt[scenario == "Complex setting (more visits)" & model_pseudo_outcome == "OIPCW (expit)"]), aes(x = factor(n), y = se, color = method)) +
            ##     geom_boxplot(outliers = FALSE) +
            ##     labs(x = "n (sample size)", y = "Estimated standard error", color = "Estimator") +
            ##     theme_bw() +
            ##     paletteer::scale_fill_paletteer_d("khroma::bright") +
            ##     theme(legend.position = "bottom")
            tab <- dt[, .(coverage = mean(lower <= true_value & upper >= true_value),
                          bias = mean(estimate - true_value),
                          mse = mean((estimate - true_value)^2),
                          sd = sd(estimate),
                          se = mean(se)),
                      by = list(n, scenario, method, model_pseudo_outcome)]
            tab_se <- copy(tab)[method != "ICE-IPCW" & scenario == "Complex setting (more visits)", .(n, scenario, method, model_pseudo_outcome, sd, se)]
            tab_gt <- tab_se[, n := paste0("n = ", n)][,scenario := NULL] %>% gt(groupname_col = "n") %>%
                fmt_number(columns = "sd", n_sigfig = 3) %>%
                fmt_number(columns = "se", n_sigfig = 3) %>%
                cols_label(
                    sd = "SD",
                    se = "SE",
                    method = "Estimator",
                    model_pseudo_outcome = "Model for iterative regressions"
                )
            tab <- tab[method != "ICE-IPCW"]

            p_cov <- ggplot(tab, aes(x = n, y = coverage, color = model_pseudo_outcome)) +
                geom_line() +
                facet_grid(scenario~method) + 
                geom_hline(yintercept = 0.95, linetype = "dashed") +
                labs(x = "n (sample size)", y = "Coverage", color = "Model for iterative regressions") +
                theme_bw() +
                theme(legend.position = "bottom") +
                paletteer::scale_color_paletteer_d("khroma::bright") +
                scale_y_continuous(labels = scales::percent)
            tab[, ratio := sd / se]
            p_ratio <- ggplot(tab, aes(x = n, y = ratio, color = model_pseudo_outcome)) +
                geom_line() +
                facet_grid(scenario~method) + 
                geom_hline(yintercept = 1, linetype = "dashed") + 
                labs(x = "n (sample size)", y = expression(widehat(SD)/widehat(SE)), color = "Model for iterative regressions") +
                theme_bw() +
                theme(legend.position = "bottom") +
                paletteer::scale_color_paletteer_d("khroma::bright")
            list(
                plot = p,
                plot_coverage = p_cov,
                plot_ratio = p_ratio,
                table = tab,
                tab_gt = tab_gt
            )
        }
    ),
    tar_target(
        results_less_visits, {
            results_combined_less_visits <- combine_results(results_rtmle_less_visits, results_ice_ipcw_less_visits, NULL, results_ice_ipcw_no_tvc_less_visits, results_ice_ipcw_no_tmle_less_visits, return_ipw = FALSE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm"))
            results_combined_less_visits[, scenario := "Fewer visits (no irregularity)"]
            results_combined_less_visits[, true_value := true_values_less_visits[time_horizon == 12, risk]]
            results_combined_less_visits_sd_large <- combine_results(results_rtmle_less_visits_sd_large, results_ice_ipcw_less_visits_sd_large, NULL, results_ice_ipcw_no_tvc_less_visits_sd_large, results_ice_ipcw_no_tmle_less_visits_sd_large, return_ipw = FALSE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm"))
            results_combined_less_visits_sd_large[, scenario := "Fewer visits (irregularity)"]
            results_combined_less_visits_sd_large[, true_value := true_values_less_visits_sd_large[time_horizon == 12, risk]]
            results_combined <- rbind(results_combined_less_visits, results_combined_less_visits_sd_large)
            results_combined <- results_combined[method %in% c("LTMLE", "ICE-IPCW (tmle)", "ICE-IPCW (no TVC)")]
            results_combined <- results_combined[model_pseudo_outcome %in% c("GLMNET", "OIPCW (expit)")]
            p <- ggplot(results_combined, aes(y = estimate, x = method)) +
                geom_boxplot() +
                facet_wrap(~scenario) +
                geom_hline(aes(yintercept = true_value))+
                                labs(x = "Estimator", y = "Estimated risk") +
                scale_y_continuous(labels = scales::percent) +
                theme_bw() +
                theme(legend.position = "bottom") +
                paletteer::scale_fill_paletteer_d("khroma::bright")
            tab <- results_combined[, .(coverage = mean((lower <= true_value) & (upper >= true_value)),
                                    bias = mean(estimate - true_value),
                                    mse = mean((estimate - true_value)^2),
                                    sd = sd(estimate),
                                    mean_se = mean(se)),
                                    by = list(method, model_pseudo_outcome, scenario)]
            list(
                plot = p,
                table_full = tab
            )
        }
        ## Save and export table with gt:
        ## gtsave(tar_read(results_less_visits)$table, "less_visits_table.tex")
        ## ggsave(plot=tar_read(results_less_visits)$plot, "~/phd/continuous_time_LTMLE/less_visits_plot.pdf", width = 8, height = 8)
        ##  ## https://www.latex-tables.com -> Typst
    ),

        tar_target(
        results_effect_confounding, {
            results_combined_effect_outcome <- combine_results(results_rtmle_effect_outcome, results_ice_ipcw_effect_outcome, NULL, results_ice_ipcw_no_tvc_effect_outcome, results_ice_ipcw_no_tmle_effect_outcome, return_ipw = FALSE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm"))
            results_combined_effect_outcome[, scenario := "Effect on outcome (no confounding)"]
            results_combined_effect_outcome[, true_value := true_values_effect_outcome[time_horizon == 12, risk]]
            results_combined_confounding_no_effect_outcome <- combine_results(results_rtmle_confounding_no_effect_outcome, results_ice_ipcw_confounding_no_effect_outcome, NULL, results_ice_ipcw_no_tvc_confounding_no_effect_outcome, results_ice_ipcw_no_tmle_confounding_no_effect_outcome, return_ipw = FALSE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm"))
            results_combined_confounding_no_effect_outcome[, scenario := "Confounding without effect on outcome"]
            results_combined_confounding_no_effect_outcome[, true_value := true_values_confounding_no_effect_outcome[time_horizon == 12, risk]]
            results_combined_confounding_effect_outcome <- combine_results(results_rtmle_confounding_effect_outcome, results_ice_ipcw_confounding_effect_outcome, NULL, results_ice_ipcw_no_tvc_confounding_effect_outcome, results_ice_ipcw_no_tmle_confounding_effect_outcome, return_ipw = FALSE, return_ice = TRUE, model_pseudo_outcomes = c("oipcw_expit", "lm"))
            results_combined_confounding_effect_outcome[, scenario := "Confounding with effect on outcome"]
            results_combined_confounding_effect_outcome[, true_value := true_values_confounding_effect_outcome[time_horizon == 12, risk]]
            results_combined <- rbind(results_combined_effect_outcome, results_combined_confounding_no_effect_outcome, results_combined_confounding_effect_outcome)
            results_combined <- results_combined[method %in% c("LTMLE", "ICE-IPCW (tmle)", "ICE-IPCW (no TVC)")]
            results_combined <- results_combined[model_pseudo_outcome %in% c("GLMNET", "OIPCW (expit)")]
            p <- ggplot(results_combined, aes(y = estimate, x = method)) +
                geom_boxplot() +
                facet_wrap(~scenario, scales = "free_y") + #, ncol = 1) +
                geom_hline(aes(yintercept = true_value))+
                labs(x = "Estimator", y = "Estimated risk") + #, fill = "Model for iterative regressions") +
                scale_y_continuous(labels = scales::percent) +
                theme_bw() +
                ## legend on bottom
                theme(legend.position = "bottom") +
                paletteer::scale_fill_paletteer_d("khroma::bright")
            results_combined <- results_combined[method != "ICE-IPCW (no TVC)"]
            
            tab <- results_combined[, .(coverage = mean((lower <= true_value) & (upper >= true_value)),
                                    bias = mean(estimate - true_value),
                                    mse = mean((estimate - true_value)^2),
                                    sd = sd(estimate),
                                    mean_se = mean(se)),
                                    by = list(method, model_pseudo_outcome, scenario)]
            tab <- tab[method != "ICE-IPCW (no TVC)"]
            list(
                plot = p,
                table_full = tab
            )
            ## Save and export table with gt:
            ## gtsave(tar_read(results_effect_confounding)$table, "confounding_effect_table.tex")
            ## ggsave(plot=tar_read(results_effect_confounding)$plot, "~/phd/continuous_time_LTMLE/confounding_effect_plot.pdf", width = 8, height = 10)
            ## https://www.latex-tables.com -> Typst
        }
        ),
       tar_target(
           results_discretization, {
               p <- ggpubr::ggarrange(
                           results_effect_confounding$plot, results_less_visits$plot,
                           ncol = 1,
                           common.legend = TRUE
                           )
                           tab <- rbind(results_effect_confounding$table_full, results_less_visits$table_full)
            tab <- tab[method != "ICE-IPCW (no TVC)" & method != "ICE-IPCW" & method != "LTMLE"][, .(method, model_pseudo_outcome, scenario, coverage, sd, mean_se)]
            tab[, ratio := sd / mean_se]
            scenario_list <- list()
            scenario_list[[1]] <- c("Effect on outcome (no confounding)", "Confounding without effect on outcome", "Confounding with effect on outcome")
            scenario_list[[2]] <- c("Fewer visits (no irregularity)", "Fewer visits (irregularity)")
            p_cov_list <- list()
            for (i in seq_along(scenario_list)){
                p_cov_list[[i]] <- ggplot(tab[scenario %in% scenario_list[[i]]], aes(x = method, y = coverage, color = model_pseudo_outcome)) +
                    geom_point() +
                   facet_wrap(~scenario) + 
                labs(x = "Estimator", y = "Coverage", color = "Model for iterative regressions") +
                scale_y_continuous(labels = scales::percent) +
                theme_bw() +
                theme(legend.position = "bottom") +
                paletteer::scale_color_paletteer_d("khroma::bright") +
                    geom_hline(yintercept = 0.95, linetype = "dashed")
                if (i < 2){
                    p_cov_list[[i]] <- p_cov_list[[i]] + theme(axis.title.x
                                                               = element_blank())
                }   
            }
            p_cov <- ggpubr::ggarrange(plotlist = p_cov_list, ncol = 1, common.legend = TRUE, legend = "bottom")
            p_ratio_list <- list()
            for (i in seq_along(scenario_list)){
                p_ratio_list[[i]] <- ggplot(tab[scenario %in% scenario_list[[i]]], aes(x = method, y = ratio, color = model_pseudo_outcome)) +
                    geom_point() +
                    facet_wrap(~scenario) +
                    #labs(x = "Estimator", y = "SD/SE", color = "Model for iterative regressions") +
                    labs(x = "Estimator", y = expression(widehat(SD)/widehat(SE)), color = "Model for iterative regressions") +
                    theme_bw() +
                    theme(legend.position = "bottom") +
                    paletteer::scale_color_paletteer_d("khroma::bright") +
                    geom_hline(yintercept = 1, linetype = "dashed")
                if (i < 2){
                    p_ratio_list[[i]] <- p_ratio_list[[i]] + theme(axis.title.x
                                                               = element_blank())
                }
            }
            p_ratio <- ggpubr::ggarrange(plotlist = p_ratio_list, ncol = 1, common.legend = TRUE, legend = "bottom")
            list(
                plot = p,
                plot_coverage = p_cov,
                plot_ratio = p_ratio
            )   
        }
    ),


    ## Save and export table with gt:
    ## gtsave(tar_read(results_complex_setting)$table, "complex_setting_table.tex")
    ## ggsave(plot=tar_read(results_complex_setting)$plot, "~/phd/continuous_time_LTMLE/complex_setting_plot.pdf", width = 8, height = 8)
    tar_target(
        results_K,
        {
            combine_results <- combine_K_results[model_pseudo_outcome == "oipcw_expit"]
            combine_results[model_pseudo_outcome == "oipcw_expit", model_pseudo_outcome := "OIPCW (expit)"]
            combine_results[model_pseudo_outcome == "lm", model_pseudo_outcome := "Linear regression"]
            p <- ggplot(combine_results, aes(x = K, y = estimate)) +
                geom_boxplot(outliers = FALSE) +
                geom_hline(yintercept = true_values_complex_setting_more_visits[time_horizon == 12, risk], linetype = "dashed") +
                labs(x = "K", y = "Estimated risk") +
                theme_bw() +
                paletteer::scale_fill_paletteer_d("khroma::bright") +
                theme(legend.position = "bottom")+
                scale_y_continuous(labels = scales::percent)

            ## table
            tab <- copy(combine_results)[, .(coverage = mean((lower <= true_values_complex_setting_more_visits[time_horizon == 12, risk]) & (upper >= true_values_complex_setting_more_visits[time_horizon == 12, risk])),
                                             bias = mean(estimate - true_values_complex_setting_more_visits[time_horizon == 12, risk]),
                                             mse = mean((estimate - true_values_complex_setting_more_visits[time_horizon == 12, risk])^2),
                                             sd = sd(estimate),
                                             mean_se = mean(se)),
                                         by = list(K, model_pseudo_outcome)]
            ## Coverage plot
            p_cov <- ggplot(tab, aes(x = K, y = coverage)) +
                geom_point() +
                geom_hline(yintercept = 0.95, linetype = "dashed") +
                labs(x = "K", y = "Coverage") +
                theme_bw() +
                theme(legend.position = "bottom") +
                paletteer::scale_color_paletteer_d("khroma::bright") +
                scale_y_continuous(labels = scales::percent)
            tab[, ratio := sd / mean_se]
            p_ratio <- ggplot(tab, aes(x = K, y = ratio)) +
                geom_point() +
                geom_hline(yintercept = 1, linetype = "dashed") + 
                labs(x = "K", y = expression(widehat(SD)/widehat(SE)), color = "Model for iterative regressions") +
                theme_bw() +
                theme(legend.position = "bottom") +
                paletteer::scale_color_paletteer_d("khroma::bright")
            
            
            list(
                table_full = tab,
                plot = p,
                plot_ratio = p_ratio,
                plot_coverage = p_cov
            )
        }
        ## Save and export table with gt:
        ## gtsave(tar_read(results_K)$table, "K_vary_table.tex")
        ## ggsave(plot=tar_read(results_K)$plot, "~/phd/continuous_time_LTMLE/K_vary_plot.pdf", width = 8, height = 6)
    )
)

## ggsave(plot=tar_read(results_discretization)$plot, "~/phd/continuous_time_LTMLE/plots/results_discretization_plot.pdf", width = 12, height = 9)
## ggsave(plot=tar_read(results_discretization)$plot_coverage, "~/phd/continuous_time_LTMLE/plots/results_discretization_plot_coverage.pdf", width = 8, height = 8)
## ggsave(plot=tar_read(results_discretization)$plot_ratio, "~/phd/continuous_time_LTMLE/plots/results_discretization_plot_ratio.pdf", width = 8, height = 8)
## ggsave(plot=tar_read(results_K)$plot, "~/phd/continuous_time_LTMLE/plots/results_K_plot.pdf", width = 12, height = 9)
## ggsave(plot=tar_read(results_K)$plot_coverage, "~/phd/continuous_time_LTMLE/plots/results_K_plot_coverage.pdf", width = 8, height = 8)
## ggsave(plot=tar_read(results_K)$plot_ratio, "~/phd/continuous_time_LTMLE/plots/results_K_plot_ratio.pdf", width = 8, height = 8)
## ggsave(plot=tar_read(results_n)$plot, "~/phd/continuous_time_LTMLE/plots/results_n_plot.pdf", width = 12, height = 9)
## ggsave(plot=tar_read(results_n)$plot_coverage, "~/phd/continuous_time_LTMLE/plots/results_n_plot_coverage.pdf", width = 8, height = 8)
## ggsave(plot=tar_read(results_n)$plot_ratio, "~/phd/continuous_time_LTMLE/plots/results_n_plot_ratio.pdf", width = 8, height = 8)
## gtsave(tar_read(results_n)$tab_gt, "~/phd/continuous_time_LTMLE/results_n_special_case.tex")
