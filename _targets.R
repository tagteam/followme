### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: May  5 2026 (09:38) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 1054
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
tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach","ggplot2","plotly", "contICEIPCW", "purrr", "riskRegression", "prodlim", "dplyr", "tidyr"),
               controller = controller)

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
                             "complex_setting_more_visits" #
                         )
## NOTE: Dropout = right-censoring
scenarios <- tibble::tibble(
                         scenario = scenario_names,
                         modify_fn = list(
                             function(dps) {
                                 dps$parameter_values$scale_dropout <- 0.0002*15
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
                                 dps
                             },
                             function(dps) {
                                 dps$visit_schedule$mean <- 1.5
                                 dps$visit_schedule$sd <- 0.4
                                 dps
                             }
                         ),
                         complex = list(FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
                     )

modify_dps <- function(complex = FALSE, modify_fn) {
    dps <- get_diabetes_simulation_setting(complex = complex)
    modify_fn(dps)
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
                intervals = c(0, intervals),
                true_values = true_values),
            cue = cue_test
        ),

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

        ## --- Coverage, MSE, Bias ---
        tar_target(
            summary_simulation_results,
            get_coverage(results_rtmle, results_ice_ipcw, results_csc, results_ice_ipcw_no_tvc, results_ice_ipcw_no_tmle, true_values, time_horizon)
        )
    ),
    ## --- Vary K in ICE-IPCW ---
    tar_target(
        dps_K,
        modify_dps(complex = TRUE, modify_fn = (scenarios %>% filter(scenario == "complex_setting_more_visits") %>% pull(modify_fn)) %>% first())
    ),
    tar_rep(
        sim_K_vary,
        {
            complex <- TRUE
            diabetes_population <- do.call(
                "simulate_cohort",
                c(list(n = 2000), dps_K)
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
    ## --- Plot results for varying K ---
    tar_target(
        plot_K_vary, {
            all_dfs <- sim_K_vary |>
                map(~ map(.x, "ice_ipcw")) |>  # reach ice_ipcw
                flatten() |>                   # remove one level
                flatten()                      # remove second level (dfs)
            results_ice_ipcw_K <- rbindlist(all_dfs)
            results_ice_ipcw_test <- results_ice_ipcw_complex_setting_more_visits
            results_ice_ipcw_test[, K := "adaptive"]
            results_ice_ipcw_K <- rbind(results_ice_ipcw_K, results_ice_ipcw_test, fill = TRUE)
            
            ggplot(results_ice_ipcw_K, aes(x = factor(K), y = estimate, fill = model_pseudo_outcome)) +
                geom_boxplot(outliers = FALSE) +
                geom_hline(yintercept = true_values_complex_setting_more_visits[time_horizon == 12,
                                                                                risk], linetype = "dashed") +
                labs(x = "K (number of past visits included in pseudo-outcomes)", y = "Estimate", fill = "Model for pseudo-outcomes") +
                theme_minimal()
        }
    ),
    
    ## --- Vary sample size in simulations ---
    tar_target(
        dps_n,
        modify_dps(complex = TRUE, modify_fn = (scenarios %>% filter(scenario == "complex_setting") %>% pull(modify_fn)) %>% first())
    ),
    tar_rep(
        sim_n_vary,
        {
            complex <- TRUE

            results <- list()
            for (n in n_values){
                message(paste0("Running ICE-IPCW with n = ", n))
                diabetes_population <- do.call(
                    "simulate_cohort",
                    c(list(n = n), dps_n)
                )
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
                    lag_propensity = 1
                )$results
                out[, n := n]
                results[[as.character(n)]] <- out
            }
            list(ice_ipcw = results)
        },
        iteration = "list",
        reps = 100, 
        batches = 100,
        cue = cue_sim
    ),
    ## --- Plot results for varying n ---
    tar_target(
        plot_n_vary, {
            all_dfs <- sim_n_vary |>
                map(~ map(.x, "ice_ipcw")) |>  # reach ice_ipcw
                flatten() |>                   # remove one level
                flatten()                      # remove second level (dfs)
            results_ice_ipcw_n <- rbindlist(all_dfs)

            ggplot(results_ice_ipcw_n, aes(x = factor(n), y = estimate, fill = model_pseudo_outcome)) +
                geom_boxplot(outliers = FALSE) +
                geom_hline(yintercept = true_values_complex_setting[time_horizon == 12,
                                                                                risk], linetype = "dashed") +
                labs(x = "n (sample size)", y = "Estimate", fill = "Model for pseudo-outcomes") +
                theme_minimal()
        })
) 

######################################################################
### _targets.R ends here

