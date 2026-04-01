### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: Apr  1 2026 (16:24) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 560
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
    ##         cpus_per_task = 2,
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
            clustermq.defaults = list(log_file="slurm.log"))
    controller <- NULL
} else {
    controller <- crew_controller_local(workers = 16,
                                        options_local = crew_options_local(log_directory = "log"))
}
tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach","ggplot2","plotly", "contICEIPCW", "purrr"),
               controller = controller)

## install rest of packages if not already installed:
## install.packages(c("targets", "tarchetypes", "crew", "crew.cluster", "lava", "survival", "data.table", "prodlim", "foreach", "ggplot2", "plotly", "glmnet", "targets", "tarchetypes", "crew", "crew.cluster", "devtools"))
# Install contICEIPCW from GitHub if not already installed:
## devtools::install_github("jsohlendorff/contICEIPCW")
# Install RTMLE from GitHub if not already installed:
## devtools::install_github("tagteam/RTMLE")

effect_SGLT2_mace <- -0.6
effect_changeHbA1c_SGLT2 <- 0.5
effect_SGLT2_changeHbA1c <- 0.4

scenarios <- tibble::tibble(
  scenario = c(
    "effect_outcome",
    "confounding_no_effect_outcome",
    "less_visits",
    "less_visits_sd_1",
    "confounding_effect_outcome",
    "complex_setting",
    "complex_setting_more_visits"
  ),
  modify_fn = list(
      function(dps) {
      dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
      dps
    },
    function(dps) {
      dps$parameter_values$effect_changeHbA1c_SGLT2 <- effect_changeHbA1c_SGLT2
      dps$parameter_values$effect_SGLT2_changeHbA1c <- effect_SGLT2_changeHbA1c
      dps
    },
    function(dps) {
      dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
      dps$visit_schedule$mean <- 6
      dps$visit_schedule$sd <- 0
      dps
    },
    function(dps) {
      dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
      dps$visit_schedule$mean <- 6
      dps
    },
    function(dps) {
      dps$parameter_values$effect_changeHbA1c_SGLT2 <- effect_changeHbA1c_SGLT2
      dps$parameter_values$effect_SGLT2_changeHbA1c <- effect_SGLT2_changeHbA1c
      dps$parameter_values$effect_SGLT2_mace <- effect_SGLT2_mace
      dps
    },
    function(dps) {
        dps
    },
    function(dps) {
      dps$visit_schedule$mean <- 1.5
      dps$visit_schedule$sd <- 0.2
      dps
    }
  ),
  complex = list(FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE)
)

time_horizon <- 12
intervals <- c(6, time_horizon)

list(
  tar_map(
    values = scenarios,
    names = scenario,

    ## -- Get the diabetes polypharmacy setting  ---
    tar_target(
      diabetes_polypharmacy_setting,
      {
        dps <- get_diabetes_simulation_setting(complex = complex)
        modify_fn(dps)
      }
    ),

    ## --- Simulate a large cohort for running methods ---
    tar_target(
      diabetes_population,
      do.call(
        "simulate_cohort",
        c(list(n = 30000), diabetes_polypharmacy_setting)
      ),
      cue = tar_cue(mode = "never")
    ),

    ## --- Calculate true values for the interventional risks ---
    tar_target(
      true_values,
      calculate_interventional_risks(
        n = 1000000,
        diabetes_polypharmacy_setting = diabetes_polypharmacy_setting,
        intervention = list("SGLT2" = 1),
        time_horizons = intervals,
        terminal_events = c("death", "mace", "dropout"),
        primary_event = "mace"
      ),
      cue = tar_cue(mode = "never")
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
          tv_covs = if (complex) c("HbA1c", "SGLT2") else c("changeHbA1c", "SGLT2"),
          exclusion_rules = NULL,
          baseline_covs = if (complex) "sex" else "HbA1c",
          name_outcome = "mace",
          name_competing = "death",
          names_intermediate = NULL,
          treatment_format = "date_value",
          method_covariate_discretization = "locf",
          method_treatment_discretization = "locf"
      ),
      cue = tar_cue(mode = "never")
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
        time_confounders = if (complex) c("HbA1c", "SGLT2_percentage") else "changeHbA1c",
        exclude_variables = if (complex) "SGLT2_percentage" else NULL,
        baseline_confounders = if (complex) "sex" else "HbA1c",
        verbose = TRUE,
        tmle_update = TRUE,
        lag_propensity = 1
      ),
      cue = tar_cue(mode = "never")
    ),

    ## --- Plotting estimates against true values ---
    tar_target(
     plot,
     plot_estimate(
       estimates_rtmle = rtmle,
       estimates_ice_ipcw = ice_ipcw,
       intervals = c(0, intervals),
       true_values = true_values)
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
                tv_covs = if (complex) c("HbA1c", "SGLT2") else c("changeHbA1c", "SGLT2"),
                exclusion_rules = NULL,
                baseline_covs = if (complex) "sex" else "HbA1c",
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
                time_confounders = if (complex) c("HbA1c", "SGLT2_percentage") else "changeHbA1c",
                exclude_variables = if (complex) "SGLT2_percentage" else NULL,
                baseline_confounders = if (complex) "sex" else "HbA1c",
                verbose = FALSE,
                tmle_update = TRUE
            )

            list(
                rtmle = rtmle,
                ice_ipcw = ice_ipcw
            )
        },
        iteration = "list",
        reps = 100, 
        batches = 100,
        cue = tar_cue(mode = "always")
    )

    ## ## --- Summarize simulation results ---
    ## tar_target(
    ##     results_rtmle,
    ##     purrr::map_dfr(sim, ~ map_dfr(.x, "rtmle"))
    ## ),

    ## tar_target(
    ##     results_ice_ipcw,
    ##     purrr::map_dfr(sim, ~ map_dfr(.x, c("ice_ipcw", "results")))
    ## )

    ## --- Plot simulation results ---
    ## tar_target(
    ##    plot_simulation_results, plot_sims(results_rtmle, results_ice_ipcw, true_values, time_horizon, y_upper = 0.2)
    ## ),

    ## ## --- Coverage, MSE, Bias ---
    ## tar_target(
    ##     summary_simulation_results,
    ##     get_coverage(results_rtmle, results_ice_ipcw, true_values, time_horizon)
    ## )
  )
)

######################################################################
### _targets.R ends her
