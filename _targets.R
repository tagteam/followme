### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: Mar 26 2026 (15:44) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 301
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
library(crew.cluster)
tar_source("functions")

if (dir.exists("/projects/biostat01/people/snf991/followme")) {
    controller <- crew_controller_slurm(
        workers = 64,
        seconds_idle = 15,
        options_cluster = crew_options_slurm(
            partition = "long",
            cpus_per_task = 2,
            log_output = "crew_log_%A.txt",
            log_error = "crew_log_%A.txt",
            verbose = TRUE
        ) # Start on markov
    )
} else {
    controller <- crew_controller_local(workers = 16,
                                        options_local = crew_options_local(log_directory = "log"))
}

tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach","ggplot2","plotly", "contICEIPCW"),
               controller = controller)
               ## debug = "sim_confounding_effect_outcome_915ab5486bc867f1")
## # Install contICEIPCW from GitHub if not already installed:
## devtools::install_github("jsohlendorff/contICEIPCW")
## # Install RTMLE from GitHub if not already installed:
## devtools::install_github("tagteam/RTMLE")

scenarios <- tibble::tibble(
  scenario = c(
    "effect_outcome",
    "confounding_no_effect_outcome",
    "less_irregular_visits",
    "confounding_effect_outcome"
  ),
  modify_fn = list(
    function(dps) {
      dps$parameter_values <- modifyList(dps$parameter_values,
        list(effect_GLP1_MACE = 1,
             effect_SGLT2_MACE = -2,
             scale_MACE = 0.002,
             scale_death = 0.001))
      dps
    },
    function(dps) {
      dps$parameter_values <- modifyList(dps$parameter_values,
        list(scale_MACE = 0.002,
             scale_death = 0.001))
      dps$parameter_values$effect_changeHbA1c_SGLT2 <- 0.5
      dps$parameter_values$effect_changeHbA1c_GLP1 <- -0.5
      dps$parameter_values$effect_SGLT2_changeHbA1c <- 0.7
      dps$parameter_values$effect_GLP1_changeHbA1c <- -0.7
      dps
    },
    function(dps) {
      dps$parameter_values <- modifyList(dps$parameter_values,
        list(effect_GLP1_MACE = 1,
             effect_SGLT2_MACE = -2,
             scale_MACE = 0.002,
             scale_death = 0.001))
      dps$visit_schedule <- modifyList(dps$visit_schedule, list(sd = 0.001))
      dps
    },
    function(dps) {
      dps$parameter_values <- modifyList(dps$parameter_values,
        list(effect_GLP1_MACE = 1,
             effect_SGLT2_MACE = -2,
             scale_MACE = 0.002,
             scale_death = 0.001))
      dps$parameter_values$effect_changeHbA1c_SGLT2 <- 0.5
      dps$parameter_values$effect_changeHbA1c_GLP1 <- -0.5
      dps$parameter_values$effect_SGLT2_changeHbA1c <- 0.7
      dps$parameter_values$effect_GLP1_changeHbA1c <- -0.7
      dps
    }
  )
)

list(
  tar_map(
    values = scenarios,
    names = scenario,

    ## -- Get the diabetes polypharmacy setting  ---
    tar_target(
      diabetes_polypharmacy_setting,
      {
        dps <- get_diabetes_polypharmacy_setting()
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
      cue = tar_cue(mode = "thorough")
    ),

    ## --- Calculate true values for the interventional risks ---
    tar_target(
      true_values,
      calculate_interventional_risks(
        n = 1000000,
        diabetes_polypharmacy_setting = diabetes_polypharmacy_setting,
        intervention = list("GLP1" = 1, "SGLT2" = 1, "DPP4" = 1),
        time_horizons = seq(0, 60, 6)[-1],
        primary_event = "MACE"
      ),
      cue = tar_cue(mode = "never")
    ),

    ## --- RTMLE estimator on a single simulated dataset ---
    tar_target(
      rtmle,
      run_rtmle_diabetes_population(
        diabetes_population = diabetes_population,
        time_horizons = seq(6, 60, 6),
        intervals = seq(0, 60, 6),
        learner = "learn_glmnet"
      )
    ),

    ## --- ICE-IPCW estimator on a single simulated dataset ---
    tar_target(
      ice_ipcw,
      run_ice_ipcw(
        data = diabetes_population,
        time_horizons = seq(6, 60, 6),
        regimens = c("GLP1", "SGLT2", "DPP4"),
        model_pseudo_outcome = "oipcw_expit",
        penalize_pseudo_outcome = FALSE,
        primary_event = "MACE",
        contrasts = TRUE,
        contrasts_reference = "SGLT2",
        verbose = FALSE
      )
    ),

    ## --- Plotting estimates against true values ---
    tar_target(
      plot,
      plot_estimate(
        estimates_rtmle = rtmle,
        estimates_ice_ipcw = ice_ipcw,
        intervals = seq(0, 60, 6),
        true_values = true_values
      )
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
          time_horizons = 48,
          intervals = seq(0, 60, 6),
          learner = "learn_glmnet"
        )
        rtmle <- summary(rtmle)

        ice_ipcw <- run_ice_ipcw(
          data = diabetes_population,
          time_horizons = 48,
          regimens = c("GLP1", "SGLT2", "DPP4"),
          model_pseudo_outcome = "oipcw_expit",
          penalize_pseudo_outcome = FALSE,
          primary_event = "MACE",
          contrasts = TRUE,
          contrasts_reference = "SGLT2",
          verbose = FALSE
        )

        list(
          rtmle = rtmle,
          ice_ipcw = ice_ipcw
        )
      },
      iteration = "list",
      reps = 20, 
      batches = 100
    )
  ),
  ## Write the simulated data to disk for RTMLE package
  tar_target(diabetes_sim_data, {
      ## If data directory doesn't exist, create it
      if (!dir.exists("data")) {
          dir.create("data")
      }
      make_and_write_diabetes_data(file_name = "data/diabetes_sim_data.csv", diabetes_polypharmacy_setting = diabetes_polypharmacy_setting_effect_outcome)
  }, format = "file")
)

######################################################################
### _targets.R ends her
