### get_diabetes_simulation_setting.R
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Mar 30 2026 (19:37) 
## Version: 
## Last-Updated: Apr  1 2026 (16:16) 
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
get_diabetes_simulation_setting <- function(complex = FALSE){
    if (complex){
        ## Percentage of time treated 3 months 
        percentage_treat_hook <- function(update_event_history, update_treatment, update_measurements, event_history){

            test <- merge(
                update_event_history[, .(id, time_ref = time)],
                event_history[, c("id", "time", names(update_treatment)), with = FALSE],
                by = "id",
                allow.cartesian = TRUE
            )

            # Relative time capped at 3
            test[, time := pmin(time_ref - time, 3)]

            # Order by id and time
            setorder(test, id, time)

            test[, keep := {
                remove <- time >= 3
                prev_remove <- c(FALSE, remove[-.N])
                !remove | (remove & !prev_remove)
            }, by = id]

            test <- test[keep == TRUE]

            test[, SGLT2_weight := {
                dt <- c(time[1L], diff(time))
                SGLT2 * dt
            }, by = id]

            # Aggregate
            test <- test[, .(SGLT2_percentage = sum(SGLT2_weight) / 3), by = id]

            # Remove if exists (safe)
            update_event_history[, SGLT2_percentage := NULL]

            # Merge result back
            merge(update_event_history, test, by = "id")
        }


        ## Lag variables for treatment and measurements:
        ## GLP1 -> GLP1
        ## SGLT2 -> SGLT2
        lag_variable_hook <- function(update_event_history, update_treatment, update_measurements, event_history){
            update_event_history[, paste0(c(colnames(update_treatment), colnames(update_measurements)), "lag") := lapply(.SD, function(x) x), .SDcols = c(colnames(update_treatment), colnames(update_measurements))]
            return(update_event_history)
        }
        list(
            max_follow = 15,
            baseline_variables = list(HbA1c = "normal", sex = "binomial"),
            baseline_visit = list(start_SGLT2 = "constant"),
            post_baseline_visit_hook = function(X){
                X[,SGLT2:=1*(start_SGLT2 == 1)]
                X[, start_SGLT2 := NULL]
                X[, SGLT2lag := SGLT2]
                X[, SGLT2_percentage := 0]
                X[]
            },
            absorbing_events = list(death = "Weibull", dropout = "Weibull", mace = "Weibull"),
            intermediate_events = list() |>
                structure(names = character(0)),
            visit_measurements = list(changeHbA1c = "normal"),
            visit_events = list(SGLT2 = "binomial"),
            visit_schedule = list(mean = 3, sd = 0.8, skip = 0),
            parameter_values = list(
                ## sex
                intercept_sex = 0,
                
                ## HbA1c parameters at baseline:
                intercept_HbA1c = 50,
                var_HbA1c = 1.5,

                ## SGLT2 at baseline (unused)
                intercept_start_SGLT2 = 1,
                effect_HbA1c_start_SGLT2 = 0,

                ## Change in HbA1c parameters:
                intercept_changeHbA1c = 0,
                effect_HbA1c_changeHbA1c = 0,
                effect_changeHbA1clag_changeHbA1c = 0.1,
                effect_SGLT2_changeHbA1c = 0.1,

                ## Death 
                scale_death = 0.000175*5,
                effect_HbA1c_death = 0,
                effect_changeHbA1c_death = 0,
                effect_SGLT2_death = 0,

                ## Censoring
                scale_dropout = 0.00015*5,
                effect_HbA1c_dropout = 0,
                effect_SGLT2_dropout = 0,

                ## MACE
                scale_mace = 0.000315*6,
                effect_HbA1c_mace = 0.025,
                effect_SGLT2_mace = 0,
                effect_SGLT2_percentage_mace = -1,
                effect_changeHbA1c_mace = 0.025,

                ## Effects on SGLT2 treatment:
                intercept_SGLT2 = -47*0.8,
                effect_SGLT2lag_SGLT2 = 0.2,
                effect_changeHbA1c_SGLT2 = 0.8,
                effect_HbA1c_SGLT2 = 0.8
            ),
            post_visit_hook = function(update_event_history, update_treatment, update_measurements, event_history) {
                update_event_history <- lag_variable_hook(update_event_history, update_treatment, update_measurements, event_history)
                ## update_event_history <- hook_hba1c(update_event_history, update_treatment, update_measurements, event_history)
                update_event_history <- percentage_treat_hook(update_event_history, update_treatment, update_measurements, event_history)
                return(update_event_history)
            },
            post_baseline_variables_hook = function(X){
                X[, changeHbA1clag := 0]
                X[]
            },
            cleanup_hook = function(X){
                X[, HbA1c := changeHbA1c + HbA1c]
                X[, c("SGLT2lag", "changeHbA1clag", "changeHbA1c") := NULL]
                X[]
            }
        )
    } else {
        list(
            max_follow = 15,
            baseline_variables = list(HbA1c = "normal"),
            baseline_visit = list(start_SGLT2 = "constant"),
            post_baseline_visit_hook = function(X){
                X[, SGLT2 := 1*(start_SGLT2 == 1)]
                X[, start_SGLT2 := NULL]
                X[]
            },
            absorbing_events = list(death = "Weibull", dropout = "Weibull", mace = "Weibull"),
            intermediate_events = list() |>
                structure(names = character(0)),
            visit_measurements = list(changeHbA1c = "normal"),
            visit_events = list(SGLT2 = "binomial"),
            visit_schedule = list(mean = 3, sd = 0.8, skip = 0),
            parameter_values = list(
                intercept_HbA1c = 50,
                var_HbA1c = 3,
                intercept_start_SGLT2 = 1,
                intercept_changeHbA1c = 0,
                intercept_SGLT2 = 0.6,
                scale_death = 0.0003*5,
                scale_dropout = 0.0002*5.5,
                scale_mace = 0.00045*8,
                effect_HbA1c_start_SGLT2 = 0,
                effect_HbA1c_SGLT2 = 0,
                effect_HbA1c_changeHbA1c = 0,
                effect_HbA1c_death = 0,
                effect_HbA1c_dropout = 0,
                effect_HbA1c_mace = 0,
                effect_changeHbA1c_death = 0,
                effect_SGLT2_death = 0,
                effect_SGLT2_dropout = 0,
                effect_SGLT2_mace = 0
            )
        )
    }
}

######################################################################
### get_diabetes_simple_setting.R ends here
