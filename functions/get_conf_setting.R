get_conf_setting <- function(null_setting){
    x = copy(null_setting)
    x$parameter_values[["intercept_gender"]] <- 0.3
    x$parameter_values[["scale_PCP"]] <- 0.001
    x$parameter_values[["effect_gender_trt"]] <- -0.5 
    x$parameter_values[["effect_trt_death"]] <- -0.5
    x$parameter_values[["effect_trt_crossover"]] <- -0.7
    x$parameter_values[["effect_trt_PCP"]] <- -1.5
    x$parameter_values[["effect_gender_crossover"]] <- 0.7
    x$parameter_values[["effect_changekarnofsky_crossover"]] <- -0.7
    x$parameter_values[["effect_karnofsky_death"]] <- -0.03
    x$parameter_values[["effect_karnofsky_PCP"]] <- -0.05
    x$parameter_values[["effect_changekarnofsky_death"]] <- -0.7
    x$parameter_values[["effect_changekarnofsky_PCP"]] <- -0.7
    x$parameter_values[["effect_crossover_death"]] <- 0.5
    x$parameter_values[["effect_crossover_PCP"]] <- 0
    x$parameter_values[["effect_continue_PCP"]] <- -1
    x$parameter_values[["effect_continue_death"]] <- -0.5
    x$parameter_values[["effect_stopmedical_PCP"]] <- 0.1
    x$parameter_values[["effect_PCP_death"]] <- 1.7
    x$parameter_values[["effect_gender_death"]] <- -0.7
    x$absorbing_events_hook <- function(absorbing_events_model,event_history){
        if (event_history[["continue"]] == 0){
            regression(absorbing_events_model,death~trt) <- 0
        }
        absorbing_events_model
    }
    x$intermediate_events_hook <- function(intermediate_events_model,event_history){
        if (event_history[["continue"]] == 0){
            regression(intermediate_events_model,PCP~trt) <- 0
        }
        intermediate_events_model
    }
    x
}
