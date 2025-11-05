get_null_setting <- function(){
    max_follow <- 36
    baseline_variables <- c("gender","trt","karnofsky")
    absorbing_events <- list("death" = "Weibull","dropout" = "Weibull")
    intermediate_events <- list("PCP" = "Weibull")
    visit_measurements <- list("changekarnofsky" = "normal")
    visit_events <- list("continue" = "binomial","crossover" = "binomial","stopmedical" = "binomial")
    visit_schedule <- list("mean" = 3,"sd" = 1, skip = 0)
    ## ipv = initialize_parameter_values(baseline_variables = baseline_variables,absorbing_events = names(absorbing_events),intermediate_events = names(intermediate_events),visit_measurements = names(visit_measurements),visit_events = names(visit_events))
    ## dput(ipv)
    parameter_values <- c(intercept_gender = 0, intercept_trt = 0, intercept_karnofsky = 0, 
                          intercept_changekarnofsky = 0, intercept_continue = 1, intercept_crossover = -1, 
                          intercept_stopmedical = -3, scale_death = 0.007, scale_dropout = 0.001, 
                          scale_PCP = 0.01, effect_gender_continue = 0, effect_trt_continue = 0, 
                          effect_karnofsky_continue = 0, effect_gender_crossover = 0, effect_trt_crossover = 0, 
                          effect_karnofsky_crossover = 0, effect_gender_stopmedical = 0, 
                          effect_trt_stopmedical = 0, effect_karnofsky_stopmedical = 0, 
                          effect_gender_changekarnofsky = 0, effect_trt_changekarnofsky = 0, 
                          effect_karnofsky_changekarnofsky = 0, effect_gender_death = 0, 
                          effect_trt_death = 0, effect_karnofsky_death = 0, effect_gender_dropout = 0, 
                          effect_trt_dropout = 0, effect_karnofsky_dropout = 0, effect_gender_PCP = 0, 
                          effect_trt_PCP = 0, effect_karnofsky_PCP = 0, effect_PCP_death = 0, 
                          effect_changekarnofsky_death = 0, effect_continue_death = 0, 
                          effect_crossover_death = 0, effect_stopmedical_death = 0, effect_PCP_dropout = 0, 
                          effect_changekarnofsky_dropout = 0, effect_continue_dropout = 0, 
                          effect_crossover_dropout = 0, effect_stopmedical_dropout = 0, 
                          effect_PCP_PCP = 0, effect_changekarnofsky_PCP = 0, effect_continue_PCP = 0, 
                          effect_crossover_PCP = 0, effect_stopmedical_PCP = 0)
    list(max_follow = max_follow,
         baseline_variables = baseline_variables,
         absorbing_events = absorbing_events,
         intermediate_events = intermediate_events,
         visit_measurements = visit_measurements,
         visit_events = visit_events,
         visit_schedule = visit_schedule,
         parameter_values = parameter_values)
}
