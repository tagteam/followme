get_alcohol_setting <- function(){
    max_follow <- 60
    baseline_variables <- list("frailty" = "normal",
                               "alcohol" = "lognormal",
                               "heart" = "binomial",
                               "liver" = "binomial")
    intermediate_events = NULL
    visit_measurements = NULL
    visit_events = NULL
    absorbing_events <- list("death" = "Exponential","dropout" = "Exponential")
    visit_schedule <- list("mean" = 20,"sd" = 1,skip = 0)
    ## ipv = initialize_parameter_values(baseline_variables = names(baseline_variables),absorbing_events = names(absorbing_events),intermediate_events = names(intermediate_events),visit_measurements = names(visit_measurements),visit_events = names(visit_events))
    ## dput(ipv)
    ipv = list(intercept_frailty = 0,
               intercept_alcohol = 2,
               intercept_heart = 0,
               intercept_liver = -1,
               scale_death = 0.005, 
               scale_dropout = 0.0000000001,
               effect_alcohol_frailty = 0,
               effect_frailty_alcohol = 0,
               effect_alcohol_heart = -0.2,
               effect_alcohol_liver = 0.05,
               effect_frailty_heart = 0,
               effect_frailty_liver = 0, 
               ## effect_frailty_death = 1,
               ## effect_alcohol_death = 1,
               effect_heart_death = 1.5,
               effect_liver_death = 1,
               effect_frailty_dropout = 0, 
               effect_alcohol_dropout = 0)
    list(max_follow = max_follow,
         baseline_variables = baseline_variables,
         absorbing_events = absorbing_events,
         intermediate_events = intermediate_events,
         visit_measurements = visit_measurements,
         visit_events = visit_events,
         visit_schedule = visit_schedule,
         parameter_values = ipv)
}
