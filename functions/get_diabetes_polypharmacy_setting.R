get_diabetes_polypharmacy_setting <- function(){
    max_follow <- 60
    baseline_variables <- list("U" = "normal",
                               "sex" = "binomial",
                               "age" = "normal",
                               "HbA1c" = "normal")
    absorbing_events <- list("death" = "Weibull","dropout" = "Weibull")
    intermediate_events <- list("MACE" = "Weibull")
    visit_measurements <- list(
        "changeHbA1c" = "normal"
    )
    visit_events <- list("GLP1" = "binomial",
                         "SGLT2" = "binomial",
                         "DPP4" = "binomial",
                         "History_GLP1" = "constant",
                         "History_SGLT2" = "constant",
                         "History_DPP4" = "constant")
    visit_schedule <- list("mean" = 6,"sd" = 1,skip = 0)
    ipv = initialize_parameter_values(baseline_variables = names(baseline_variables),
                                      absorbing_events = names(absorbing_events),
                                      intermediate_events = names(intermediate_events),
                                      visit_measurements = names(visit_measurements),
                                      visit_events = names(visit_events))
    ## dput(ipv)
    list(max_follow = max_follow,
         baseline_variables = baseline_variables,
         absorbing_events = absorbing_events,
         intermediate_events = intermediate_events,
         visit_measurements = visit_measurements,
         visit_events = visit_events,
         visit_schedule = visit_schedule,
         parameter_values = ipv)
}
