# ----------------------------------------
# simulate a cohort of patients
# ----------------------------------------
simulate_cohort <- function(n,
                            seed = NULL,
                            max_follow,
                            baseline_variables,
                            baseline_hook = NULL,
                            visit_schedule,
                            visit_events,
                            visit_measurements,
                            post_visit_hook = NULL,
                            intermediate_events,
                            intermediate_events_hook = NULL,                            
                            absorbing_events,
                            absorbing_events_hook = NULL,
                            parameter_values,
                            regime = NULL) {
    if (!is.null(seed)) set.seed(seed)
    out <- foreach::foreach(i = 1:n,.combine = "rbind")%do%{
        cbind(id = i,
              simulate_person(
                  max_follow = max_follow,
                  baseline_variables = baseline_variables,
                  baseline_hook = baseline_hook,
                  visit_schedule = visit_schedule,
                  visit_events = visit_events,
                  visit_measurements = visit_measurements,
                  post_visit_hook = post_visit_hook,
                  intermediate_events = intermediate_events,
                  intermediate_events_hook = intermediate_events_hook,
                  absorbing_events = absorbing_events,
                  absorbing_events_hook = absorbing_events_hook,
                  parameter_values = parameter_values,
                  regime = regime))
    }
    out[]
}
