initialize_parameter_values <- function(baseline_variables,
                                        visit_schedule,
                                        visit_events,
                                        visit_measurements,
                                        intermediate_events,
                                        absorbing_events,
                                        intercept_value = 0,
                                        scale_value = 1/100,
                                        effect_value = 0){
    events <- c(absorbing_events,intermediate_events)
    intercepts <- sapply(c(baseline_variables,
                           visit_measurements,
                           visit_events),function(v){intercept_value})
    names(intercepts) <- paste0("intercept_",names(intercepts))
    scales <- sapply(events,function(v){scale_value})
    names(scales) <- paste0("scale_",names(scales))
    construct_effects <- function(vector1,vector2,effect_value){
        combinations <- data.table::setDT(expand.grid(vector1, vector2,stringsAsFactors = FALSE))
        # remove self-effects
        combinations <- combinations[Var1 != Var2]
        setNames(rep(effect_value, nrow(combinations)), paste0("effect_",apply(combinations, 1, paste, collapse = "_")))
    }
    effects_baseline_baseline <- construct_effects(baseline_variables,baseline_variables,effect_value = effect_value)

    effects_baseline_events <- construct_effects(baseline_variables,
                                                 events,
                                                 effect_value = effect_value)
    if (length(visit_events)>0){
        effects_baseline_visit <- construct_effects(baseline_variables,
                                                    c(visit_events,visit_measurements),
                                                    effect_value = effect_value)
    }else{
        effects_baseline_visit <- NULL
    }
    if (length(intermediate_events)>0){
        effects_timevar <- construct_effects(c(intermediate_events,visit_measurements,visit_events),
                                             events,
                                             effect_value = effect_value)
    }else{
        effects_timevar <- NULL
    }
    as.list(c(intercepts,
              scales,
              effects_baseline_baseline,
              effects_baseline_visit,
              effects_baseline_events,
              effects_timevar))
}
