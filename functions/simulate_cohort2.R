simulate_cohort2 <- function(n,
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
                             regime = NULL){
    if (!is.null(seed)) set.seed(seed)
    ##
    ## Helper function which removes variables
    ## that we want to draw from X (lava::sim just passes these as they are)
    ##
    simX <- function(x,X,variables = NULL,only_variables = length(variables)>0,keep_id = TRUE,...){
        on.exit({
            if(!is.null(X)) data.table::setDT(X)
        })
        if (length(x$M)>0){
            if (sum(x$M)>0 && NROW(X)>0){
                data.table::setDF(X)
                if (any(variables%in%names(X))){ ## Why if-statement needed?
                    X = X[,setdiff(names(X),variables),drop = FALSE]
                }
                d <- data.table::setDT(lava::sim(x = x,X = X,...))
                if("id"%in%names(X) && keep_id)
                    d$id <- X$id
            }else{
                d <- data.table::setDT(lava::sim(x = x,...)) 
            }
            if (only_variables[[1]] == TRUE){ ## Why only_variables as a separate argument?
                d = d[,variables,with = FALSE]
            }
        }else{
            d <- data.table()
        }
        return(d)
    }
    ##
    ## Baseline
    ##
    if (is.function(baseline_hook)){
        baseline_model <- do.call(baseline_hook,list(baseline_model = baseline_model))
    }
    baseline_model <- make_regression_model(baseline_variables,parameter_values)
    X_baseline <- simX(x = baseline_model,X = NULL,n = n)
    X_baseline[, id := 1:.N]
       
    ## initialize time dependent variables
    for (v in c(names(intermediate_events),names(visit_measurements))){
        data.table::set(X_baseline,j = v,value = 0)
    }
    # scheduled measurements
    visit_measurements_model <- make_regression_model(outcome_variables = visit_measurements,
                                                      parameter_values = parameter_values)
    # treatment related events
    visit_event_model <- make_regression_model(outcome_variables = visit_events,
                                               parameter_values = parameter_values)
    
    # event hazard rate model
    intermediate_events_model <- make_regression_model(outcome_variables = intermediate_events,
                                                       parameter_values = parameter_values)
    absorbing_events_model <- make_regression_model(outcome_variables = absorbing_events,
                                                    parameter_values = parameter_values)
    # initialize event_history at time 0 
    init_visit_events <- data.table(as.data.frame(as.list(sapply(names(visit_events),function(v)0))))
    event_history <- cbind(data.table(time = 0,event = "baseline"),
                           X_baseline,
                           init_visit_events)
    ##
    ## Loop until an absorbing event
    ##
    at_risk_event_history = event_history
    while (nrow(at_risk_event_history)>0) {

        ## n_at_risk <- at_risk_event_history[, length(unique(id))]
        setorder(at_risk_event_history, id, time)
        last_entry <- at_risk_event_history[,.SD[.N],id]

        ## draw time of next scheduled visit, allowing for skipped visits
        skipped_visits <- rbinom(n = nrow(last_entry),3,visit_schedule[["skip"]])
        next_visit <- skipped_visits*visit_schedule[["mean"]]+pmax(rnorm(n = nrow(last_entry), mean = visit_schedule[["mean"]], sd = visit_schedule[["sd"]]), 0.1)
        
        ## apply hook for absorbing events
        if (is.function(absorbing_events_hook)){
            ## FIXME: Check if this is working (why is event_history needed for this hook?)
            absorbing_events_model <- do.call(absorbing_events_hook,                                              
                                              list(absorbing_event_model = absorbing_events_model,
                                                   event_history = last_entry))
        }
        ## apply hook for intermediate events
        if (is.function(intermediate_events_hook)){
            ## FIXME: Check if this is working (why is event_history needed for this hook?)
            intermediate_events_model <- do.call(intermediate_events_hook,
                                                 list(intermediate_events_model = intermediate_events_model,
                                                      event_history = last_entry))
        }
        
        ## draw next event time as the minimum of
        ## a) the next visit time
        ## b) the intermediate event times
        ## c) the absorbing event times
        latent_times <- as.matrix(cbind(
            simX(intermediate_events_model,
                 variables = names(intermediate_events),
                 n = nrow(last_entry),
                 X = last_entry,
                 p = parameter_values),
            simX(absorbing_events_model,
                 variables = names(absorbing_events),
                 n = nrow(last_entry),
                 X = last_entry,
                 p = parameter_values),
            data.table(visit = next_visit)))
        current_event <- data.table(id = last_entry[, id],
                                    event = colnames(latent_times)[apply(latent_times, 1, which.min)],
                                    time = last_entry[, time] + apply(latent_times, 1, min))

        ## Divide subjects at risk into two categories
        visit_inter_ids <- current_event[event %chin% c("visit",names(intermediate_events)), .(id, time, event)]
        absorbed_ids <- current_event[event %chin% names(absorbing_events), .(id, time, event)]

        ## Cases of non abserved events
        if(nrow(visit_inter_ids)>0){
            ## Store last entry for visit ids
            last_entry_visit <- last_entry[visit_inter_ids[,.(id)], on = "id"]
            ## Update time and event
            update_visit <- last_entry[, !c("time","event")][visit_inter_ids, on = "id"]

            ## Increase count of intermediate events
            for(vv in names(intermediate_events)){
                update_visit[event == vv, (vv) := get(vv) + 1L]
            }            

            update_measurements <- simX(visit_measurements_model,
                                        n = nrow(update_visit),
                                        p = parameter_values,
                                        variables = names(visit_measurements),
                                        X = update_visit)
            for (new in names(update_measurements)){
                data.table::set(update_visit,j = new,value = update_measurements[[new]])
            }
            # draw visit treatment actions conditional on history
            update_treatment <- simX(visit_event_model,
                                     n = nrow(update_visit),
                                     p = parameter_values,
                                     variables = names(visit_events),
                                     X = update_visit)
            # force a hierarchy of the binary treatment process indicators
            if (is.function(post_visit_hook)){
                ## FIXME: Check if this is working (why is event_history needed for this hook?)
                update_visit <- post_visit_hook(update_event_history = update_visit,
                                                update_treatment = update_treatment,
                                                update_measurements = update_measurements,
                                                event_history = event_history)
            }
            ## adding the updated treatment status
            for (xx in names(update_treatment)){
                if (length(histxx <- strsplit(xx,"^History_")[[1]])>1){
                    ## FIXME: AM says: I do not understand this code properly
                    # update treatment history
                    data.table::set(update_visit,j = xx,
                                    ## FIXME: Check whether this should be last_entry_visit or update_visit
                                    value = last_entry_visit[[xx]]+update_treatment[[histxx[[2]]]])
                }else{
                    data.table::set(update_visit,j = xx,value = update_treatment[[xx]])
                }
            }            
        }else{
            update_visit = data.table(time = numeric(),
                                      id = numeric())
        }        

        update_absorbed = last_entry[, !c("time","event")][absorbed_ids, on = "id"]

        ## Update full event_history
        event_history <- rbind(event_history,
                               update_visit,
                               update_absorbed,
                               fill = TRUE)

        ## Update risk set and at_risk_event_history
        at_risk_ids <- update_visit[time<max_follow, .(id)] ## NB: Assuming only alternative to being in update_visit is being absorbed
        at_risk_event_history <- event_history[at_risk_ids, on = "id"]
    }
    return(event_history[])
}
