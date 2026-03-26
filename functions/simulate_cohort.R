simulate_cohort <- function(n,
                            seed = NULL, 
                            max_follow, 
                            baseline_variables, 
                            post_baseline_variables_hook = NULL,
                            baseline_visit = NULL,
                            post_baseline_visit_hook = NULL,
                            visit_schedule, 
                            visit_events, 
                            visit_measurements, 
                            post_visit_hook = NULL, 
                            intermediate_events, 
                            intermediate_events_hook = NULL, 
                            absorbing_events, 
                            absorbing_events_hook = NULL, 
                            parameter_values, 
                            intervention = NULL, 
                            regime = NULL){
    if (!is.null(seed)) set.seed(seed) 
    ##
    ## Helper function which initially removes all the variables from X
    ## that we actually want to draw from the distribution. lava::sim 
    ## passes given X variables.
    ##
    simX <- function(x,X,variables = NULL,...){
        on.exit({
            if(!is.null(X)) data.table::setDT(X)
        })
        if (length(x$M)>0){
            if (sum(x$M)>0 && NROW(X)>0){
                if (any(variables%in%names(X))){ ## Why if-statement needed?
                    X = X[, setdiff(names(X),variables),drop = FALSE, with = FALSE]
                }
                d <- data.table::setDT(lava::sim(x = x,X = X,...))
            }else{
                d <- data.table::setDT(lava::sim(x = x,...))
            }
            if (length(variables)>0){
                d = d[,variables,with = FALSE]
            }
        }else{
            d <- data.table()
        }
        return(d)
    }
    if (length(intervention)>0){
        ## Remove dropout to generate the interventional distribution 
        absorbing_events$dropout <- NULL
        for (i in seq_len(length(intervention))){
            visit_event <- names(intervention)[i]
            visit_events[[visit_event]] <- "constant"
            parameter_values[[paste0("intercept_",visit_event)]] <- intervention[[i]]
        }
    }
    ##
    ## Baseline variables
    ##
    baseline_model <- make_regression_model(baseline_variables,parameter_values)
    X_baseline <- simX(x = baseline_model,X = NULL,n = n)
    X_baseline[, id := 1:.N]
    
    # post-baseline hook
    if (is.function(post_baseline_variables_hook)){
        X_baseline <- do.call(post_baseline_variables_hook,list(X = X_baseline))
    }    
    ## initialize time dependent variables
    for (v in c(names(intermediate_events),names(visit_measurements))){
        data.table::set(X_baseline,j = v,value = 0)
    }
    ## baseline visit (e.g., randomization)
    if (length(baseline_visit)>0){
        baseline_visit_model <- make_regression_model(baseline_visit,parameter_values)
        X_baseline_visit <- simX(x = baseline_visit_model,X = X_baseline,n = n)
    }else{
        X_baseline_visit <- NULL
    }
    if (length(X_baseline_visit) && is.function(post_baseline_visit_hook)){
        X_baseline_visit <- do.call(post_baseline_visit_hook,list(X = X_baseline_visit))
    }
    #
    # regression models used to simulate intermediate and absorbing events
    # 
    
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


    # initialize event_history at time 0 only for variables that do not occur in X_baseline_visit
    still_uninitialized <- setdiff(names(visit_events), names(X_baseline_visit))
    if (length(still_uninitialized)>0){
        init_visit_events <- data.table::as.data.table(setNames(rep(list(0), length(still_uninitialized)), 
                                                                still_uninitialized))
    }else{
        init_visit_events <- NULL
    }
    
    event_history <- cbind(data.table(time = 0,event = "baseline"),
                           X_baseline,
                           X_baseline_visit,
                           init_visit_events)

    ## index event_history by id (helps later joins/appends)
    data.table::setindex(event_history, id)

    ##
    ## Loop until an absorbing event
    ##

    ## keep a rolling state of at-risk subjects (latest row per id)
    last_entry <- data.table::copy(event_history)  # at baseline, 1 row per id, time=0
    data.table::setkey(last_entry, id)
    
    while (nrow(last_entry)>0) {

        ## draw time of next scheduled visit, allowing for skipped visits
        nrisk <- nrow(last_entry)  
        skipped_visits <- rbinom(n = nrisk, 3, visit_schedule[["skip"]])
        next_visit <- skipped_visits*visit_schedule[["mean"]] +
            pmax(rnorm(n = nrisk, mean = visit_schedule[["mean"]], sd = visit_schedule[["sd"]]), 0.1)

        ## apply hook for absorbing events
        if (is.function(absorbing_events_hook)){
            absorbing_events_model <- do.call(absorbing_events_hook,
                                              list(absorbing_event_model = absorbing_events_model,
                                                   event_history = last_entry))
        }

        ## apply hook for intermediate events
        if (is.function(intermediate_events_hook)){
            intermediate_events_model <- do.call(intermediate_events_hook,
                                                 list(intermediate_events_model = intermediate_events_model,
                                                      event_history = last_entry))
        }

        ## draw next event time as min of visit/intermediate/absorbing
        intermediate_time <- simX(intermediate_events_model,
                                  variables = names(intermediate_events),
                                  n = nrisk,
                                  X = last_entry,
                                  p = parameter_values)
        absorbing_time <- simX(absorbing_events_model,
                               variables = names(absorbing_events),
                               n = nrisk,
                               X = last_entry,
                               p = parameter_values)

        # ensure matrices
        intermediate_time <- as.matrix(intermediate_time)
        absorbing_time <- as.matrix(absorbing_time)

        latent_times <- matrix(NA_real_, nrow = nrisk, ncol = ncol(intermediate_time) + ncol(absorbing_time) + 1)
        latent_times[, seq_len(ncol(intermediate_time))] <- intermediate_time
        latent_times[, ncol(intermediate_time) + seq_len(ncol(absorbing_time))] <- absorbing_time
        latent_times[, ncol(intermediate_time) + ncol(absorbing_time) + 1] <- next_visit
        colnames(latent_times) <- c(colnames(intermediate_time), colnames(absorbing_time), "visit")
        
        idx <- max.col(-latent_times, ties.method = "first")
        current_event <- data.table(
            id = last_entry[, id],
            event = colnames(latent_times)[idx],
            time = last_entry[, time] + latent_times[cbind(seq_len(nrow(latent_times)), idx)]
        )

        ## Divide subjects at risk into two categories
        subjects_with_intermediate_events <- current_event[event %chin% c("visit",names(intermediate_events)), .(id, time, event)]
        subjects_absorbed <- current_event[event %chin% names(absorbing_events), .(id, time, event)]

        ## Cases of intermediate events
        if(nrow(subjects_with_intermediate_events)>0){

            ## Start from last_entry and overwrite time/event
            update_visit <- last_entry[,-c("time","event"),with = FALSE][subjects_with_intermediate_events, on = "id", nomatch = 0L]
            if (nrow(update_visit)) {
                data.table::setkey(update_visit, id)
            }

            ## Increase count of intermediate events
            for(vv in names(intermediate_events)){
                vv_update <- which(update_visit$event == vv)
                set(update_visit, j = vv, i = vv_update, value = update_visit[vv_update][[vv]] + 1L)
            }

            ## draw visit measurements conditional on updated history
            update_measurements <- simX(visit_measurements_model,
                                        n = nrow(update_visit),
                                        p = parameter_values,
                                        variables = names(visit_measurements),
                                        X = update_visit)
            for (new in names(update_measurements)){
                data.table::set(update_visit, j = new, value = update_measurements[[new]])
            }

            ## draw visit treatment actions conditional on history
            update_treatment <- simX(visit_event_model,
                                     n = nrow(update_visit),
                                     p = parameter_values,
                                     variables = names(visit_events),
                                     X = update_visit)

            ## post-visit hook
            if (is.function(post_visit_hook)){
                update_visit <- post_visit_hook(update_event_history = update_visit,
                                                update_treatment = update_treatment,
                                                update_measurements = update_measurements,
                                                event_history = event_history)
            }

            ## adding the updated treatment status
            for (xx in names(update_treatment)){
                data.table::set(update_visit, j = xx, value = update_treatment[[xx]])
            }
        } else {
            update_visit <- data.table(time = numeric(), id = numeric())
        }

        ## Prepare absorbed updates (overwrite time/event to absorbing ones)
        update_absorbed <- last_entry[,-c("time","event"),with = FALSE][subjects_absorbed, on = "id", nomatch = 0L]
        
        ## Update full event_history (as before)
        event_history <- data.table::rbindlist(
                                         list(event_history, update_visit, update_absorbed), use.names = TRUE, fill = TRUE
                                     )

        ## Update the rolling risk set (last_entry) 
        if (nrow(update_visit)) {
            last_entry <- update_visit
            ## Enforce max follow-up for next iteration's risk set
            last_entry <- last_entry[time <= max_follow]
        } else{
            break
        }
    }
    ## Remove events after max_follow
    event_history[time>max_follow, c("time","event") := list(max_follow,"dropout")]
    setcolorder(event_history,"id")
    return(event_history[])
}


