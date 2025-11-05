simulate_patient <- function(max_follow,
                             baseline_variables,
                             visit_schedule,
                             visit_events,
                             visit_measurements,
                             intermediate_events,
                             intermediate_events_hook = NULL,
                             absorbing_events,
                             absorbing_events_hook = NULL,
                             parameter_values,
                             regime = NULL){
    ##
    ##  Baseline
    ## 
    X_baseline <- data.table(karnofsky = sample(seq(40,100,1),size = 1),
                             gender = rbinom(1,1,.5),
                             trt = rbinom(1,1,.5))
    ## initialize time dependent variables
    for (v in c(names(intermediate_events),names(visit_measurements))){
        set(X_baseline,j = v,value = 0)
    }
    # scheduled measurements
    visit_measurements_model <- make_regression_model(outcome_variables = visit_measurements,
                                                      parameter_values = parameter_values)
    # treatment events
    visit_event_model <- make_regression_model(outcome_variables = visit_events,
                                               parameter_values = parameter_values)
    
    # event hazard rate model
    intermediate_events_model <- make_regression_model(outcome_variables = intermediate_events,
                                                       parameter_values = parameter_values)
    absorbing_events_model <- make_regression_model(outcome_variables = absorbing_events,
                                                    parameter_values = parameter_values)
    # initialize event_history at time 0 where everyone continues treatment
    event_history <- cbind(data.table(time = 0,event = "baseline"),
                           X_baseline,
                           data.table(continue = 1,crossover = 0,stopmedical = 0))
    ##
    ## Loop until an absorbing event
    ##
    current_time <- 0
    while (current_time < max_follow) {
        # draw time of next scheduled visit, allowing for skipped visits
        skipped_visits <- rbinom(1,3,visit_schedule[["skip"]])
        next_visit <- skipped_visits*visit_schedule[["mean"]]+max(rnorm(1, mean = visit_schedule[["mean"]], sd = visit_schedule[["sd"]]), 0.1) 
        # draw next event time, make sure that the intermediate events are not conditioned on
        if (is.function(absorbing_events_hook)){
            absorbing_events_model <- do.call(absorbing_events_hook,
                                              list(absorbing_events_model = absorbing_events_model,
                                                   event_history = event_history[NROW(event_history)]))
        }
        if (is.function(intermediate_events_hook)){
            intermediate_events_model <- do.call(intermediate_events_hook,
                                                 list(intermediate_events_model = intermediate_events_model,
                                                      event_history = event_history[NROW(event_history)]))
        }
        latent_times <- c(sim(intermediate_events_model,
                              n = 1,
                              X = event_history[NROW(event_history),setdiff(names(event_history),names(intermediate_events))],
                              p = parameter_values)[,names(intermediate_events),drop = FALSE],
                          sim(absorbing_events_model,
                              n = 1,
                              X = event_history[NROW(event_history)],
                              p = parameter_values)[,names(absorbing_events),drop = FALSE],
                          c(visit = next_visit))
        current_event <- latent_times[which.min(unlist(latent_times))]
        current_time <- current_time+current_event[[1]]
        if (names(current_event) %in% names(intermediate_events)){
            i_event <- data.table(V = 1)
            setnames(i_event,names(current_event))
            event_history <- rbind(event_history,cbind(i_event,event_history[NROW(event_history),setdiff(names(event_history),names(i_event)),with = FALSE]),fill = TRUE)
            event_history[NROW(event_history),event := names(current_event)]
            event_history[NROW(event_history),time := current_time]
        }
        ## We assume that the treatment status is reconsidered as in a visit
        ## also just after intermediate events
        if(names(current_event) %in% c("visit",names(intermediate_events))){
            ## FIXME: could rename previous measurements of same variable as prev_variable
            #
            # draw visit measurements conditional on current history
            # excluding previous measurements of the same variable from the X 
            #
            current_event_history <- event_history[NROW(event_history),
                                                   setdiff(names(event_history),names(visit_measurements)),with = FALSE]
            new_event_history <- copy(current_event_history)
            new_measurements <- setDT(sim(visit_measurements_model,
                                          n = 1,
                                          p = parameter_values,
                                          X = current_event_history)[names(visit_measurements)])
            for (new in names(new_measurements)){
                set(new_event_history,j = new,value = new_measurements[[new]])
            }
            # draw visit treatment actions conditional on current history
            # remove crossover and stopmedical unless they have happened.
            # if crossover has happened then we assume the patient stays on crossover drug
            vnames <- setdiff(names(current_event_history),names(visit_events))
            if (current_event_history[["crossover"]] == 1) vnames = c(vnames,"crossover")
            current_event_history <- current_event_history[,vnames,with = FALSE]
            # draw next value of treatment process
            X_treatment <- setDT(sim(visit_event_model,n = 1,p = parameter_values,X = current_event_history))
            # force hierarchy of the binary treatment process indicators
            # FIXME: the following lines need to become a functional argument (post_visit_hook)
            if ("crossover" %in% names(current_event_history) && current_event_history[["crossover"]] == 1) {
                X_treatment[crossover == 1, continue := 0]
                X_treatment[crossover == 1, stopmedical := 0]
            }
            X_treatment[continue == 1, crossover := 0]
            X_treatment[continue == 1, stopmedical:= 0]
            if (names(current_event) %in% names(intermediate_events)){
                # have already a new line, now just adding the post visit information
                for (x in names(X_treatment)){
                    set(event_history,i = NROW(event_history),j = x,value = X_treatment[[x]])
                }
                for (new in names(new_measurements)){
                    set(event_history,i = NROW(event_history),j = new,value = new_measurements[[new]])
                }
            }else{
                event_history <- rbind(event_history,
                                       cbind(X_treatment,new_event_history[,setdiff(names(event_history),names(X_treatment)),with = FALSE]),
                                       fill = TRUE)
            }
            event_history[NROW(event_history),event := paste0(unique(c(names(current_event),"visit")),collapse = "/")]
            event_history[NROW(event_history),time := current_time]
        }else{
            if (names(current_event) %in% c(names(absorbing_events))){
                event_history <- rbind(event_history,event_history[NROW(event_history)])
                event_history[NROW(event_history),event := names(current_event)]
                event_history[NROW(event_history),time := current_time]
                break
            }
            else{
                print(names(current_event))
                stop("Don't know what happened")
            }
        }
    }
    return(event_history[])
}
