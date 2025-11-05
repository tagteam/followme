### simulate_person.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:15) 
## Version: 
## Last-Updated: nov  5 2025 (16:02) 
##           By: Thomas Alexander Gerds
##     Update #: 52
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
simulate_person <- function(max_follow,
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
    ##
    ## Helper function which removes variables
    ## that we want to draw from X (lava::sim just passes these as they are)
    ##
    simX <- function(x,X,variables = NULL,only_variables = length(variables)>0,...){
        if (sum(x$M)>0 && NROW(X)>0){
            data.table::setDF(X)
            if (any(variables%in%names(X))){
                X = X[,setdiff(names(X),variables),drop = FALSE]
            }
            d <- data.table::setDT(lava::sim(x = x,X = X,...))
        }else{
            d <- data.table::setDT(lava::sim(x = x,...))
        }
        if (only_variables[[1]] == TRUE){
            d[,variables,with = FALSE]
        }else{
            d
        }
    }
    ##
    ## Baseline
    ##
    if (is.function(baseline_hook)){
        baseline_model <- do.call(baseline_hook,list(baseline_model = baseline_model))
    }
    baseline_model <- make_regression_model(baseline_variables,parameter_values)
    X_baseline <- simX(x = baseline_model,X = NULL,n = 1)
    
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
    current_time <- 0
    while (current_time < max_follow) {
        # draw time of next scheduled visit, allowing for skipped visits
        skipped_visits <- rbinom(1,3,visit_schedule[["skip"]])
        next_visit <- skipped_visits*visit_schedule[["mean"]]+max(rnorm(1, mean = visit_schedule[["mean"]], sd = visit_schedule[["sd"]]), 0.1) 
        # apply hook for absorbing events
        if (is.function(absorbing_events_hook)){
            absorbing_events_model <- do.call(absorbing_events_hook,
                                              list(absorbing_event_model = absorbing_events_model,
                                                   event_history = event_history[NROW(event_history)]))
        }
        # apply hook for intermediate events
        if (is.function(intermediate_events_hook)){
            intermediate_events_model <- do.call(intermediate_events_hook,
                                                 list(intermediate_events_model = intermediate_events_model,
                                                      event_history = event_history[NROW(event_history)]))
        }
        # draw next event time as the minimum of
        # a) the next visit time
        # b) the intermediate event times
        # c) the absorbing event times
        latent_times <- c(simX(intermediate_events_model,
                               variables = names(intermediate_events),
                               n = 1,
                               X = event_history[NROW(event_history)],
                               p = parameter_values),
                          simX(absorbing_events_model,
                               variables = names(absorbing_events),
                               n = 1,
                               X = event_history[NROW(event_history)],
                               p = parameter_values),
                          c(visit = next_visit))
        # note that latent_times is a named vector 
        current_event <- latent_times[which.min(unlist(latent_times))]
        # update current time
        current_time <- current_time+current_event[[1]]
        ## case visit. We assume that any intermediate event triggers a visit where the
        ## treatment status is reconsidered
        if(names(current_event) %in% c("visit",names(intermediate_events))){
            ## FIXME: could rename previous measurements of same variable as prev_variable
            #
            # draw visit measurements conditional on current history
            # excluding previous measurements of the same variable from the X 
            #
            update_event_history <- event_history[NROW(event_history)]
            set(update_event_history,j = "time",value = current_time)
            # case intermediate event
            if (names(current_event) %in% names(intermediate_events)){
                # count number of intermediate_events
                set(update_event_history,j = names(current_event),value = 1+event_history[.N,names(current_event),with = FALSE])
                set(update_event_history,j = "event",value = names(current_event))
            }else{
                set(update_event_history,j = "event",value = paste0(unique(c(names(current_event),"visit")),collapse = "/"))
            }
            # draw measurements conditional on history
            update_measurements <- simX(visit_measurements_model,
                                        n = 1,
                                        p = parameter_values,
                                        variables = names(visit_measurements),
                                        X = update_event_history)
            for (new in names(update_measurements)){
                data.table::set(update_event_history,j = new,value = update_measurements[[new]])
            }
            # draw visit treatment actions conditional on history
            update_treatment <- simX(visit_event_model,
                                     n = 1,
                                     p = parameter_values,
                                     variables = names(visit_events),
                                     X = current_event_history)
            # force a hierarchy of the binary treatment process indicators
            if (is.function(post_visit_hook)){
                update_event_history <- post_visit_hook(update_event_history = update_event_history,
                                                        update_treatment = update_treatment,
                                                        update_measurements = update_measurements,
                                                        event_history = event_history)
            }
            # adding the updated treatment status
            for (x in names(update_treatment)){
                if (length(histx <- strsplit(x,"^History_")[[1]])>1){
                    # update treatment history
                    data.table::set(update_event_history,j = x,value = event_history[.N][[x]]+update_treatment[[histx[[2]]]])
                }else{
                    data.table::set(update_event_history,j = x,value = update_treatment[[x]])
                }
            }
            event_history <- rbind(event_history,update_event_history,fill = TRUE)
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

######################################################################
### simulate_person.R ends here
