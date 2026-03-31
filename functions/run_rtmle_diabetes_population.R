run_rtmle_diabetes_population <- function(diabetes_population,
                                          intervals = seq(0, 60, 6),
                                          time_horizons = 30,
                                          regimens = c("GLP1", "SGLT2", "DPP4"),
                                          tv_covs = c("changeHbA1c", regimens),
                                          exclusion_rules = list("SGLT2_0" = c("GLP1_0","DPP4_0"),
                                                       "GLP1_0" = c("SGLT2_0","DPP4_0"),
                                                       "DPP4_0" = c("SGLT2_0","GLP1_0")),
                                          baseline_covs = c("age", "sex"),
                                          name_outcome = "MACE",
                                          name_competing = "death",
                                          names_intermediate = "MACE",
                                          treatment_format = "date_value",
                                          method_covariate_discretization = "locf",
                                          method_treatment_discretization = "locf",
                                          ...){
    if (FALSE){
        tar_load_globals()
        tar_load(diabetes_population)
    }
    setkey(diabetes_population,id,time,event)
    intervals <- intervals[intervals <= max(time_horizons)]
    time_horizons <- match(time_horizons, intervals) - 1
    if (any(is.na(time_horizons))){
        stop("time_horizons must be a subset of intervals")
    }
    if (any(time_horizons == 0)){
        stop("time_horizons must not include 0")
    }
    x <- rtmle::rtmle_init(name_id = "id",
                           name_outcome = name_outcome,
                           name_competing = name_competing,
                           name_censoring = "Censored",
                           censored_label = "censored",
                           time_grid = intervals)
    diabetes_population[, last := as.integer(.I == .I[.N]), by = id]
    diabetes_population[, first := as.integer(.I == .I[1L]), by = id]
    ## diabetes_population[last == 1,table(event)]
    tv <- lapply(tv_covs, function(tv){
        if (tv %in% regimens){
            if (treatment_format == "date_value"){
                d <- diabetes_population[,c("id","time",tv),with = FALSE]
                setnames(d,c("time", tv), c("date","value"))
            } else if (treatment_format == "date"){
                d <- diabetes_population[,c("id","time",tv),with = FALSE]
                setnames(d,c("id","time",tv),c("id","date","value"))
                d <- d[value == 1]
                d[, value := NULL]
            } else if (treatment_format == "start_stop"){
                d <- diabetes_population[,c("id","time",tv),with = FALSE]
                setnames(d,c("id","start_date","value"))
                d[, end_date := shift(x = start_date,n = 1,type = "lead"),by = id]
                d <- d[!is.na(end_date)]
                d <- d[value != 0]
                d[,value := NULL]
            } else {
                stop("treatment_discretization_scheme must be one of date_value, date, or start_stop")
            }
        }else{
            d <- diabetes_population[event%in%c("baseline",names_intermediate,"visit"),c("id","time",tv),with = FALSE]
            setnames(d,c("id","date","value"))
        }
        d[]
    })
    names(tv) <- tv_covs
    if (!is.null(name_competing)){
       competing_data <- diabetes_population[event == name_competing,.(id,date = time)][!duplicated(id)]
    } else {
        competing_data <- NULL
    }
    x <- rtmle::add_long_data(x,
                              outcome_data=diabetes_population[event == name_outcome,.(id,date = time)][!duplicated(id)],
                              censored_data=diabetes_population[last == 1 & event %in% c("visit","dropout"),.(id,date = time)],
                              competing_data=competing_data,
                              timevar_data=tv)
    x <- rtmle::add_baseline_data(x,data=diabetes_population[first == 1, c("id", baseline_covs), with = FALSE])
    long_to_wide_args <- list(start_followup_date = 0)
    for (tv in tv_covs){
        if (tv %in% regimens){
            long_to_wide_args[[tv]] <- list(method = method_treatment_discretization)
        } else {
            long_to_wide_args[[tv]] <- list(method = method_covariate_discretization)
        }
    }
    x <- do.call(rtmle::long_to_wide, c(list(x), long_to_wide_args))
    for (treat in regimens){
        intervention <- data.table(time = x$intervention_nodes, treat = factor(rep("1",length(intervals)-1),levels = c("0","1")))
        setnames(intervention, "treat", treat)
        x <- rtmle::protocol(x,name = paste0("Always_", treat),
                             intervention = intervention)
    }
    x <- rtmle::target(x,name = "Outcome_risk",
                       estimator = "tmle",
                       protocols = paste0("Always_",regimens))
    x <- rtmle::prepare_rtmle_data(x)
    x <- rtmle::model_formula(x, exclusion_rules = exclusion_rules)
    x <- run_rtmle(x, time_horizon = time_horizons, ...)
    return(x)
}


