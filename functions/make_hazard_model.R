# -----------------------------
# Hazard rate model for events
# -----------------------------
make_hazard_model <- function(events,predictors,scale_values) {
    m <- lvm()
    # Use a Cox-Weibull parametrization for the hazard functions of the time-to-event variables.
    for (e in events){
        distribution(m,e) <- coxWeibull.lvm(scale = scale_values[[paste0("scale_",e)]])
        regression(m) <- formula(paste0(e," ~ ",paste(sapply(predictors,function(pred){paste0("f(",pred,",effect_",pred,"_",e,")")}),collapse = "+")))
    }
    m
}
