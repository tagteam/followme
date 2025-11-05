#
# lava model
# 
make_regression_model <- function(outcome_variables,parameter_values) {
    m <- lvm()
    for (v in names(outcome_variables)){
        if (outcome_variables[[v]] == "constant"){
            distribution(m, v) <- constant.lvm()
        }
        if (outcome_variables[[v]] == "binomial"){
            distribution(m, v) <- binomial.lvm("logit")
            intercept(m,v) <- parameter_values[[paste0("intercept_",v)]]
        }
        if (outcome_variables[[v]] == "normal"){
            distribution(m, v) <- normal.lvm()
            intercept(m,v) <- parameter_values[[paste0("intercept_",v)]]
        }
        if (outcome_variables[[v]] == "Weibull"){
            distribution(m, v) <- coxWeibull.lvm(scale = parameter_values[[paste0("scale_",v)]])
        }
        # Regression parameters (if any)
        v_effects <- parameter_values[grep(paste0("_",v,"$"),names(parameter_values),value = TRUE)]
        v_effects <- v_effects[-grep("^intercept_|^scale_",names(v_effects),value = FALSE)]
        v_effects <- setNames(v_effects,sub("^effect_","",names(v_effects)))
        v_effects <- setNames(v_effects,sub(paste0("_",v,"$"),"",names(v_effects)))
        v_effects <- v_effects[v_effects != 0]
        if (length(v_effects)>0){
            regression(m) <- formula(paste0(v," ~ ",paste0(sapply(names(v_effects),function(e){paste0("f(",e,",",v_effects[[e]],")")}),collapse = "+")))
        }
    }
    m
}
