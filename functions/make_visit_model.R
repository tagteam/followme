# -----------------------------
# Treatment update model (hierarchical binaries)
# -----------------------------
make_visit_model <- function(parameter_values,
                             visit_measurements,
                             visit_events) {
    m <- lvm()
    for (v in c(visit_measurements,visit_events)){
        distribution(m, v) <- binomial.lvm("logit")
        intercept(m,v) <- parameter_values[[paste0("intercept_",v)]]
        # Regression parameters
        v_effects <- parameter_values[grep(paste0("_",v,"$"),names(parameter_values),value = TRUE)]
        v_effects <- v_effects[-grep("^intercept_",names(v_effects),value = FALSE)]
        v_effects <- setNames(v_effects,sub("^effect_","",names(v_effects)))
        v_effects <- setNames(v_effects,sub(paste0("_",v,"$"),"",names(v_effects)))
        regression(m) <- formula(paste0(v," ~ ",paste0(sapply(names(v_effects),function(e){paste0("f(",e,",",v_effects[[e]],")")}),collapse = "+")))
    }
    m
}
