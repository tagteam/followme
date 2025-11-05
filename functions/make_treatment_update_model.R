# -----------------------------
# Treatment update model (hierarchical binaries)
# -----------------------------
make_treatment_update_model <- function(intercept_values) {
    m <- lvm()
    distribution(m, ~ trt_continue + trt_crossover + trt_stopmedical) <- binomial.lvm("logit")


    # Intercepts: label them so they match default_par (intercept_trtcont, intercept_trtcross, intercept_trtstopmed)
    intercept(m,~trt_continue) <- intercept_values[["intercept_continue"]]
    intercept(m,~trt_crossover) <- intercept_values[["intercept_crossover"]]
    intercept(m,~trt_stopmedical) <- intercept_values[["intercept_stopmedical"]]

    # Regression parameters
    regression(m) <- trt_continue ~ f(karnofsky,effect_karnofsky_continue) + f(gender,effect_gender_continue) + f(trt,effect_trt_continue)
    regression(m) <- trt_crossover ~ f(karnofsky,effect_karnofsky_crossover) + f(gender,effect_gender_crossover) + f(trt,effect_trt_crossover)
    regression(m) <- trt_stopmedical ~ f(karnofsky,effect_karnofsky_stopmedical) + f(gender,effect_gender_stopmedical) + f(trt,effect_trt_stopmedical)
    m
}
