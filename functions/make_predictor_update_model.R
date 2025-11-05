# -----------------------------
# Predictor update model
# -----------------------------
make_predictor_update_model <- function() {
    m <- lvm()
    # three binary outcomes updated at each visit (logit link)
    distribution(m, ~ karnofsky_score + asthenia + hemoglobin) <- binomial.lvm("logit")


    # Intercepts: name the intercept parameters so they match 'default_par'
    intercept(m) <- karnofsky_score ~ f(a_karn)
    intercept(m) <- asthenia ~ f(a_asth)
    intercept(m) <- hemoglobin ~ f(a_hemo)


    # Regressions: depend on previous values, baseline and current treatment indicators
    regression(m, pars = c("b_karn_kprev","b_karn_aprev","b_karn_hprev","b_karn_g","b_karn_it","b_karn_tc","b_karn_cr","b_karn_tm")) <-
        karnofsky_score ~ karnofsky_score_prev + asthenia_prev + hemoglobin_prev + gender + init_trt + trt_continue + trt_crossover + trt_stop_medical


    regression(m, pars = c("b_asth_aprev","b_asth_kprev","b_asth_hprev","b_asth_g","b_asth_it","b_asth_tc","b_asth_cr","b_asth_tm")) <-
        asthenia ~ asthenia_prev + karnofsky_score_prev + hemoglobin_prev + gender + init_trt + trt_continue + trt_crossover + trt_stop_medical


    regression(m, pars = c("b_hemo_hprev","b_hemo_kprev","b_hemo_aprev","b_hemo_g","b_hemo_it","b_hemo_tc","b_hemo_cr","b_hemo_tm")) <-
        hemoglobin ~ hemoglobin_prev + karnofsky_score_prev + asthenia_prev + gender + init_trt + trt_continue + trt_crossover + trt_stop_medical


    m
}





