run_rtmle_analyis <- function(register_data){
    if (FALSE){
        setwd("~/metropolis/Teaching/doctoral-course-longitudinal-causal-inference/practicals/")
        library(rtmle)
        library(targets)
        library(data.table)
        tar_load(register_data)
    }
    intervals <- seq(0,36*30.45,4*30.45)
    x <- rtmle_init(intervals = length(intervals)-1,name_id = "id",name_outcome = "Death",name_competing = NULL,name_censoring = "Censored",censored_label = "censored")
    x <- add_long_data(x,outcome_data=register_data$death_data,censored_data=register_data$censored_data,competing_data=NULL,timevar_data=register_data$timevar_data)
    x <- add_baseline_data(x,data=register_data$baseline_data)
    x <- long_to_wide(x,intervals = intervals,start_followup_date = "start_followup_date")
    x <- protocol(x,name = "Always_bactrim",
                  intervention = data.frame("bactrim" = factor(rep("1",length(intervals)-1),levels = c("0","1"))))
    x <- protocol(x,name = "Always_AP",
                  intervention = data.frame("AP" = factor(rep("0",length(intervals)-1),levels = c("0","1"))))
    x <- prepare_data(x)
    x <- target(x,name = "Outcome_risk",
                estimator = "tmle",
                protocols = c("Always_bactrim","Always_AP"))
    x <- model_formula(x,exclude_variables = "AP")
    x2 <- copy(x)
    x0 <- run_rtmle(x,time_horizon = 1:9,learner = "learn_glm",learn_variables = "NONE")
    x <- run_rtmle(x,learner = "learn_glmnet",time_horizon = 1:9)
    x2 <- model_formula(x2,exclude_variables = "AP",Markov = c("karnofsky","PCP"))
    x2 <- run_rtmle(x2,learner = "learn_glmnet",time_horizon = 1:9)
}
