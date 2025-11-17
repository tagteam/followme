library(targets)
library(here)
library(lava)
library(data.table)
library(prodlim)
# tar_source(here("functions"))
tar_load_globals()
x <- get_alcohol_setting()
test <- do.call("simulate_person",x)
test <- do.call("simulate_cohort",c(list(n = 100),x))



## Old school
alco_simulator <- function(n, frail_out = 1, frail_survey = 0, alco_out = 1, alco_survey = 0, shape_out = 2, scale_out = 1, shape_survey = 1.5, scale_survey = 1){

    m <- lvm()
    distribution(m,~frail) <- binomial.lvm(p=0.5)
    categorical(m,labels=c("None","Moderate","High")) <- "alco"
    distribution(m,~outcome_time) <- coxWeibull.lvm(scale=shape_out,shape=scale_out)
    distribution(m,~survey_time) <- coxWeibull.lvm(scale=shape_survey,shape=scale_survey)
    
    regression(m,outcome_time~frail+alco) <- c(frail_out, alco_out)
    regression(m,survey_time~frail+alco) <- c(frail_survey, alco_survey)
    
    out <- sim(m, n = n)        
    setDT(out)

    out[, sampled := ifelse(survey_time < outcome_time, 1, 0)]
    
    return(out[])
    
}


dd <- alco_simulator(10000, frail_out = 2)

dd[, prop.table(table(alco, frail), 1)]
dd[sampled == 1, prop.table(table(alco, frail), 1)]

plot(prodlim(Hist(outcome_time)~alco, data = dd), xlim = c(0,0.1))
plot(prodlim(Hist(outcome_time-survey_time)~alco, data = dd[sampled == 1]), xlim = c(0,0.1))

## How to get a U/J-shape? 


## ## How to reproduce about with followme?
## ll <- list(max_follow = 60,
##            baseline_variables = list("U" = "normal"),
##            absorbing_events = list("death" = "Weibull"))

## initialize_parameter_values(baseline_variables = names(ll$baseline_variables),
##                             absorbing_events = names(ll$absorbing_events),
##                             intermediate_events = "dummy",
##                             visit_measurements = "dummy",
##                             visit_events = "dummy"
##                             )

##?



