library(targets)
library(here)
library(lava)
library(data.table)
library(prodlim)
tar_load_globals()

## Testing new simulator on alcohol setup
x <- get_alcohol_setting()
test <- do.call("simulate_cohort",c(list(n = 100),x))
test2 <- do.call("simulate_cohort2",c(list(n = 100),x))


## Testing new simulator on diabetes polypharmacy setting
s <- tar_read(diabetes_polypharmacy_setting)
s$parameter_values$intercept_age <- 50
s$parameter_values$intercept_HbA1c <- 60
s$parameter_values$effect_U_age = 12.9
s$parameter_values$effect_U_HbA1c = 8.3
s$parameter_values$effect_U_changeHbA1c = -0.5
s$parameter_values$effect_changeHbA1c_MACE = 0.03
s$parameter_values$effect_U_death = 0.1

uu <- do.call("simulate_cohort",c(list(n = 100),s))
uu2 <- do.call("simulate_cohort2",c(list(n = 100),s))

set.seed(111)
do.call("simulate_person",s)
set.seed(111)
do.call("simulate_cohort2",c(list(n = 1),s))

set.seed(111)
do.call("simulate_person",x)
set.seed(111)
do.call("simulate_cohort2",c(list(n = 1),x))

## Manual setup, target
sim_alco <- function(n,
                     alco_heart = -1,
                     alco_liver = 1,
                     heart_death = 1.2,
                     liver_death = 1.2,
                     frail_heart = 1,
                     frail_liver = 1,
                     frail_death = 1){

    U = 1*(runif(n)<.5)

    A = sample(c("none","medium","high"), size = n, replace = TRUE, prob = rep(1/3,3))
    heart_protect = 1*(A != "none")
    liver_harm = 1*(A == "high")

    H = rbinom(n,1, plogis(-1 + alco_heart*heart_protect + frail_heart*U))
    L = rbinom(n,1, plogis(-1 + alco_liver*liver_harm + frail_liver*U))

    T = rexp(n, rate = exp(-1 + heart_death*H + liver_death*L + frail_death*U))
    
    data.table(frail = U,
               alcohol = A,
               ## heart_protect = heart_protect,
               ## liver_harm = liver_harm,
               heart = H,
               liver = L,
               time = T,
               event = 1)

}

sim_dd <- sim_alco(n = 1000000)
## pl <- prodlim(Hist(time, event)~alcohol, data = sim_dd)
## plot(pl, xlim = c(0, 1))
pred_hor <- 0.5
sim_dd[, 100*prop.table(table(alcohol, Surv = 1*(time >= pred_hor)), 1)]
visit_time <- 2
sim_dd[time>visit_time][, 100*prop.table(table(alcohol, Surv = 1*(time >= visit_time+pred_hor)), 1)]
## Sort of done now. The effect vanishes in this setup.

## Try to just do it with simple setup with one frailty model and a binary treatment.
## What would it take for it to switch over?

dd_cond <- sim_dd[time>visit_time][, rest_time := time-visit_time]
pl_cond <- prodlim(Hist(rest_time, event)~alcohol, data = dd_cond)
plot(pl_cond, xlim = c(0, 1))


