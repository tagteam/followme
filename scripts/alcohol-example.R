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





## ## Alcohol project:
## SEM:
## alcohol --(+)--> heart
## alcohol --(-)--> liver
## frailty --(-)--> heart
## frailty --(-)--> liver
## heart   --(-)--> death
## liver   --(-)--> death


## Think: Make heart and liver time or baseline covariates.

test[, alco_group := cut(alcohol, breaks = c(0,5,10,Inf), labels = c("low", "medium", "high"))]

test[, hist(alcohol, breaks = 100)]

test[, table(event)]
test[, died := 1*("death" %in% event), id]
setorder(test, id, time)
test[event == "visit", visit_nr := 1:.N, by = .(id)]



## test[event == "baseline", 100*prop.table(table(Publish::acut(frailty, 3), died), 1)]
## test[visit_nr == 1, 100*prop.table(table(Publish::acut(frailty, 3), died), 1)]

test[event == "baseline", 100*prop.table(table(alco_group, died), 1)]
test[visit_nr == 1, 100*prop.table(table(alco_group, died), 1)]



## Simulate from J curve: Two fraily levels, pushed alcohol in different directions




