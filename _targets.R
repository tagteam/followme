### _targets.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: okt 23 2025 (15:22) 
## Version: 
## Last-Updated: nov  5 2025 (07:27) 
##           By: Thomas Alexander Gerds
##     Update #: 6
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
### _targets.R --- 
library(targets)
library(lava)
library(data.table)
tar_source("functions")
tar_option_set(packages = c("lava","survival","data.table","prodlim","rtmle","foreach"))
list(
    tar_target(name = diabetes_polypharmacy_setting,{
        command =
            get_diabetes_polypharmacy_setting()
    }),
    tar_target(name = diabetes_population,{
        command =
            do.call("simulate_cohort",
                    c(list(n = 20),
                      diabetes_polypharmacy_setting))
    },cue = tar_cue(mode = "thorough"))
)



######################################################################
### _targets.R ends here
