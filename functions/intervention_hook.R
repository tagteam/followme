### intervention_hook.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr 24 2026 (11:34) 
## Version: 
## Last-Updated: Apr 24 2026 (15:13) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 96
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

intervention_hook_percentage_sglt2_long <- function(data, delay = 3, treatment_name = "SGLT2") {
  dt <- data[, .(id, time, A = get(treatment_name))]
  ## Check if order is already sorted by id and time, if not sort it
  if (!key(dt)[1] == "id" || key(dt)[2] != "time") {
      setorder(dt, id, time)
  }
  
  # Compute time_relevant once per group
  dt[, time_relevant := max(max(time) - delay, 0), by = id]
  
  # Find last time <= time_relevant via rolling join
  dt[, is_last_relevant_time := FALSE]
  dt[dt,
     on = .(id, time <= time_relevant),
     mult = "last",
     is_last_relevant_time := TRUE]
  
  # Adjust time at cutoff
  dt[is_last_relevant_time == TRUE, time := time_relevant]
  
  # Keep only relevant window
  dt <- dt[time >= time_relevant]
  
  # Compute intervals
  dt[, next_time := shift(time, type = "lead"), by = id]
  
  # Final aggregation
  dt[!is.na(next_time),
     .(time_treated = sum(A * (next_time - time)) / delay),
     by = id]
}

## intervention_hook_percentage_sglt2_long <- function(data, delay = 3, treatment_name = "SGLT2") {
##     dt <- copy(data)[, c("id", "time", treatment_name), with = FALSE]
##     setnames(dt, treatment_name, "A")
##     dt[, time_relevant := pmax(max(time) - delay, 0), by = "id"]
##     dt[, is_last_relevant_time := max(time[time <= time_relevant]) == time, by = id]
##     dt[is_last_relevant_time == TRUE, time := time_relevant]
##     dt <- dt[time >= time_relevant]
##     dt[, next_time := shift(time, type = "lead", fill = NA), by = id]
##     return(dt[!is.na(next_time), .(time_treated = sum(A * (next_time - time)) / delay), by = id])
## }

intervention_hook_percentage_sglt2_wide <- function(data, k, delay = 3, treatment_name = "A") {
    dt <- copy(data)[, c("id", paste0("time_", 0:k), paste0("event_", 0:k), paste0(paste0(treatment_name,"_"), 0:k)), with = FALSE]
    dt <- melt(
        dt,
        id.vars = "id",
        measure = list(cols_A, cols_time, cols_event),
        value.name = c(treatment_name, "time", "event"),
        variable.name = "t"
    )[order(id, t)][, t := NULL]
    dt <- intervention_hook_percentage_sglt2_long(dt, delay = delay, treatment_name = treatment_name)
    data[, paste0(treatment_name,"_percentage_", k) := dt[["time_treated"]]]
    return(data[])
}

######################################################################
### intervention_hook.R ends here
