### plot_dropout.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr 23 2026 (17:08) 
## Version: 
## Last-Updated: Apr 27 2026 (12:47) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 9
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

dropout_data <- function(dt) {
  # Work on a copy to avoid modifying input by reference
  dt <- copy(dt)

  # Step 1: Select censored events
  censored_events <- dt[
    event == "dropout"
  ]
 
  # Keep first occurrence per id and recode as 0
  censored_events[, event := 0L]

  # Step 1.5: Dropout events that are visits without SGLT2 treatment
  dropout_events <- dt[
    (event == "visit" & SGLT2 == 0) 
  ]

  setkey(dropout_events, id, time)
  dropout_events <- dropout_events[, .SD[1L], by = id]
  dropout_events[, event := 1L]

  # Step 2: Select outcome events (death, mace)
  outcome_events <- dt[event %in% c("death", "mace")]

  # Recode: 2
  outcome_events[, event := 2]

  # Step 3: Combine and take first event per id
  result <- rbind(censored_events, dropout_events, outcome_events)[
    order(id, time), .SD[1L], by = id
  ]

  return(result[])
}

plot_dropout <- function(dt) {
  dropout_res <- dropout_data(dt)
  ## fit <- prodlim::prodlim(Hist(time, event) ~ 1, data = dropout_res)
  ggplot(data = dropout_res, aes(x = time, event = event)) +
    prodlim::geom_prodlim(cause = 1, type = "risk")
}

######################################################################
### plot_dropout.R ends here
