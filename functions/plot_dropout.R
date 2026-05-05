### plot_dropout.R --- 
#----------------------------------------------------------------------
## Author: Johan Sebastian Ohlendorff
## Created: Apr 23 2026 (17:08) 
## Version: 
## Last-Updated: Apr 30 2026 (10:12) 
##           By: Johan Sebastian Ohlendorff
##     Update #: 23
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

dropout_data <- function(dt) {
  base_events <- dt[
    (event == "visit" & SGLT2 == 0) | event %in% c("dropout", "mace", "death")
  ]
  base_events[event == "dropout", event := 0L]
  base_events[event == "visit" & SGLT2 == 0, event := 1L]
  base_events[event %in% c("mace", "death"), event := 2L]
  base_events[, event := as.numeric(event)]

  setkey(base_events, id, time)
  base_events <- base_events[, .SD[1L], by = id][, .(id, time, event)]
  return(base_events[])    
}

plot_dropout <- function(dt) {
  dropout_res <- dropout_data(dt)
  ## fit <- plot(prodlim::prodlim(Hist(time, event) ~ 1, data = dropout_res),cause = 1)
  ggplot(data = dropout_res, aes(x = time, event = event)) +
    prodlim::geom_prodlim(cause = 1, type = "risk")
}

######################################################################
### plot_dropout.R ends here
