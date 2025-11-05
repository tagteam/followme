sample_visit_gap <- function(mean = 3, jitter = 1, min_gap = 0.25) {
  gap <- mean + runif(1, -jitter, jitter)
  max(gap, min_gap)
}
