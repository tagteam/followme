library(targets)
library(lava)
library(foreach)
library(data.table)
tar_source("functions")
tar_make()
s <- tar_read(diabetes_polypharmacy_setting)
## dput(s$parameter_values)
s$parameter_values$intercept_age <- 50
s$parameter_values$intercept_HbA1c <- 60
s$parameter_values$effect_U_age = 12.9
s$parameter_values$effect_U_HbA1c = 8.3
test <- do.call("simulate_person",s)
d <- do.call("simulate_cohort",c(list(n = 20),s))
d

# timeline ####

## example code ####

# Required libraries
library(ggplot2)
library(ggimage)
library(dplyr)

# Example data - patient 1
timeline_data <- data.frame(
  PatientID = "Patient_01",
  Event = c("Baseline", "Visit", "Dropout"),
  Date = as.Date(c("2023-01-03", "2023-01-04", "2023-01-07"))
)

# Example data - patient 2
timeline_data <- data.frame(
  PatientID = "Patient_02",
  Event = c("Baseline", "Visit", "Death"),
  Date = as.Date(c("2023-01-03", "2023-01-04", "2023-01-07"))
)

# Example data - patient 3
timeline_data <- data.frame(
  PatientID = "Patient_03",
  Event = c("Baseline", "Visit", "MACE"),
  Date = as.Date(c("2023-01-03", "2023-01-04", "2023-01-07"))
)


# Example mapping from Event to icon file
image_options <- data.frame(Event = c("Baseline", "Visit", "Dropout", "Death", "MACE"), 
                            image = c(
  "https://images.emojiterra.com/google/android-10/512px/1fa7a.png",
  "https://images.emojiterra.com/google/android-12l/512px/1f3e5.png",
  "https://images.emojiterra.com/mozilla/1024px/2753.png",
  "https://cdn-icons-png.flaticon.com/512/2699/2699654.png",
  "https://cdn-icons-png.flaticon.com/512/7292/7292483.png"
))

timeline_data <- timeline_data %>% 
  inner_join(image_options)

segment_data <- timeline_data %>%
  mutate(xend = lead(Date), seg_col = Event) %>%
  filter(!is.na(xend))


ggplot(timeline_data, aes(x = Date, y = PatientID)) +
  geom_line(aes(group = PatientID), linetype = "dashed") +
  geom_segment(
    data = segment_data,
    aes(x = Date, xend = xend, y = y_pos, yend = y_pos, color = seg_col),
    size = 2
  ) +
  #geom_point(size = 4) +
  geom_image(aes(image = image), size = 0.10) +
  geom_text(aes(label = Event), vjust = -4) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", name = "Treatment") +
  labs(
    title = "Patient Timeline",
    x = "Time",
    y = "Patient"
  )

#  geom_image(aes(image = image), size = 0.10, position = position_nudge(y = -0.1)) +

## code for simulation ####

# Required libraries
library(ggplot2)
library(ggimage)
library(dplyr)

set.seed(1234)

d_test <- d[which(d$id %in% sample(d$id, 5, replace = T)),]

# Example data - patient 1
timeline_data <- data.frame(
  id = d_test$id,
  Event = d_test$event,
  Date = d_test$time,
  GLP1 = d_test$GLP1,
  SGLT2 = d_test$SGLT2,
  DPP4 = d_test$DPP4) %>% 
  rowwise() %>% 
  mutate(PatientID = paste0("Patient ", toString(unique(id))),
         Regime = case_when((GLP1 == 1) & (SGLT2 == 0) & (DPP4 == 0) ~ "GLP1",
                     (GLP1 == 0) & (SGLT2 == 1) & (DPP4 == 0) ~ "SGLT2",
                     (GLP1 == 0) & (SGLT2 == 0) & (DPP4 == 1) ~ "DPP4",
                     (GLP1 == 1) & (SGLT2 == 1) & (DPP4 == 0) ~ "GLP1 + SGLT2",
                     (GLP1 == 1) & (SGLT2 == 0) & (DPP4 == 1) ~ "GLP1 + DPP4",
                     (GLP1 == 0) & (SGLT2 == 1) & (DPP4 == 1) ~ "SGLT2 + DPP4",
                     (GLP1 == 1) & (SGLT2 == 1) & (DPP4 == 1) ~ "GLP1 + SGLT2 + DPP4",
                     (GLP1 == 0) & (SGLT2 == 0) & (DPP4 == 0) ~ "No treatment")
)



# Example mapping from Event to icon file
image_options <- data.frame(Event = c("baseline", "visit", "dropout", "death", "MACE"), 
                            image = c(
                              "https://images.emojiterra.com/google/android-10/512px/1fa7a.png",
                              "https://images.emojiterra.com/google/android-12l/512px/1f3e5.png",
                              "https://images.emojiterra.com/mozilla/1024px/2753.png",
                              "https://cdn-icons-png.flaticon.com/512/2699/2699654.png",
                              "https://cdn-icons-png.flaticon.com/512/7292/7292483.png"
                            ))

timeline_data <- timeline_data %>% 
  inner_join(image_options)

segment_data <- timeline_data %>%
  group_by(PatientID) %>% 
  mutate(xend = lead(Date), seg_col = Regime) %>%
  filter(!is.na(xend)) %>%
  ungroup()



ggplot(timeline_data, aes(x = Date, y = PatientID, y_pos = PatientID)) +
  geom_line(aes(group = PatientID), linetype = "dashed") +
  geom_segment(
    data = segment_data,
    aes(x = Date, xend = xend, y = PatientID, yend = PatientID, color = seg_col),
    size = 2
  ) +
  #geom_point(size = 4) +
  geom_image(aes(image = image, y_pos = PatientID), size = 0.10) +
  geom_text(aes(label = Event), vjust = -3.5) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1", name = "Treatment") +
  labs(
    title = "Patient Timelines",
    x = "Time",
    y = "Patient"
  )





# Anna ####

# baseline covariates: normal or binomial distribution depending on continuous or binary covariate
# absorbing states: death or discontinuation - Weibull distribution 
# - with what parameter lambda? 
#        - <1: high initial failure
#        - = 1: equal rate across time
#        - >1: failure rate increase over time
# intermediate events: MACE - Weibull distribution
# visit_measurements: HbA1c - normal distribution
# visit events: 
# - GLP1 / SGLT2 / DPP4: binomial
# - History of GLP1 / SGLT2 / DPP4: constant
# visit schedule: mean, sd, skip - What is the skip? - 6 visits or do you vary the visit strucuture?
# parameter values: are these all interaction effects?

# Questions
# - You require a measurement at a doctors visit?
# - What would truth look like?
# - simulate person vs patient - what is the difference?
# - why do you need the sample visit gap? That it cannot happen immediately after?
# - Why not use somehting like this? - https://github.com/XiLinStats/LongSiMSM

# install.packages(
#   "https://cran.r-project.org/src/contrib/Archive/gsl/gsl_2.1-8.tar.gz",
#   repos = NULL,
#   type = "source",
#   configure.args = "--with-gsl-config=$HOME/.local/bin/gsl-config"
# )
# 
# install.packages(c("copula"))  
# 
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/gsl/gsl_2.1-1.tar.gz"
# utils::install.packages(pkgs = packageurl, repos = NULL, type = "source")
# 
# library(data.table)
# library(copula)
# 
# expit <- function(x) {
#   1 / (1 + exp(-x))
# }
# 
# sim_func <- function(n=500,beta = -0.3){
#   
#   # Baseline ----------------------------------------------------------------
#   
#   # obesity
#   B <- rbinom(n,1,0.1)
#   # age
#   C <- runif(n,25,35)
#   # L_0
#   
#   w0 <- runif(n)
#   L_0 <- qnorm(w0, mean  = 11-0.05*B -0.02*C, sd = 0.5)
#   A_0 <- rbinom(n,1,0.5)
#   
#   # Later time points -------------------------------------------------------
#   w1 <- runif(n)
#   L_1 <- qnorm(w1, mean  = L_0 + 0.5*A_0, sd = 0.1)
#   A_1 <- A_0
#   
#   w2 <- runif(n)
#   L_2 <- qnorm(w2, mean  = L_1 + 0.5*A_1, sd = 0.1)
#   A_2 <- A_0
#   w3 <- runif(n)
#   L_3 <- qnorm(w3, mean  = L_2 + 0.5*A_2, sd = 0.1)
#   A_3 <- rbinom(n,1,expit(20 - 2*L_3 + 0.05*B + 0.01 *C))
#   
#   w4 <- runif(n)
#   L_4 <- qnorm(w4, mean  = L_3 + 0.5*A_3, sd = 0.1)
#   A_4 <- rbinom(n,1,expit(20 - 2*L_4  + 0.05*B + 0.01 *C))
#   
#   
#   # Simulate Y --------------------------------------------------------------
#   
#   cop <- tCopula(param = -0.5, df = 5, dim = 2, dispstr = "un")
#   
#   wy <- runif(n)
#   
#   nu4 <- cCopula(as.matrix(cbind(w4,wy)),copula = cop,inverse = T)[,2]
#   nu3 <- cCopula(as.matrix(cbind(w3,nu4)),copula = cop,inverse = T)[,2]
#   nu2 <- cCopula(as.matrix(cbind(w2,nu3)),copula = cop,inverse = T)[,2]
#   nu1 <- cCopula(as.matrix(cbind(w1,nu2)),copula = cop,inverse = T)[,2]
#   nu0 <- cCopula(as.matrix(cbind(w0,nu1)),copula = cop,inverse = T)[,2]
#   
#   Y  <- qbinom(nu0,size = 1, prob = expit(-2 + 0.1*B + 0.02*C + beta *(A_0 + A_1 +A_2 + A_3 + A_4)))
#   
#   
#   dat <- data.table(B = B, C= C,
#                     A_0 = A_0, L_0 = L_0, Y_0 = 0,
#                     A_1 = A_1, L_1 = L_1, Y_1 = 0,
#                     A_2 = A_2, L_2 = L_2, Y_2 = 0,
#                     A_3 = A_3, L_3 = L_3, Y_3 = 0,
#                     A_4 = A_4, L_4 = L_4, Y_4 = Y)
#   
#   
#   return(dat)
# }
# 
# sim_func()