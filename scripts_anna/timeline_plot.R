
# Create a script for a plot that evaluates the timeline of multiple patient trajectories.


# Required libraries
library(ggplot2)
library(ggimage)
library(dplyr)
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

