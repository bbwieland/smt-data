library(tidyverse)
library(sportyR)

library(tidymodels)
library(broom)

model_data = read_csv("Data/model-train-data.csv") %>%
  # removing ground balls
  filter(hit_type %in% c("fly ball","line drive","pop up")) %>%
  # calculating which fielder "should" be responsible for the play — the closest one to the ball
  mutate(nearest_fielder = case_when(distance_from_lf < distance_from_cf & distance_from_lf < distance_from_rf ~ "lf",
                            distance_from_cf < distance_from_lf & distance_from_cf < distance_from_rf ~ "cf",
                            distance_from_rf < distance_from_cf & distance_from_rf < distance_from_lf ~ "rf")) %>%
  # removing outliers due to errors in tracking data 
  filter(next_event_y <= 450)

# fitting the models
lf_model = glm(out_lf ~ distance_from_avg_lf_x*distance_from_avg_lf_y + hit_type, model_data, family = "binomial"(link = "logit"))
cf_model = glm(out_cf ~ distance_from_avg_cf_x*distance_from_avg_cf_y + hit_type, model_data, family = "binomial"(link = "logit"))
rf_model = glm(out_rf ~ distance_from_avg_rf_x*distance_from_avg_rf_y + hit_type, model_data, family = "binomial"(link = "logit"))

#  %>% filter(nearest_fielder == "rf")

# predicting with the models
model_pred = model_data %>%
  mutate(lf_out_prob = predict(lf_model,.,type = "response"),
         cf_out_prob = predict(cf_model,.,type = "response"),
         rf_out_prob = predict(rf_model,.,type = "response"),
         # scaled overall out probability, wrapped between 0 and 1
         overall_out_prob = (((lf_out_prob + cf_out_prob + rf_out_prob) - min(lf_out_prob + cf_out_prob + rf_out_prob))/
                               (max(lf_out_prob + cf_out_prob + rf_out_prob) - min(lf_out_prob + cf_out_prob + rf_out_prob))))

summary(lf_model)
summary(cf_model)
summary(rf_model)

write_csv(model_pred,"Data/oaa-model-results.csv")

