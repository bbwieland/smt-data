library(tidyverse)
library(sportyR)

events = read_csv("Data/game_events.csv")
ball = read_csv("Data/ball_pos.csv")
player = read_csv("Data/player_pos.csv")
gameinfo = read_csv("Data/game_info.csv")

# importing the data created via DataCreation.R 
balls_in_play = read_csv("Data/balls-in-play-index.csv")

# obtaining the timestamps of the event immediately following a ball batted into play
next_event_timestamps = events %>%
  group_by(game_str,play_id) %>%
  filter(4 %in% event_code) %>%
  filter(lag(event_code) == 4) %>%
  select(game_str,play_id,next_event_timestamp = timestamp) %>%
  distinct(game_str,play_id,.keep_all = T)

# obtaining the maximum ball locations (x,y) from home plate between hit & next ball event
max_ball_locations = ball %>% 
  left_join(next_event_timestamps) %>%
  group_by(game_str,play_id) %>%
  filter(timestamp <= next_event_timestamp) %>%
  mutate(max_x = max(abs(ball_position_x)),
         max_y = max(abs(ball_position_y))) %>%
  mutate(outfield = ifelse(sqrt(max_x^2 + max_y^2) >= 150,T,F)) %>%
  distinct(game_str,play_id,.keep_all = T)

# list of balls hit more than 150 ft from home plate ("outfield")
of_balls = max_ball_locations %>% filter(outfield == T) %>%
  select(game_str,play_id)

# obtaining the average LF location across all plays
lf_x = mean((player %>% filter(player_position == 7))$field_x)
lf_y = mean((player %>% filter(player_position == 7))$field_y)

# obtaining the average CF location across all plays
cf_x = mean((player %>% filter(player_position == 8))$field_x)
cf_y = mean((player %>% filter(player_position == 8))$field_y)

# obtaining the average RF location across all plays
rf_x = mean((player %>% filter(player_position == 9))$field_x)
rf_y = mean((player %>% filter(player_position == 9))$field_y)

# Determining the player responsible for the event immediately following a batted ball
hit_info = events %>%
  group_by(game_str,play_id) %>%
  filter(4 %in% event_code) %>%
  mutate(next_player = lead(player_position)) %>%
  filter(event_code == 4)

# Combining balls_in_play, hit_info, and filtering to only of_balls via an inner join
of_plays = left_join(balls_in_play,hit_info) %>%
  inner_join(of_balls) %>%
  select(-at_bat)

# Creating the data for modeling
model_data = left_join(gameinfo,of_plays,by = c("game_str","play_per_game")) %>%
  # determining if the batter got on base on play N+1 as a result of play N
  mutate(got_on_base = ifelse(batter == lead(first_baserunner) | batter == lead(second_baserunner) | batter == lead(third_baserunner),1,0)) %>%
  filter(is.na(final_x) == F) %>%
  # determining which position was responsible for the out
  mutate(out_lf = ifelse(got_on_base == 0 & next_player == 7,1,0),
         out_cf = ifelse(got_on_base == 0 & next_player == 8,1,0),
         out_rf = ifelse(got_on_base == 0 & next_player == 9,1,0)) %>%
  # determining the Pythagorean distance, sqrt(x^2 + y^2), from the average LF/CF/RF locations of each of the ball locations during the next event after a batted ball
  mutate(distance_from_avg_lf_x = abs(next_event_x - lf_x),
         distance_from_avg_lf_y = abs(next_event_y- lf_y),
         distance_from_avg_cf_x = abs(next_event_x - cf_x),
         distance_from_avg_cf_y = abs(next_event_y- cf_y),
         distance_from_avg_rf_x = abs(next_event_x - rf_x),
         distance_from_avg_rf_y = abs(next_event_y- rf_y),
         distance_from_lf = sqrt(distance_from_avg_lf_x^2 + distance_from_avg_lf_y^2),
         distance_from_cf = sqrt(distance_from_avg_cf_x^2 + distance_from_avg_cf_y^2),
         distance_from_rf = sqrt(distance_from_avg_rf_x^2 + distance_from_avg_rf_y^2)) %>% 
  # removing plays without a next_event ball location (foul balls)
  filter(is.na(next_event_x) == F & is.na(next_event_y) == F)

# Saving the dataframe
write_csv(model_data,"Data/model-train-data.csv")

