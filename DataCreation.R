library(tidyverse)
library(sportyR)

# Importing the data

events = read_csv("Data/game_events.csv")
ball = read_csv("Data/ball_pos.csv")
player = read_csv("Data/player_pos.csv")
gameinfo = read_csv("Data/game_info.csv") 

# Creating a dataset of batter on base vs. not on base outcomes
# This variable can be used as a proxy for out/safe distinctions

gameoutcomes = gameinfo %>%
  mutate(batter_state = ifelse(batter == lead(batter),"same",
                               ifelse(batter == lead(first_baserunner),"onbase",
                                      ifelse(batter == lead(first_baserunner),"onbase",
                                             ifelse(batter == lead(first_baserunner),"onbase","not-onbase"))))) %>%
  select(game_str,play_per_game,batter_state)

# Dataframe containing the timestamps of the next event following a ball in play 

next_event_timestamps = events %>%
  group_by(game_str,play_id) %>%
  filter(4 %in% event_code) %>%
  filter(lag(event_code) == 4) %>%
  select(game_str,play_id,next_event_timestamp = timestamp) %>%
  distinct(game_str,play_id,.keep_all = T)

# Dataframe containing the furthest distance, in feet, the ball traveled from home plate between contact and the next event

max_ball_locations = ball %>% 
  left_join(next_event_timestamps) %>%
  group_by(game_str,play_id) %>%
  filter(timestamp <= next_event_timestamp) %>%
  mutate(max_x = max(abs(ball_position_x)),
         max_y = max(abs(ball_position_y))) %>%
  # "outfield" = max distance from home plate of 150 feet or greater
  mutate(outfield = ifelse(sqrt(max_x^2 + max_y^2) >= 150,T,F)) %>%
  distinct(game_str,play_id,.keep_all = T)

# A dataset containing only the play IDs of plays where a batter made contact & hit the ball more than 150 feet from home plate before the next event occurred

of_balls = max_ball_locations %>% filter(outfield == T) %>%
  select(game_str,play_id)

# Defining "outfielders" — a vector of the positions of left field (7), center field (8), and right field (9)
outfielders = c(7,8,9)

# Every play where event code 2 occurred immediately after the ball was put in play
outfield_catches = events %>%
  filter((event_code == 2 & player_position %in% outfielders & lag(event_code) == 4 & lag(player_position) == 10) | 
           (event_code == 4 & player_position == 10 & lead(event_code) == 2 & lead(player_position) %in% outfielders)) %>%
  mutate(PlayIndex = rep(1:(2352/2), each = 2)) %>%
  # joining the ball data
  left_join(ball,by = c("game_str","play_id","timestamp")) %>%
  # cleaning the ball data
  group_by(PlayIndex) %>%
  mutate(of_player_position = min(player_position)) %>%
  filter(is.na(ball_position_x) == F) %>%
  filter(n() > 1) %>%
  ungroup() %>%
  # joining the player data
  left_join(player,by = c("game_str","play_id","timestamp","of_player_position" = "player_position")) %>% 
  # cleaning the player data
  group_by(PlayIndex) %>%
  filter(is.na(field_x) == F) %>%
  filter(n() > 1) %>%
  ungroup() 

# Every event where a ball was put in play to the outfield, filtered to the relevant ball events: the hit & the next immediate event
balls_in_play = events %>%
  inner_join(of_balls) %>%
  group_by(game_str,play_id) %>%
  filter(4 %in% event_code) %>% 
  filter(lag(event_code) == 4) %>%
  select(game_str,play_id)

# Dataframe used to obtain timestamps of next ball event
balls_in_play_next_event = events %>%
  group_by(game_str,play_id) %>%
  filter(4 %in% event_code) %>% 
  filter(lag(event_code) == 4) %>% 
  select(game_str,play_id,timestamp) %>%
  mutate(next_event = T)

# Dataframe used to obtain timestamp of batter making contact with ball event
ball_contact_moments = events %>%
  group_by(game_str,play_id) %>%
  filter(4 %in% event_code) %>% 
  filter((event_code) == 4) %>%
  select(game_str,play_id,timestamp) %>%
  mutate(contact_made = T)

# Combined dataframe of the above three dataframes. 
ball_balls_in_play = left_join(balls_in_play,ball,by = c("game_str","play_id")) %>%
  left_join(ball_contact_moments,by = c("game_str","play_id","timestamp")) %>%
  left_join(balls_in_play_next_event,by = c("game_str","play_id","timestamp")) %>%
  group_by(game_str,play_id) %>%
  fill(contact_made,.direction = "down") %>%
  fill(next_event,.direction = "down") %>%
  # We only want timestamps *after* the ball was put into play
  filter(contact_made == T) %>%
  mutate(index = seq(1:n())) %>%
  # Using trigonometry to calculate the launch angle of the ball
  mutate(horiz_change = sqrt((ball_position_x[2] - ball_position_x[1])^2 + (ball_position_y[2] - ball_position_y[1])^2),
         vert_change = ball_position_z[2] - ball_position_z[1],
         launch_angle = atan(vert_change/horiz_change)*180/pi,
         # Assigning a "hit type" variable based on MLB classifications for ground ball, line drive, fly ball, pop up
         hit_type = case_when(launch_angle < 10 ~ "ground ball",
                              launch_angle < 25 ~ "line drive",
                              launch_angle < 50 ~ "fly ball",
                              launch_angle >= 50 ~ "pop up"),
         final_x = ball_position_x[n()],
         final_y = ball_position_y[n()])

# Dataset containing the x, y, z locations of the ball at the moment of the ball event following the ball being put into play
ball_balls_next_event = ball_balls_in_play %>%
  filter(next_event == T) %>%
  group_by(game_str,play_id) %>%
  mutate(next_event_x = ball_position_x[1],
         next_event_y = ball_position_y[1],
         next_event_z = ball_position_z[1]) %>%
  select(game_str,play_id,next_event_x,next_event_y,next_event_z) %>%
  distinct(game_str,play_id,.keep_all = T)

# Final dataframe
balls_in_play_index = ball_balls_in_play %>%
  distinct(game_str,play_id,.keep_all = T) %>%
  left_join(ball_balls_next_event,by = c("game_str","play_id"))

# Final dataframe with relevant variables selected
balls_in_play_index_final = balls_in_play_index %>% 
  select(game_str,play_id,launch_angle,hit_type,next_event_x,next_event_y,next_event_z,final_x,final_y)

# Saving the dataframe
write_csv(balls_in_play_index_final,"balls-in-play-index.csv")

