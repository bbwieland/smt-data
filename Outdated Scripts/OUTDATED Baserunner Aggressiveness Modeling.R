library(tidyverse)
library(gt)
library(gtExtras)
setwd("~/Desktop/SMT-Data-Challenge")

game_events = read_csv("game_events.csv")
ball_pos = read_csv("ball_pos.csv")
game_info = read_csv("game_info.csv")
player_pos = read_csv("player_pos.csv")
team_info = read_csv("team_info.csv")

first_base_x = 63.6396
first_base_y = 62.9313
second_base_x = 0
second_base_y = 126.5709
third_base_x = -63.6396
third_base_y = 62.9313

play_types = game_events %>% 
  group_by(game_str,play_id) %>%
  summarise(values = c(event_code)) %>%
  mutate(ball_in_play = ifelse(4 %in% values,T,F)) %>%
  distinct(c(play_id),.keep_all = T) %>% 
  select(game_str,play_id,ball_in_play)

game_events = game_events %>% left_join(play_types,by = c("game_str","play_id"))

balls_in_play = game_events %>% filter(ball_in_play == T)

baserunner_balls_in_play = player_pos %>% 
  inner_join(balls_in_play %>% select(game_str,play_id,ball_in_play),by = c("game_str","play_id")) %>% 
  filter(player_position %in% c(11,12,13) & ball_in_play == T) %>%
  select(-ball_in_play) %>%
  unique()

baserunner_info = game_info %>% select(game_str,play_per_game,first_baserunner,second_baserunner,third_baserunner) %>%
  mutate(play_per_game = play_per_game)

baserunner_leadoff_measurements = baserunner_balls_in_play %>%
  mutate(distance_from_starting_base_x = case_when(player_position == 11 ~ abs(field_x - first_base_x),
                                                   player_position == 12 ~ abs(field_x - second_base_x),
                                                   player_position == 13 ~ abs(field_x - third_base_x)),
         distance_from_starting_base_y = case_when(player_position == 11 ~ abs(field_y - first_base_y),
                                                   player_position == 12 ~ abs(field_y - second_base_y),
                                                   player_position == 13 ~ abs(field_y - third_base_y))) %>%
  mutate(distance_from_starting_base = sqrt(distance_from_starting_base_x^2 + distance_from_starting_base_y^2)) %>%
  group_by(game_str,play_id,player_position) %>% 
  mutate(id = row_number()) %>%
  mutate(last_timestamp = lag(timestamp),last_x = lag(field_x),last_y = lag(field_y),time_since_last_obs = timestamp - last_timestamp,time_since_play_start = timestamp - timestamp[id == 2]) %>%
  mutate(net_movement_since_last_obs = sqrt((field_x - last_x)^2 + (field_y - last_y)^2),estimated_ft_sec = net_movement_since_last_obs*1000/time_since_last_obs, estimated_miles_hr = estimated_ft_sec/1.466667) %>%
  filter(estimated_ft_sec <= 35 & time_since_last_obs <= 1000) %>%
  ungroup() %>%
  left_join(baserunner_info, by = c("game_str",'play_id' = "play_per_game")) %>% 
  mutate(runner_id = case_when(player_position == 11 ~ first_baserunner,
                               player_position == 12 ~ second_baserunner,
                               player_position == 13 ~ third_baserunner)) %>%
  select(-c(first_baserunner,second_baserunner,third_baserunner)) %>%
  filter(runner_id != 0)

baserunner_data_by_player = baserunner_leadoff_measurements %>% 
  group_by(runner_id) %>%
  summarise(estimated_mph_values = list(estimated_ft_sec),observations = n(),max_ft_sec = max(estimated_ft_sec),mean_ft_sec = mean(estimated_ft_sec),pctile_80_ft_sec = quantile(estimated_ft_sec,probs = 0.8)) %>%
  filter(observations >= 250)

baserunner_data_by_player = baserunner_leadoff_measurements %>% 
  group_by(runner_id) %>%
  summarise(estimated_mph_values = list(estimated_ft_sec),observations = n(),max_ft_sec = max(estimated_ft_sec),mean_ft_sec = mean(estimated_ft_sec),pctile_80_ft_sec = quantile(estimated_ft_sec,probs = 0.8)) %>%
  filter(observations >= 250)

baserunner_data_by_player_year = baserunner_leadoff_measurements %>%
  mutate(year = as.numeric(substr(game_str,1,4))) %>%
  group_by(runner_id,year) %>%
  summarise(estimated_mph_values = list(estimated_ft_sec),observations = n(),max_ft_sec = max(estimated_ft_sec),mean_ft_sec = mean(estimated_ft_sec),pctile_80_ft_sec = quantile(estimated_ft_sec,probs = 0.75)) %>%
  filter(observations >= 50)

baserunner_multiyear_info = baserunner_data_by_player_year %>%
  count() %>%
  filter(n > 1)

baserunner_data_by_player_multiyear = baserunner_data_by_player_year %>%
  filter(runner_id %in% baserunner_multiyear_info$runner_id) %>%
  arrange(year) %>%
  group_by(runner_id) %>%
  mutate(last_pctile_80_ft_sec = lag(pctile_80_ft_sec)) %>%
  filter(!is.na(last_pctile_80_ft_sec))

ggplot(baserunner_data_by_player_multiyear,aes(x = last_pctile_80_ft_sec, y = pctile_80_ft_sec)) +
  geom_point() +
  geom_smooth(method = "lm")
## plotting — do NOT need to run the code below this! 

sample_player_data = head(baserunner_data_by_player,10)

sample_data = baserunner_leadoff_measurements[sample(seq(1,nrow(baserunner_leadoff_measurements)),10000),]

ggplot(sample_data,aes(x = field_x,y = field_y)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE,show.legend = FALSE) +
  geom_point(x = 0, y = 60.5,shape = 0,size = 4,color = "white") +
  geom_point(x = 0, y = 0, shape = 5,size = 4,color = "white") +
  geom_point(x = 0, y = 127.27, shape = 5,size = 4,color = "white") +
  geom_point(x = 63.635, y = 63.635, shape = 5,size = 4,color = "white") +
  geom_point(x = -63.635, y = 63.635, shape = 5,size = 4,color = "white") +
  theme_void() +
  theme(legend.position = "top") +
  facet_grid(~player_position)

ggplot(sample_data,aes(x = field_x,y = field_y)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE,show.legend = FALSE) +
  geom_point(x = 0, y = 60.5,shape = 0,size = 4,color = "white") +
  geom_point(x = 0, y = 0, shape = 5,size = 4,color = "white") +
  geom_point(x = 0, y = 127.27, shape = 5,size = 4,color = "white") +
  geom_point(x = 63.635, y = 63.635, shape = 5,size = 4,color = "white") +
  geom_point(x = -63.635, y = 63.635, shape = 5,size = 4,color = "white") +
  theme_void() +
  theme(legend.position = "top") +
  facet_grid(~player_position)

ggplot(sample_data,aes(x = field_x,y = field_y,color = estimated_ft_sec)) + 
  geom_tile() +
  geom_point(x = 0, y = 60.5,shape = 0,size = 4,color = "black") +
  geom_point(x = 0, y = 0, shape = 5,size = 4,color = "black") +
  geom_point(x = 0, y = 127.27, shape = 5,size = 4,color = "black") +
  geom_point(x = 63.635, y = 63.635, shape = 5,size = 4,color = "black") +
  geom_point(x = -63.635, y = 63.635, shape = 5,size = 4,color = "black") +
  theme_void() +
  theme(legend.position = "top") +
  facet_wrap(~player_position)

gt(sample_player_data) %>%
  gtExtras::gt_plt_dist(estimated_mph_values)
  
  
  
