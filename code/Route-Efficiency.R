library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)

player_pos <- read_csv("player_pos.csv")
ball_pos <- read_csv("ball_pos.csv")
game_events <- read_csv("game_events.csv")
game_info <- read_csv("game_info.csv")
team_info <- read_csv("team_info.csv")

View(player_pos)
View(game_events)
View(game_info)

nrow(game_events)
nrow(player_pos)

#outfielder positions
outfielder <- c(7, 8, 9)

#ben's dataset of all balls to outfielders
of_events <- read_csv("model-train-data.csv")

View(of_events)

#filter to just outfield or ball dropped events
of_events <- of_events %>% 
  filter(next_player %in% outfielder | next_player == 255)
  
#graph all fly balls that dropped
ggplot(of_events, aes(x = next_event_x, y = next_event_y))
sportyR::geom_baseball(league = "mlb") +
  geom_point(of_events %>% filter(got_on_base == 1, hit_type == "fly ball"), 
             mapping = aes(x = next_event_x, y = next_event_y, color = next_player),show.legend = F) +
  scale_x_continuous(limits = c(-250,250)) +
  scale_y_continuous(limits = c(-50,450))

#when needed use, lead() and lag() to get start and end time
#straight line distance (optimal route) = x,y distance between player at start and end times (timestamps for this)
#total distance traveled = sum of x,y distances between start and end times at each timestamp
#acknowledge flaws in the paper

View(player_pos)

#add diff between each step
player_pos <- player_pos %>% 
  group_by(game_str, play_id, player_position) %>% 
  mutate(distance_x = abs(field_x - lag(field_x)), distance_y = abs(field_y - lag(field_y)))

#total distance traveled
total_distance_traveled <- player_pos %>% 
  group_by(game_str, play_id, player_position) %>% 
  #outfielders only
  filter(player_position %in% outfielder, !is.na(distance_x), !is.na(distance_y)) %>% 
  summarise(total_distance_x = sum(distance_x), total_distance_y = sum(distance_y), timestamp_play_end = max(timestamp))

View(total_distance_traveled)

#get straight line distance for each player
straight_distance <- player_pos %>% 
  group_by(game_str, play_id, player_position) %>% 
  #outfielders only
  filter(player_position %in% outfielder) %>% 
  summarise(straight_line_distance_x = abs(first(field_x) - last(field_x)), straight_line_distance_y = 
              abs(first(field_y) - last(field_y)), timestamp_play_end = max(timestamp))

View(straight_distance)

#combine dfs
route_eff <- inner_join(total_distance_traveled, straight_distance, by = c("game_str", "play_id", 
                                                                           "timestamp_play_end", "player_position"))

#sanity check
View(route_eff)
nrow(route_eff)

#remove outliers and get total distance/route eff
route_eff <- route_eff %>% 
  filter(straight_line_distance_x <= total_distance_x, straight_line_distance_y <= total_distance_y) %>% 
  mutate(total_distance = sqrt(total_distance_x * total_distance_x + total_distance_y * total_distance_y),
         straight_line_distance = sqrt(straight_line_distance_x * straight_line_distance_x + straight_line_distance_y *
                                         straight_line_distance_y),
         route_efficiency = straight_line_distance / total_distance)

#sanity check
nrow(route_eff)

#join to of_events
nrow(of_events)

#edit 255s in of_events to closest outfielder
of_events <- of_events %>% 
  mutate(next_player = if_else(next_player == 255, 
                               case_when(
                                 distance_from_lf < distance_from_cf & distance_from_lf < distance_from_rf ~ 7,
                                 distance_from_cf < distance_from_lf & distance_from_cf < distance_from_rf ~ 8,
                                 distance_from_rf < distance_from_cf & distance_from_rf < distance_from_lf ~ 7,
                               ),
                               next_player))

final_of_df <- left_join(of_events, route_eff, by = c("game_str", "play_id", "next_player" = "player_position"))

#add player id
final_of_df <- final_of_df %>% 
  mutate(player_id = case_when(
    next_player == 9 ~ right_field,
    next_player == 8 ~ center_field, 
    next_player == 7 ~ left_field
  ))

#final csv for outfielders with route eff
write_csv(final_of_df, "route_eff.csv")
