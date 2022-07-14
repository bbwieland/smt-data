library(tidyverse)
library(sportyR)

events = read_csv("Data/game_events.csv")
ball = read_csv("Data/ball_pos.csv")
player = read_csv("Data/player_pos.csv")

outfielders = c(7,8,9)

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

ggplot(outfield_catches, aes(group = PlayIndex, x = field_x, y = field_y))
geom_baseball(league = "mlb") +
  geom_point(outfield_catches %>% filter(event_code == 2), 
             mapping = aes(group = PlayIndex, x = field_x, y = field_y, color = of_player_position),show.legend = F) +
  scale_x_continuous(limits = c(-250,250)) +
  scale_y_continuous(limits = c(-50,450))

geom_baseball(league = "mlb") +
  geom_point(outfield_catches %>% filter(event_code == 4), 
             mapping = aes(group = PlayIndex, x = field_x, y = field_y, color = of_player_position),show.legend = F) +
  scale_x_continuous(limits = c(-250,250)) +
  scale_y_continuous(limits = c(-50,450))
