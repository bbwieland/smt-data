library(tidyverse)
library(gganimate)
setwd("~/Desktop/SMT-Data-Challenge")

game_events = read_csv("game_events.csv")
ball_pos = read_csv("ball_pos.csv")
game_info = read_csv("game_info.csv")
player_pos = read_csv("player_pos.csv")
team_info = read_csv("team_info.csv")

getPlay = function(game,play) {
  play_data = plays %>% 
    filter(game_str %in% game & play_id %in% play)
  
  ball_data = ball_pos %>%
    filter(game_str %in% game & play_id %in% play)
  
  player_data = player_pos %>%
    filter(game_str %in% game & play_id %in% play)
  
  data = left_join(player_data,ball_data,by = c("timestamp","game_str","play_id")) 
  
  return(data)
}

play = getPlay("1903_32_TeamNB_TeamA1",1)

test = ball_pos[1:15,]

animation = ggplot(test,aes(x = ball_position_x,y = ball_position_y)) +
  geom_point() +
  scale_x_continuous(limits = c(-130,130)) +
  scale_y_continuous(limits = c(-10,350)) +
  transition_states(timestamp) +
  geom_point(x = 0, y = 60.5,shape = 0) +
  geom_point(x = 0, y = 0, shape = 5) +
  geom_point(x = 0, y = 127.27, shape = 5) +
  geom_point(x = 63.635, y = 63.635, shape = 5) +
  geom_point(x = -63.635, y = 63.635, shape = 5) +
  theme_void()
