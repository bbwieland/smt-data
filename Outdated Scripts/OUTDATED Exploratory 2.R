library(tidyverse)
library(gganimate)
setwd("~/Desktop/SMT-Data-Challenge")

plays = read_csv("baseball_comp.csv")
player_pos = read_csv("player_pos.csv")
ball_pos = read_csv("ball_pos.csv")

getPlay = function(game,play,plot = F) {
  if (plot == F) {
    play_data = plays %>% 
      filter(game_str %in% game & play_id %in% play)
    
    ball_data = ball_pos %>%
      filter(game_str %in% game & play_id %in% play)
    
    player_data = player_pos %>%
      filter(game_str %in% game & play_id %in% play)
    
    data = left_join(player_data,ball_data,by = c("timestamp","game_str","play_id")) %>%
      filter(player_position %in% c(1:13) & is.na(ball_position_x) == F)
    
    return(data)
  }
  
  if (plot == T) {
    
    play_data = plays %>% 
      filter(game_str %in% game & play_id %in% play)
    
    ball_data = ball_pos %>%
      filter(game_str %in% game & play_id %in% play)
    
    player_data = player_pos %>%
      filter(game_str %in% game & play_id %in% play)
    
    data = left_join(player_data,ball_data,by = c("timestamp","game_str","play_id")) %>%
      filter(player_position %in% c(1:13) & is.na(ball_position_x) == F)
    
    plot = ggplot(data) +
      geom_point(aes(x = field_x, y = field_y,group = player_position),show.legend = F,size = 4,color = ifelse(data$player_position %in% seq(1,9),"blue","green")) + 
      geom_point(aes(x = ball_position_x,y = ball_position_y,size = 3 + log10(ball_position_z)), color = "red",show.legend = F) +
      scale_x_continuous(limits = c(-150,150)) +
      scale_y_continuous(limits = c(-10,400)) +
      geom_point(x = 0, y = 60.5,shape = 0,size = 4) +
      geom_point(x = 0, y = 0, shape = 5,size = 4) +
      geom_point(x = 0, y = 127.27, shape = 5,size = 4) +
      geom_point(x = 63.635, y = 63.635, shape = 5,size = 4) +
      geom_point(x = -63.635, y = 63.635, shape = 5,size = 4) +
      geom_text(aes(label = paste("x:",round(ball_position_x,1))),x = 63.635,y = 40) +
      geom_text(aes(label = paste("y:",round(ball_position_y,1))),x = 63.635,y = 25) +
      geom_text(aes(label = paste("z:",round(ball_position_z,1))),x = 63.635,y = 10) +
      theme_void() +
      transition_states(timestamp) +
      labs(subtitle = "Frame {frame} of {nframes}",
           title = paste0(game," - Play ",play)) +
      theme(plot.subtitle = element_text(hjust = 0.5),
            plot.title = element_text(hjust = 0.5),
            plot.caption = element_text(hjust = 0.5))
    
    animation = animate(plot,nframes = length(unique(data$timestamp)))
    
    return(animation)
  }
}

# example of the function animation
getPlay("1903_25_TeamNK_TeamB",63,plot = T)

