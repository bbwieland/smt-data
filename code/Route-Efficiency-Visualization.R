library(tidyverse)
library(dplyr)
library(readr)
library(ggplot2)
library(ggridges)


final_of_df <- read_csv("route_eff.csv")
oaa_players <- read_csv("oaa-players.csv")

#player route efficiency
route_eff_players <- final_of_df %>% 
  filter(!is.na(route_efficiency)) %>% 
  group_by(player_id) %>% 
  summarise(n = n(), route_eff = mean(route_efficiency))

#make sure in one of 4 teams that played at least 3 games
route_eff_players <- left_join(route_eff_players, team_info, by = c("player_id")) %>% 
  filter(team_id %in% c("TeamA1", "TeamA2", "TeamA3", "TeamB")) %>% 
  select(-team_year) %>% 
  unique()

#make sure num plays isn't lost
route_eff_players <- route_eff_players %>% 
  group_by(player_id, n) %>% 
  summarise(route_eff = mean(route_eff))

#min 10 plays, leaders and laggers in route eff
top <- route_eff_players %>% 
  filter(n >= 10) %>% 
  arrange(desc(route_eff)) %>% 
  head(10)

bottom <- route_eff_players %>% 
  filter(n >= 10) %>% 
  arrange((route_eff)) %>% 
  head(10)

#rbind for graph
re_graph <- rbind(top, bottom)

ggplot(re_graph, aes(x = reorder(player_id, -route_eff), route_eff)) +
  geom_col(alpha = 0.8) +
  theme_bw() +
  labs(
    x = "Player", 
    y = 'Route Efficiency',
    title = "Leaders and Laggers In Route Efficiency",
    subtitle = "Min 10 Plays | Top Ten and Bottom Ten"
  )

#route eff by position
player_ids <- oaa_players %>% 
  select(player_id, position)

re_pos <- left_join(route_eff_players, player_ids, by = c("player_id"))

#remove na
re_pos <- re_pos %>% 
  filter(player_id != 7606)

ggplot(re_pos, aes(x = route_eff, y = reorder(position, route_eff), fill = position)) +
  geom_density_ridges(alpha = 0.6) +
  theme_bw() +
  labs(
    x = "Route Efficiency",
    title = "Route Efficiency By Position",
    y = "Position"
  ) +
  theme(
    legend.position = "top",
    plot.background = element_rect(fill ="white",color = "white"),
  ) +
  stat_density_ridges(quantile_lines = TRUE, quantiles = 0.5)

#by teams
route_eff_teams <- left_join(route_eff_players, team_info, by = c("player_id"))

route_eff_teams <- route_eff_teams %>% 
  filter(team_id %in% c("TeamA1", "TeamA2", "TeamA3", "TeamB")) %>% 
  group_by(team_id) %>% 
  summarise(route_eff = weighted.mean(route_eff, n))

ggplot(route_eff_teams, aes(x = reorder(team_id, -route_eff), route_eff)) +
  geom_col(alpha = 0.6) +
  theme_bw() +
  labs(
    x = "Team", 
    y = 'Route Efficiency',
    title = "Best Teams By Route Efficiency"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 0.5)
  )

#route eff/oaa correlation by players
re_oaa_cor <- inner_join(route_eff_players, oaa_players, by = c("player_id"))

re_oaa_cor <- re_oaa_cor %>% 
  group_by(player_id) %>% 
  summarise(n.y, oaa_per = mean(oaa_per), route_eff = mean(route_eff)) %>% 
  rename(Plays = n.y)

ggplot(re_oaa_cor, aes(x = oaa_per, y = route_eff)) +
  geom_point(aes(color = Plays)) +
  theme_bw() +
  labs(
    x = "OAA Per Play",
    y = "Route Efficiency",
    title = "OAA Compared to Route Efficiency",
    subtitle = "By Players | R: 0.256"
  ) +
  scale_color_viridis() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE, alpha = 0.6) 

#route eff/oaa correlation by teams
oaa_teams <- read_csv("oaa-teams.csv")

re_oaa_teams <- inner_join(route_eff_teams, oaa_teams, by = c("team_id")) %>% 
  rename(plays = n)

re_oaa_teams <- re_oaa_teams %>% 
  group_by(team_id) %>% 
  summarise(plays, oaa_per = weighted.mean(oaa_per, plays), route_eff = weighted.mean(route_eff, plays)) %>% 
  filter(team_id %in% c("TeamA1", "TeamA2", "TeamA3", "TeamB"))

ggplot(re_oaa_teams, aes(x = oaa_per, y = route_eff)) +
  theme_bw() +
  labs(
    x = "OAA Per Play",
    y = "Route Efficiency",
    title = "OAA Compared to Route Efficiency",
    subtitle = "By Teams | R: 0.942"
  ) +
  scale_color_viridis() +
  geom_smooth(method = "lm", linetype = "dashed", se = FALSE, alpha = 0.6) +
  geom_point(aes(color = plays)) 
