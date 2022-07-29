library(tidyverse)
library(sportyR)

library(MLmetrics)

library(patchwork)
library(ggbeeswarm)
library(ggridges)


teams = read_csv("Data/team_info.csv") 

theme_set(theme_bw())

model_pred = read_csv("Data/oaa-model-results.csv") %>%
  mutate(year = as.numeric(substr(game_str,1,4)))

lf_heatmap = geom_baseball(league = "mlb") +
  geom_point(data = model_pred,aes(x = next_event_x, y = next_event_y,color = lf_out_prob),size = 2) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Expected Outs Model, Left Field",
       x = "",
       y = "")

cf_heatmap = geom_baseball(league = "mlb") +
  geom_point(data = model_pred,aes(x = next_event_x, y = next_event_y,color = cf_out_prob),size = 2) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Expected Outs Model, Center Field",
       x = "",
       y = "")

rf_heatmap = geom_baseball(league = "mlb") +
  geom_point(data = model_pred,aes(x = next_event_x, y = next_event_y,color = rf_out_prob),size = 2) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Expected Outs Model, Right Field",
       x = "",
       y = "")

overall_heatmap = geom_baseball(league = "mlb") +
  geom_point(data = model_pred,aes(x = next_event_x, y = next_event_y,color = overall_out_prob),size = 4, alpha = 0.5) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Expected Outs Model, All Fielders",
       x = "",
       y = "")

geom_baseball(league = "mlb") +
  geom_point(data = model_pred,aes(x = next_event_x, y = next_event_y,color = overall_out_prob),size = 4, alpha = 0.4) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Expected Outs Model, All Fielders",
       x = "",
       y = "")

heatmaps = (lf_heatmap + cf_heatmap + rf_heatmap)

geom_baseball(league = "mlb") +
  geom_density_2d_filled(data = model_pred,aes(x = next_event_x, y = next_event_y),alpha = 0.5,show.legend = F) +
  theme_bw() +
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5)) +
  facet_wrap(~hit_type) +
  labs(x = "", y = "")

beeswarm = ggplot(data = model_pred,aes(x = hit_type,y = overall_out_prob)) +
  geom_beeswarm(priority = "random",alpha = 0.5,color = "grey") +
  geom_boxplot(alpha = 0) +
  labs(x = "Hit Type", y = "Scaled Out Probability", title = "Out Probability by Play Type")

ggsave("Plots/overall-heatmap.jpg",overall_heatmap,width = 6, height = 6, units = "in")
ggsave("Plots/heatmaps.jpg",heatmaps,width = 12, height = 4,units = "in")
ggsave("Plots/beeswarm.jpg",beeswarm,width = 12, height = 4,units = "in")


lf_plays = model_pred %>%
  filter(nearest_fielder == "lf") %>%
  mutate(player_id = left_field) %>%
  left_join(teams,by = c("player_id","year" = "team_year"))

cf_plays = model_pred %>%
  filter(nearest_fielder == "cf") %>%
  mutate(player_id = center_field) %>%
  left_join(teams,by = c("player_id","year" = "team_year"))

rf_plays = model_pred %>%
  filter(nearest_fielder == "rf") %>%
  mutate(player_id = right_field) %>%
  left_join(teams,by = c("player_id","year" = "team_year"))

lf_leaders = lf_plays %>%
  group_by(left_field) %>%
  summarise(n = n(),outs = sum(out_lf),xouts = sum(lf_out_prob),oaa = outs - xouts,oaa_per = oaa/n) %>%
  arrange(-oaa_per) %>%
  mutate(position = "lf") %>%
  rename(player_id = left_field)

lf_team_leaders = lf_plays %>%
  group_by(team_id) %>%
  summarise(n = n(),outs = sum(out_lf),xouts = sum(lf_out_prob),oaa = outs - xouts,oaa_per = oaa/n) %>%
  mutate(position = "lf")

cf_leaders = cf_plays %>%
  group_by(center_field) %>%
  summarise(n = n(),outs = sum(out_cf),xouts = sum(cf_out_prob),oaa = outs - xouts,oaa_per = oaa/n) %>%
  arrange(-oaa_per) %>%
  mutate(position = "cf") %>%
  rename(player_id = center_field)

cf_team_leaders = cf_plays %>%
  group_by(team_id) %>%
  summarise(n = n(),outs = sum(out_cf),xouts = sum(cf_out_prob),oaa = outs - xouts,oaa_per = oaa/n) %>%
  mutate(position = "cf") 

rf_leaders = rf_plays %>%
  group_by(right_field) %>%
  summarise(n = n(),outs = sum(out_rf),xouts = sum(rf_out_prob),oaa = outs - xouts,oaa_per = oaa/n) %>%
  arrange(-oaa_per) %>%
  mutate(position = "rf") %>%
  rename(player_id = right_field)

rf_team_leaders = rf_plays %>%
  group_by(team_id) %>%
  summarise(n = n(),outs = sum(out_rf),xouts = sum(rf_out_prob),oaa = outs - xouts,oaa_per = oaa/n) %>%
  mutate(position = "rf") 

overall_leaders = rbind(lf_leaders,cf_leaders,rf_leaders) %>%
  mutate(position = factor(position,levels = c("lf","cf","rf")))

overall_team_leaders = rbind(lf_team_leaders,cf_team_leaders,rf_team_leaders) %>%
  mutate(position = factor(position,levels = c("lf","cf","rf")))

overall_team_filtered = overall_team_leaders %>%
  filter(n > 50)

overall_team_filtered_long = overall_team_leaders %>%
  filter(n > 50) %>%
  pivot_longer(cols = c(outs,xouts),
               names_to = "statistic",
               values_to = "value") 

team_stats_1 = ggplot(overall_team_filtered_long,aes(x = team_id, y = value, fill = statistic)) +
  geom_col(position = "dodge",color = "white") +
  facet_wrap(~position) +
  labs(title = "Outs vs. Expected Outs by Position")

team_stats_2 = ggplot(overall_team_filtered,aes(x = team_id, y = (outs - xouts)/n)) +
  geom_col(aes(fill = team_id)) +
  geom_hline(yintercept = 0) +
  facet_wrap(~position) +
  labs(y = "Outs Above Average per Play", x = "", title = "Outs Above Average per Play by Position")

ggsave("Plots/outs-xouts-team.jpg",team_stats_1,width = 12, height = 4,units = "in")
ggsave("Plots/oaa-pos-team.jpg",team_stats_2,width = 12, height = 4,units = "in")

oaa_per_by_pos = ggplot(overall_leaders,aes(x = oaa_per, y = position, fill = position)) +
  geom_density_ridges(scale = 0.6) +
  labs(x = "OAA Per Play", y = "Position", title = "Outs Above Average per Play by Position") +
  theme(legend.position = "top")

ggsave("Plots/oaa-per-play-position.jpg",oaa_per_by_pos,width = 9, height = 6,units = "in")

oaa_player_leaders = overall_leaders %>%
  mutate(player_pos = paste0(player_id,"-",toupper(position))) %>%
  filter(n >= 15) %>% 
  ggplot(aes(x = reorder(player_pos,-oaa_per), y = oaa_per)) +
  geom_col(aes(fill = position)) +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1),legend.position = "top") +
  labs(x = "", y = "Outs Above Average per Play",
       title = "Outs Above Average per Play - Player Leaderboard",
       subtitle = "Only players with 15+ plays included")

ggsave("Plots/oaa-player-leaders.jpg",oaa_player_leaders,width = 9, height = 6,units = "in")


oaa_per = ggplot(overall_leaders,aes(x = n, y = oaa_per)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  facet_wrap(~position) +
  labs(x = "Plays", y = "OAA Per Play", title = "Outs Above Average per Play by Position & Number of Plays")

ggsave("Plots/oaa-per-play.jpg",oaa_per,width = 12, height = 4,units = "in")

write_csv(overall_leaders,"oaa-players.csv")
write_csv(overall_team_leaders,"oaa-teams.csv")

