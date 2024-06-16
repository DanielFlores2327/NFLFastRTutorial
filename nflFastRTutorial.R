library(tidyverse)
library(nflfastR)
library(ggimage)
library(gt)

pbp <- load_pbp(2020:2021)

pbp %>% head()

pbp %>% select(posteam, defteam, season, down, ydstogo, rush, pass, yards_gained) %>% head()

names(pbp)

nrow(pbp)

pbp_rp <- pbp %>%
  filter(rush == 1 | pass == 1) %>%
  filter(!is.na(epa))

pbp_rp %>%
  filter(posteam == "DAL") %>%
  filter(!is.na(passer_player_name)) %>%
  group_by(passer_player_name) %>%
  summarize(passes = n(),
            avg_epa = mean(epa)) %>%
  filter(passes >= 10) %>%
  arrange(-avg_epa)

offense_20 <- pbp_rp %>%
  filter(season == 2020) %>%
  group_by(posteam) %>%
  summarize(epa_20 = mean(epa))

offense_21 <- pbp_rp %>%
  filter(season == 2021) %>%
  group_by(posteam) %>%
  summarize(epa_21 = mean(epa))
View(offense_21)

offenses_all <- offense_20 %>%
  left_join(offense_21, by = "posteam")

View(teams_colors_logos)

offenses_all <- offenses_all %>%
  left_join(teams_colors_logos, by= c("posteam" = "team_abbr"))

offenses_all %>%
  ggplot(aes(x = epa_20, y = epa_21)) +
  geom_hline(yintercept = mean(offenses_all$epa_21), linetype="dashed") +
  geom_vline(xintercept = mean(offenses_all$epa_20), linetype="dashed") +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  geom_image(aes(image=team_logo_espn), size = 0.05, asp = 16/9) +
  theme_bw() +
  labs(x = "Offensive EPA/Play in 2020",
       y = "Offensive EPA/Play in 2021",
       title = "Offensive EPA/Play in 2020 Compared to 2021",
       caption = "By Daniel Flores | @dflores7273") +
  theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5))

ggsave('off-epa-20-21.png', width = 14, height = 10, dpi = "retina")

pbp_rp <- pbp_rp %>%
  mutate(yards_past_sticks = air_yards - ydstogo)

qb_agg <- pbp_rp %>%
  filter(!is.na(yards_past_sticks)) %>%
  filter(down %in% c(3, 4)) %>%
  group_by(passer_player_name) %>%
  summarize(passes = n(), 
            avg_yps = mean(yards_past_sticks),
            team_abbr = last(posteam)) %>%
  filter(passes >= 70) %>%
  left_join(teams_colors_logos, by = "team_abbr")

qb_agg %>%
  ggplot(aes(x = avg_yps, y = fct_reorder(passer_player_name, avg_yps))) +
  geom_bar(aes(fill = team_color, color = team_color2), stat = "identity", alpha = 0.8) +
  scale_color_identity(aesthetics = c("fill", "color")) + 
  theme_bw() +
  labs(x = "Average Late-Down Yards Past Sticks",
       y = "Passer Name",
       title = "How Aggressive Each Quarterbacks is on Late Downs",
       subtitle = "2019-2020, minimum of 70 passes to qualify") +
  theme(legend.position = "none",
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, hjust = 0.5),
        panel.grid.major.y = element_line(size = 0.1))

qb_gt <- qb_agg %>%
  mutate(avg_yps = round(avg_yps, 2)) %>%
  arrange(-avg_yps) %>%
  mutate(rank = row_number()) %>%
  select(rank, passer_player_name, team_wordmark, avg_yps)