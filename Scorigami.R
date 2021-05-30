library(nbastatR)
library(tidyverse)

logall = nbastatR::game_logs(seasons = 1947:2021, result_types = "team", season_types = c("Regular Season", "Playoffs"))

logs = logall %>% 
  group_by(idGame) %>% 
  mutate(score = paste(ptsTeam, collapse = "")) %>% 
  distinct(idGame, .keep_all = T) %>% 
  select(yearSeason, idGame, score) %>% 
  ungroup()

logs %>% 
  distinct(score, .keep_all = T) %>% 
  group_by(yearSeason) %>% 
  add_count() %>% 
  slice(n = n()) %>% 
  ungroup() %>% 
  mutate(uniqueScore = cumsum(n)) %>% 
  ggplot(aes(x = yearSeason, y = uniqueScore)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(1950,2020,10), limits = c(1947,2021)) +
  #scale_y_continuous(breaks = s, limits = c(2,4))
  labs(title = "Cumulative Unique NBA Scores by Season",
       subtitle = "Regular Season and Playoff Scores, 1947-2021",
       caption = "graph: @burakcankoc\nsource: basketball-reference.com",
       x = "Season",
       y = "Number of Unique NBA Scores") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        plot.title = element_text(size=20, face="bold"))
