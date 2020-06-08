library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()

names(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) + 
  geom_point(alpha = 0.5)

?Teams

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R / G, AB_per_game = AB / G) %>%
  ggplot(aes(R_per_game, AB_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  ggplot(aes(W_per_game, E_per_game)) + 
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  ggplot(aes(X3B_per_game, X2B_per_game)) + 
  geom_point(alpha = 0.5)

R <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R / G, AB_per_game = AB / G) %>%
 
  summarize(r = cor(R_per_game, AB_per_game))
R



R <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game = W / G, E_per_game = E / G) %>%
  
  summarize(r = cor(W_per_game, E_per_game))
R

R <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(X3B_per_game = X3B / G, X2B_per_game = X2B / G) %>%
  
  summarize(r = cor(X2B_per_game, X3B_per_game))
R