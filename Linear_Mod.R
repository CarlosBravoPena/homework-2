# find regression line for predicting runs from BBs
library(tidyverse)
library(Lahman)
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>% 
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))



# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR/G, 1), 
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata >= 0.4 & HR_strata <=1.2)

# scatterplot for each HR stratum
dat %>% 
  ggplot(aes(BB_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%  
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 


 lm(son ~ father, data = galton_heights)


  lm(formula = son ~ father, data = galton_heights)
  
  galton_heights <- galton_heights %>%
    mutate(father_centered=father - mean(father))
  
  lm(son ~ father_centered, data = galton_heights)
  
  lm(formula = son ~ father_centered, data = galton_heights)
  
  # compute RSS for any pair of beta0 and beta1 in Galton's data
  library(HistData)
  library(dplyr)
  library(ggplot2)
  data("GaltonFamilies")
  set.seed(1983)
  galton_heights <- GaltonFamilies %>%
    filter(gender == "male") %>%
    group_by(family) %>%
    sample_n(1) %>%
    ungroup() %>%
    select(father, childHeight) %>%
    rename(son = childHeight)
  rss <- function(beta0, beta1, data){
    resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
    return(sum(resid^2))
  }
  
  # plot RSS as a function of beta1 when beta0=25
  beta1 = seq(0, 1, len=nrow(galton_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 25))
  results %>% ggplot(aes(beta1, rss)) +  
    geom_line(aes(beta1, rss))
  
  # fit regression line to predict son's height from father's height
  fit <- lm(son ~ father, data = galton_heights)
  fit
  
  # summary statistics
  summary(fit)
  
  # Monte Carlo simulation
  B <- 1000
  N <- 50
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>% 
      lm(son ~ father, data = .) %>% 
      .$coef 
  })
  lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
  
  # Plot the distribution of beta_0 and beta_1
  library(gridExtra)
  p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
  p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
  grid.arrange(p1, p2, ncol = 2)
  
  # summary statistics
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    summary %>%
    .$coef
  
  lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
  
  # Monte Carlo simulation
  B <- 1000
  N <- 50
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>% 
      lm(son ~ father, data = .) %>% 
      .$coef 
  })
  lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
  
  # Plot the distribution of beta_0 and beta_1
  library(gridExtra)
  p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
  p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
  grid.arrange(p1, p2, ncol = 2)
  
  # summary statistics
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    summary %>%
    .$coef
  
  lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))
  
  
  lse %>% summarize(cor(beta_0, beta_1))
  
  B <- 1000
  N <- 50
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
      mutate(father = father - mean(father)) %>%
      lm(son ~ father, data = .) %>% .$coef 
  })
  
  cor(lse[1,], lse[2,])
  
  
  # plot predictions and confidence intervals
  library(dplyr)
  library(ggplot2)
  
  galton_heights %>% ggplot(aes(son, father)) +
    geom_point() +
    geom_smooth(method = "lm")
  
  # predict Y directly
  fit <- galton_heights %>% lm(son ~ father, data = .) 
  Y_hat <- predict(fit, se.fit = TRUE)
  names(Y_hat)
  
  # plot best fit line
  galton_heights %>%
    mutate(Y_hat = predict(lm(son ~ father, data=.))) %>%
    ggplot(aes(father, Y_hat))+
    geom_line()
  
  
  #ejercicio pregunta 1
  beta1 = seq(0, 1, len=nrow(galton_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 36))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss), col=2)
  
  #pregunta 4 ejercicio
  B <- 1000
  N <- 100
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>% 
      lm(son ~ father, data = .) %>% .$coef 
  })
  
  lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse



set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)


sample_n(female_heights, N, replace = TRUE) %>% 
  lm(daughter ~ mother, data = .) %>% 
  summary %>%
  .$coef

fit$coef[1]

female_heights$mother[1]


library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
#pregunta del examen

library(Lahman)
bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  group_by(playerID) %>%
  summarize(mean_singles = mean(singles), mean_bb = mean(bb))
sum(bat_99_01$mean_singles > 0.2)
#pregunta del examen

sum(bat_99_01$mean_bb > 0.2)
set.seed(1)

new<-inner_join(bat_02,bat_99_01)
cor(new$singles,new$mean_singles)
new<-inner_join(bat_02,bat_99_01)
cor(new$BB,new$mean_bb)

fit_singles <- lm(singles ~ mean_singles, data = new)
fit_singles$coef[2]
fit_bb <- lm(bb ~ mean_bb, data = new)
fit_bb$coef[2]

