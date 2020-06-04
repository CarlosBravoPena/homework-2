library(tidyverse)
options(digits = 3)
library(dslabs)
data(brexit_polls)

p <- 0.481    # official proportion voting "Remain"
d <- 2*p-1    # official spread
n <- 1500
exp_val<-n*p
se<- sqrt(n*p*(1-p))
exp_val
se
se_hat<- sqrt((p*(1-p)/n))
se_hat
#question 2
names(brexit_polls)
mean(brexit_polls$spread)
sd(brexit_polls$spread)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread+1)/2)

names(brexit_polls)
mean(brexit_polls$x_hat)
sd(brexit_polls$x_hat)

brexit_polls[1,]
first_poll <- brexit_polls[1,]

se_hat<-  sqrt((first_poll$x_hat*(1-first_poll$x_hat)/first_poll$samplesize))
se_hat
x_hat<- first_poll$x_hat

lower<- x_hat - qnorm(0.975)*se_hat
upper<- x_hat + qnorm(0.975)*se_hat
brexit_polls

june_polls <- brexit_polls %>% filter(enddate>= "2016-06-01")
june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01")
nrow(june_polls)
june_polls <- june_polls %>%
  mutate(se_x_hat = sqrt(x_hat*(1-x_hat)/samplesize),
         se_spread = 2*se_x_hat,
         ci_lower_spread = spread - qnorm(0.975)*se_spread,
         ci_upper_spread = spread + qnorm(0.975)*se_spread)
mean(june_polls$ci_lower_spread < 0 & june_polls$ci_upper_spread > 0)

mean(june_polls$ci_lower_spread > 0)

june_polls <- june_polls %>%
  mutate(hit = (2*p-1 > ci_lower_spread) & (2*p-1 < ci_upper_spread))
mean(june_polls$hit)
head(june_polls)

names(june_polls)
ggplot(june_polls)+geom_boxplot(aes(spread, poll_type))




combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2,
            se_spread = 2*sqrt(p_hat*(1-p_hat)/N),
            spread_lower = spread - qnorm(.975)*se_spread,
            spread_upper = spread + qnorm(.975)*se_spread)
combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(spread_lower)
combined_by_type %>%
  filter(poll_type == "Online") %>%
  pull(spread_upper)


brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)

brexit_chisq <- table(brexit_hit$poll_type, brexit_hit$hit)
chisq.test(brexit_chisq)$p.value
# online > telephone
hit_rate <- brexit_hit %>%
  group_by(poll_type) %>%
  summarize(avg = mean(hit))
hit_rate$avg[hit_rate$poll_type == "Online"] > hit_rate$avg[hit_rate$poll_type == "Telephone"]

# statistically significant
chisq.test(brexit_chisq)$p.value < 0.05
# convert to data frame
chisq_df <- as.data.frame(brexit_chisq)

online_true <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "TRUE"]
online_false <- chisq_df$Freq[chisq_df$Var1 == "Online" & chisq_df$Var2 == "FALSE"]

online_odds <- online_true/online_false
online_odds

phone_true <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "TRUE"]
phone_false <- chisq_df$Freq[chisq_df$Var1 == "Telephone" & chisq_df$Var2 == "FALSE"]

phone_odds <- phone_true/phone_false
phone_odds

online_odds/phone_odds