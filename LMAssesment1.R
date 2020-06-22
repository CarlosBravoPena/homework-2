library(tidyverse)
library(dplyr)
library(ggplot2)
library(dslabs)
#ejercicio de los accesos a financiamiento en holanda
data("research_funding_rates")
research_funding_rates

new_table_applications<- research_funding_rates %>%
  select(applications_men,applications_women,awards_men,awards_women)

names(new_table_applications)

new_table_applications<-new_table_applications%>%
  mutate(not_aw_men=applications_men-awards_men,not_aw_women=applications_women-awards_women)%>%select(awards_men,awards_women,not_aw_men,not_aw_women)

new_table_applications

app_men<- sum(new_table_applications$applications_men)
app_women<- sum(new_table_applications$applications_women)

aw_men<- sum(new_table_applications$awards_men)
aw_women<- sum(new_table_applications$awards_women)

not_aw_men<- app_men-aw_men
not_aw_women<- app_women-aw_women

app_men
app_women
aw_men
aw_women
not_aw_men
not_aw_women

men<- c(aw_men, not_aw_men)
women<- c(aw_women,not_aw_women)
table<- data.frame("awarded","not_awarded")
table["men",]<-men
table["women",]<- women
table

#respuesta pregunta 1 

two_by_two <- research_funding_rates %>% 
  select(-discipline) %>% 
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men, 
            no_men = applications_men - awards_men, 
            yes_women = awards_women, 
            no_women = applications_women - awards_women) %>%
  gather %>%
  separate(key, c("awarded", "gender")) %>%
  spread(gender, value)
two_by_two

#respuesta pregunta 2a
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(men)

#respuesta pregunta 2b
two_by_two %>% 
  mutate(men = round(men/sum(men)*100, 1), women = round(women/sum(women)*100, 1)) %>%
  filter(awarded == "yes") %>%
  pull(women)

# respuesta pregunta 3

  two_by_two%>%select(-awarded)%>%do(tidy(chisq.test(.)))
  
  #pregunta 4
  dat <- research_funding_rates %>% 
    mutate(discipline = reorder(discipline, success_rates_total)) %>%
    rename(success_total = success_rates_total,
           success_men = success_rates_men,
           success_women = success_rates_women) %>%
    gather(key, value, -discipline) %>%
    separate(key, c("type", "gender")) %>%
    spread(type, value) %>%
    filter(gender != "total")
  dat
  
  
  dat %>% 
    ggplot(aes(discipline, success, size = applications, color = gender)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    geom_point()
  
 
  