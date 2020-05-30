library(tidyverse)
library(dslabs)
library(dplyr)

path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
wide_data


data("gapminder")
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
tidy_data

new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

new_tidy_data %>%
  ggplot(aes(year, fertility, color = country)) +
  geom_point()

path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")
raw_dat <- read_csv(filename)

co2
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>% 
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))

select(raw_dat, 1:5)

head(co2_tidy)
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()


library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)
tmp <- gather(admissions, key, value, admitted:applicants)
tmp

tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016, c(1:3, 5, 7:8)) %>% select(state, electoral_votes)
tab2


install.packages(c("Lahman"),lib="C:/Users/Carlos Bravo/Desktop/R/packages")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
