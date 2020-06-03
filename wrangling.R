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


install.packages(c("gutenbergr"),lib="C:/Users/Carlos Bravo/Desktop/R/packages")
library(Lahman)
top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
names(top)
Master %>% as_tibble()
top_names <- top %>% left_join(Master) %>%
  select(playerID, nameFirst, nameLast, HR)
top_names

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
names(AwardsPlayers)
tabla <- AwardsPlayers %>% filter(yearID==2016)

library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)
nodes <- html_nodes(h, "table")
nodes
html_text(nodes[[4]])
html_table(nodes[[4]])
sapply(nodes[1:4], html_table=TRUE)

html_table(nodes[[length(nodes)-2]])
html_table(nodes[[length(nodes)-1]])
html_table(nodes[[length(nodes)]])

h10<- html_table(nodes[10])
h19<- html_table(nodes[19])
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
table<- read_html(url)
table
tab<- html_nodes(table, "table")
length(html_table(tab[[5]], fill=TRUE))


tab[[5]] %>% html_table(fill = TRUE) %>% names()
maptools::readShapeSpatial("C:/Users/Carlos Bravo/Desktop/R/comunas.shp")
library(sf)

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)


data(brexit_polls)
head(brexit_polls)
names(brexit_polls)
brexit_polls<- brexit_polls%>% mutate(mes=month(brexit_polls$startdate))
brexit_polls
sum(brexit_polls$mes==4)



sum(round_date(brexit_polls$enddate, unit = "week") == "2016-06-12")

brexit_polls<- brexit_polls%>% mutate(dia=weekdays(brexit_polls$enddate))
sum(brexit_polls$dia=="lunes")
sum(brexit_polls$dia=="martes")
sum(brexit_polls$dia=="miércoles")
sum(brexit_polls$dia=="jueves")
sum(brexit_polls$dia=="viernes")
sum(brexit_polls$dia=="sábado")
sum(brexit_polls$dia=="domingo")
  
table(weekdays(brexit_polls$enddate))

data(movielens)
names(movielens)
movielens

movielens<- movielens%>% mutate(fecha=as_datetime(timestamp),hora=hour(fecha))
table(movielens$hora)

library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits = 3)
gutenberg_metadata
gutenberg_works(str_detect(title, "Pride and Prejudice"))


text<- gutenberg_download(1342)
text
words<- text%>%unnest_tokens(word,text)
stop_words
no_stop
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)
words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()

book <- gutenberg_download(1342)
words <- book %>%
  unnest_tokens(word, text)
nrow(words)
words <- words %>% anti_join(stop_words)
nrow(words)
words <- words %>%
  filter(!str_detect(word, "\\d"))
nrow(words)

words %>%
  count(word) %>%
  filter(n > 100) %>%
  nrow()

words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(word)

words %>%
  count(word) %>%
  top_n(1, n) %>%
  pull(n)

afinn <- get_sentiments("afinn")
afinn_sentiments<- inner_join(words, afinn)
afinn_sentiments
sum(afinn_sentiments$value==4)