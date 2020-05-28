library(tidyverse)
library(dslabs)
library(dplyr)

getwd()
setwd()

system.file("extdata",package="dslabs")
list.files("C:/Users/Carlos Bravo/Documents/R/win-library/4.0/dslabs/extdata")

filename <- "murders.csv"
fullpath<- file.path("C:/Users/Carlos Bravo/Documents/R/win-library/4.0/dslabs/extdata",filename)
fullpath

getwd()
file.copy(fullpath,getwd())

read_lines("murders.csv", n_max = 3)
dat <- read_csv(filename)
dat <- read_csv(fullpath)
head(dat)
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path, filename))
dat1=read.csv(file.path(path, filename1))
dat2=read.csv(file.path(path, filename2))

head(dat1)
data(gapminder)
library(readxl)
path <- system.file("extdata", package="dslabs")
filename <- file.path(path,  "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, `1960`:`1967`)
wide_data

