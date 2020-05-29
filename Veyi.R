library(tidyverse)
library(readr)
library(readxl)
library(dslabs)
library(dplyr)
library(gridExtra)

install.packages("gridExtra")

filename <- "Pedidos.xls"
file<- file.path("C:/Users/Carlos Bravo/Desktop/R/homework-2",filename)

veyi_wide <- read_excel(file)
names(veyi_wide)
head(veyi_wide)
veyi_tidy_temp <- veyi_wide %>% select("ID","Nombre del Producto","Precio del Producto","Municipio de envío")


head(veyi_tidy_temp)
new_tidy <- veyi_tidy_temp 

names(new_tidy)
head(new_tidy)

new_tidy$ID <- as.numeric(as.character(new_tidy$ID))
new_tidy$`Precio del Producto` <- as.numeric(as.character(new_tidy$`Precio del Producto`), string.asFactors=FALSE)
head(new_tidy)


new_tidy
tickets<- new_tidy %>% group_by(ID) %>%
  summarise(precio_canasta=sum(`Precio del Producto`),n=n())
tickets
mean(tickets$precio_canasta)
grafico_puntos<-tickets%>% ggplot()+geom_point(aes(x=ID,y=precio_canasta,colour=precio_canasta))+geom_line(aes(x=ID,y=mean(precio_canasta)))

grafico_hist<-tickets%>% ggplot()+geom_histogram(aes(precio_canasta))
grid.arrange(grafico_puntos,grafico_hist,ncol=2)






