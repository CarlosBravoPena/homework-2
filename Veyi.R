library(tidyverse)
library(readr)
library(readxl)
library(dslabs)
library(dplyr)
library(gridExtra)



filename <- "Pedidos.xls"
file<- file.path("C:/Users/Carlos Bravo/Desktop/R/homework-2",filename)

veyi_wide <- read_excel(file)
names(veyi_wide)
head(veyi_wide)
veyi_tidy_temp <- veyi_wide %>% select("ID","Estado de Pago","Nombre del Producto","Precio del Producto","Cantidad de Productos","Municipio de envío")


head(veyi_tidy_temp)
new_tidy <- veyi_tidy_temp 

names(new_tidy)
head(new_tidy)

new_tidy$ID <- as.numeric(as.character(new_tidy$ID))
new_tidy$`Precio del Producto` <- as.numeric(as.character(new_tidy$`Precio del Producto`), string.asFactors=FALSE)
new_tidy$`Cantidad de Productos` <- as.numeric(as.character(new_tidy$`Cantidad de Productos`), string.asFactors=FALSE)
head(new_tidy)
new_tidy<- new_tidy %>% mutate(Total=`Precio del Producto`*`Cantidad de Productos`)
names(new_tidy)
head(new_tidy)



new_tidy
tickets<- new_tidy %>% group_by(ID) %>%
  summarise(precio_canasta=sum(Total),n_productos=n())
tickets
mean(tickets$precio_canasta)
mean(tickets$n)
grafico_puntos_tickets<-tickets%>% ggplot()+geom_point(aes(x=ID,y=precio_canasta,colour=precio_canasta))+geom_line(aes(x=ID,y=mean(precio_canasta)))

grafico_hist<-tickets%>% ggplot()+geom_histogram(aes(precio_canasta))


names(new_tidy)
comuna<- new_tidy %>% filter(!is.na(`Municipio de envío`)) %>% select(ID,`Municipio de envío`)

frec_comuna <- comuna %>% group_by(`Municipio de envío`) %>% count(`Municipio de envío`)
frec_comuna
grafico_frec_comunas <- frec_comuna %>% ggplot()+geom_point(aes(x=`Municipio de envío`,y=n))
#grid.arrange(grafico_puntos_tickets,grafico_hist,grafico_frec_comunas,ncol=3)
grafico_frec_comunas
grafico_puntos_tickets
grafico_hist


