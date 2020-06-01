library(tidyverse)
library(readr)
library(readxl)
library(dslabs)
library(dplyr)
library(gridExtra)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(maptools)

install.packages(c("ggrepel","ggthemes"),lib="C:/Users/Carlos Bravo/Desktop/R/packages")


filename <- "Pedidos.xls"
file<- file.path("C:/Users/Carlos Bravo/Desktop/R/homework-2",filename)

veyi_wide <- read_excel(file)
names(veyi_wide)
head(veyi_wide)
#seleccionamos los campos a trabajar desde el archivo que se baja de VEYI.cl
new_tidy <- veyi_wide %>% select("ID","Estado de Pago","Nombre del Producto","Precio del Producto","Cantidad de Productos","Municipio de envío")
head(new_tidy)
library(stringr)
#alimino el coma cero cero del producto
new_tidy$`Precio del Producto`<- str_replace_all(veyi_wide$`Precio del Producto`,",00","")


#convierto valores caracteres en numericos para campos de precio, ID, cantidad
new_tidy$ID <- as.numeric(new_tidy$ID)
new_tidy$`Precio del Producto` <- as.numeric(new_tidy$`Precio del Producto`)
new_tidy$`Cantidad de Productos` <- as.numeric(new_tidy$`Cantidad de Productos`)

#agrego variable TOTAL como producto del precio y la cantidad
new_tidy<- new_tidy %>% mutate(Total=`Precio del Producto`*`Cantidad de Productos`)
names(new_tidy)
head(new_tidy)

#creo tabla Tickets con ID, valor de la canasta, y n como numero de itemes del pedido
tickets<- new_tidy %>% group_by(ID) %>%
  summarise(precio_canasta=sum(Total),n_productos=n())
tickets
#grafico de ID ticket versus valor de la canasta
grafico_puntos_tickets<-tickets%>% ggplot(aes(x=ID,y=precio_canasta))+
  geom_point(aes(ID,colour=precio_canasta),size=2)+
  geom_line(aes(x=ID,y=mean(precio_canasta)))+
  geom_line(aes(x=ID,y=median(precio_canasta)))+
  ggtitle("Dispersion de Valor Tickets")+
  xlab("Pedido #")+
  ylab("Valor ticket")+
  scale_color_continuous(name="tickets")

grafico_hist<-tickets%>% ggplot()+geom_histogram(aes(precio_canasta))
#creo tabla de pedidos con comuna
comuna<- new_tidy %>% filter(!is.na(`Municipio de envío`)) %>% select(ID,`Municipio de envío`)
comuna
tickets
#junto las tablas por ID
new_wide_comunas <- left_join(tickets,comuna)
new_wide_comunas

grafico_comunas_canastas <- new_wide_comunas %>% ggplot()+
  geom_jitter(aes(`Municipio de envío`, precio_canasta))+
  ggtitle("Tickets por canasta")+
  xlab("Municipio")+
  ylab("Valor ticket")
grafico_comunas_canastas

frec_comuna <- comuna %>% group_by(`Municipio de envío`) %>% count(`Municipio de envío`)
frec_comuna <- frec_comuna %>% mutate(Comuna=`Municipio de envío`)
frec_comuna
grafico_frec_comunas <- frec_comuna %>% ggplot()+geom_point(aes(x=`Municipio de envío`,y=n))
#grid.arrange(grafico_puntos_tickets,grafico_hist,grafico_frec_comunas,ncol=3)
grafico_frec_comunas
grafico_puntos_tickets
grafico_hist

new_wide_comunas
comunassantiago<- st_read("C:/Users/Carlos Bravo/Desktop/R/comunas/comunas.shp", stringsAsFactors=FALSE)
names(comunassantiago)

str(comunassantiago)

str(mapa_y_datos)

head(comunassantiago)
comunassantiago<- comunassantiago %>% filter(Provincia =="Santiago")
comunas_veyi<- left_join(comunassantiago,frec_comuna)
comunas_veyi<- comunas_veyi %>% filter(!is.na(`Municipio de envío`))
ggplot(comunas_veyi)+
  geom_sf(aes(fill=n))+
  scale_fill_gradient(low="#5681f7",high="#123843")

library(tmap)
library(tmaptools)

tm_shape(comunas_veyi)+
  tm_polygons("n",id="Comuna",palette="Greens")

tmap_mode("view")
tmap_last()
test_map<-tmap_last()
tmap_save(test_map,"C:/Users/Carlos Bravo/Desktop/R/comunas/test_map.html")


