library(tidyverse)
library(readr)
library(readxl)


filename <- "Pedidos.xls"
file<- file.path("C:/Users/Carlos Bravo/Desktop/R/homework-2",filename)

veyi_wide <- read_excel(file)
names(veyi_wide)
head(veyi_wide)
veyi_tidy_temp <- veyi_wide %>% select("ID","Nombre del Producto","Precio del Producto")


head(veyi_tidy_temp)
new_tidy <- veyi_tidy_temp 


names(new_tidy)
head(new_tidy)

new_tidy$ID <- as.numeric(as.character(new_tidy$ID))
new_tidy$`Precio del Producto` <- as.numeric(as.character(new_tidy$`Precio del Producto`), string.asFactors=FALSE)

                                             
head(new_tidy)

