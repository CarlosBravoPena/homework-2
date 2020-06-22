library(dslabs)
library(caret)
library(dplyr)
library(tidyverse)

install.packages(c("caret"),lib="C:/Users/Carlos Bravo/Desktop/R/packages")

data(heights)
y<-heights$sex
x<-heights$height

set.seed(2007)
test_index<- createDataPartition(y, times=1,p=0.5,list=FALSE)
#creamos los set para testeo y entrenamiento
test_set<- heights[test_index,] 
train_set<- heights[-test_index,]
#creamos un codigo para "adivinar" el resultado

y_hat<- sample(c("Male","Female"),length(test_index),replace=TRUE) %>%
  factor(levels=levels(test_set$sex))
mean(y_hat==test_set$sex)

#si revisamos los datos vemos que los hombres son en la media un poco mas altos que las mujeres por lo que podriamos hacelo mejor que adivinar

heights%>% group_by(sex)%>% summarize(mean(height),sd(height))
#probamos con un corte para predecir hombres si son mas altos que 62
y_hat<- ifelse(x>62,"Male","Female")%>%
  factor(levels=levels(test_set$sex))

mean(y==y_hat)

#podemos analizar el desempeño con 10 distintos cortes

cutoff<-seq(61,70)
accuracy<- map_dbl(cutoff,function(x){
  y_hat<-ifelse(train_set$height>x,"Male","Female")%>%
    factor(levels=levels(test_set$sex))
  mean(y_hat==train_set$sex)
  })
plot(cutoff,accuracy)

max(accuracy)

best_cutoff<-cutoff[which.max(accuracy)]
best_cutoff






                                 