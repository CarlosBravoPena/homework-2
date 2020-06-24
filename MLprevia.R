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

#ahora podemos probar este cutoff en el grupo de test y ver si esta funcionando bien

y_hat<- ifelse(test_set$height>best_cutoff,"Male","Female")%>%
  factor(levels=levels(test_set$sex))
y_hat<-factor(y_hat)
mean(y_hat==test_set$sex)

#confusion matrix

table(predicted=y_hat,actual=test_set$sex)
#calculamos la accuracy por sexo

test_set%>%
  mutate(y_hat=y_hat)%>%
  group_by(sex)%>%
  summarize(accuracy=mean(y_hat==sex))

#matriz de confusion con caret package

cm<- confusionMatrix(data=y_hat,reference = test_set$sex)

cm

F_1<- map_dbl(cutoff,function(x){
  y_hat<-ifelse(train_set$height>x,"Male","Female")%>%
    factor(levels=levels(test_set$sex))
  F_meas(data=y_hat, reference=factor(train_set$sex))
  
})
plot(cutoff,F_1)
max(F_1)
best_cutoff<-cutoff[which.max(F_1)]
best_cutoff
y_hat<- ifelse(test_set$height>best_cutoff,"Male","Female")%>%
  factor(levels=levels(test_set$sex))

sensitivity(data=y_hat,reference=test_set$sex)
specificity(data=y_hat, reference=test_set$sex)

#ROC cuando comparamos el metodo de cutoff con adivinar
p<-0.9

probs<- seq(0,1,length.out = 10)
guessing<- map_df(probs,function(x){
  y_hat<-
    sample(c("Male","Female"),n,replace=TRUE,prob=c(p,1-p))%>%
    factor(levels=c("Female","Male"))
  list(method="Guessing",
       FPR=1-specificity(y_hat,test_set$sex),
       TPR=sensitivity(y_hat,test_set$sex))

})
plot(guessing$FPR,guessing$TPR)




                                 