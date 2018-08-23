# Datos de usuarios - Abril de 2018
rm(list=ls(all=TRUE))
  ## Librerias
#if(!require(devtools)) install.packages("devtools")
#devtools::install_github("kassambara/factoextra")                      #https://github.com/kassambara/factoextra
library(factor)
library(dplyr)

  ## Cargar los datos
url_users<- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQcdCrucn0E0VD7MS0Ca-wbMcNQK81ilDtaP9yf5r05yNtMGQtlrAQ4O1t7saZDC1nNUei6h9EXfFRy/pub?gid=1843742492&single=true&output=csv"
data_users<- read.csv(url(url_users), encoding = "UTF-8")
attach(data_users)

    ## Descripción general 
summary(data_users)

## Mirar las variables de interes por si hay algo raro
  for (var in unique(data_users)) {
print(summary(var, data=data_users))
  }

## Arreglar variables utiles
  # Crear Edad
data_users$Edad<-2018-as.numeric(substring(Fecha.Nacimiento,1,4))

## Prueba de outliers
boxplot(Número.de.Préstamos)
#cut<-quantile(Número.de.Préstamos, probs = 0.95) 
Número.de.Préstamos[Número.de.Préstamos>=cut]<- NA
boxplot(Número.de.Préstamos)

  ## Modelación
    # Subset de variables utiles
sample_users = na.omit(subset(data_users, select = c(Naturaleza, Edad, codbiblioteca, Barrio, Número.de.Préstamos)))
  ## Definir el número de clusters
    # Valores iniciales
errores_base<-0 
errores_all<-0 
rep<-20

    # Crear el vector de errores de cada modelo

for (i in 1:rep) {
  model_base <- kmeans(c(sample_users$Número.de.Préstamos), centers = i, nstart=20)
  errores_base[i] <- model_base$tot.withinss
  
  model_all <- kmeans(c(sample_users$Naturaleza, sample_users$Edad, sample_users$codbiblioteca, sample_users$Barrio, sample_users$Número.de.Préstamos), centers = i, nstart=20)
  errores_all[i] <- model_all$tot.withinss
  
}
  # Graficar el vector (Se deja la k donde cambia la concavidad de la función)
    #Base (K=5)
plot(1:rep, errores_base, type = "b", 
     xlab = "Clusters", 
     ylab = "Grupos")
    #All  (K=3)
plot(1:rep, errores_all, type = "b", 
     xlab = "Clusters", 
     ylab = "Grupos")


