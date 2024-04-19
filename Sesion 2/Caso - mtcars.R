###INSTALACION DE LIBRERIA
library("dplyr")
library("readr")
library("tidyr")
library("beeswarm")
library("ggplot2")
library("readxl")
library("stringr")
library(PerformanceAnalytics)

peso=c(51,59,49,54,50,55,48,53,52,57)
largo =c(33.5,38,32,37.5,31.5,33,31,36.5,34,35)
pairs(largo~peso, col="red")


df = data.frame(peso,largo)
chart.Correlation(df)

# Correlacion de Spearman

shapiro.test(peso)
shapiro.test(largo)

# CASO PRACTICO MTCARS


##Cargar datos
  
mtcars = read_delim("D:/UPEU/07. SEXTIMO CICLO/5. Análisis multivariado/Unidad 01/Sesion 2/mtcars.xlsx", ",",escape_double = FALSE, trim_ws=TRUE)
head(mtcars, 5)

# edad y talla
peso = c(26,18,20,19,25,22,37,56,78)
talla = c(1.56,1.72,1.65,1.44,1.69,1.66,1.51,1.62,1.42)

pairs(peso~talla, col="red")

dfedadtalla = data.frame(peso,talla)
chart.Correlation(dfedadtalla)

###//Correlaciones//

# Ejecutar matriz de correlaciones

M = cor(mtcars)
corrplot(M, method = "ellipse")
corrplot(M, method = "circle")
corrplot(M, method = "square")
corrplot(M, method = "number")
corrplot(M, method = "shade")
corrplot(M, method = "color")
corrplot(M, method = "pie")
