##POR MATRIZ DE CORRELACION
round(cor(x = bookstore, method = "pearson"), 3)

##PASOS A SEGUIR
##1. IDENTIFICAR COLINEALIDAD --- POR GRAFICA 1
library(psych) 
multi.hist(x = bookstore, dcol = c("purple", "red"),
           dlty = c("dotted", "solid"), main = "")

##GRAFICO NUMERO 2
library(GGally) 
ggpairs(bookstore, lower = list(continuous =
                                                 "smooth"), diag = list(continuous = "barDiag"), axisLabels =
                          "none")

##2. ELEGIR MEJOR PREDICTOR-- identificar mejores predictores
modelox = lm(bookstore$price ~ bookstore$pages+bookstore$reviews+bookstore$n_reviews
             +bookstore$star5+bookstore$star4+bookstore$star3+bookstore$star2+bookstore$star1)

step(object = modelox, direction = "both", trace=1)

library(car)

vif(modelox)

######### Gráfica en 3D (3 variable 1y y 2x)
library(rgl)

plot3d(bookstore$price , bookstore$pages, bookstore$reviews, pch = ".", size = 0.5)
