library("dplyr")
library("readr")
library("tidyr")
library("beeswarm")
library("ggplot2")
library("readxl")
library("stringr")

###cargar los datos
resultados2014 = read_delim("D:/UPEU/07. SEXTIMO CICLO/5. Análisis multivariado/Unidad 01/Sesion 1/resultados2014.csv", ",",escape_double = FALSE, trim_ws=TRUE)
head(resultados2014, 5)
resultados2018 = read_delim("D:/UPEU/07. SEXTIMO CICLO/5. Análisis multivariado/Unidad 01/Sesion 1/resultados2018.csv", ",",escape_double = FALSE, trim_ws=TRUE)
head(resultados2018, 5)

View(resultados2014)

dim(resultados2014)

str(resultados2014)

summary(resultados2014)

names(resultados2014)

####cambiar nombre
partidos_nombre = c('pase18', 'pac18', 'adc18', 'pt18', 'fa18', 'pin18', 'pln18', 'pml18', 'png18', 'prc18', 'prsc18', 'prn18', 'pusc18')

cam_nombre = function(dataframe)
{
  for (i in 1:length(partidos_nombre))
  {      	names(dataframe)[names(dataframe) == 	paste0('votos', i)] = partidos_nombre[i]
  }
  return(dataframe)
}
resultados2018=cam_nombre(resultados2018)

head(resultados2018, 5)
names(resultados2018)

###calcular porcentaje

votos_porcentaje= function(dataframe){
  x=dataframe%>%
    group_by(codigo)%>%
    mutate_all(funs((. / votos_validos)*100))%>%
    select(-votos_validos)
  return(x)
}

por_resultados2014 = votos_porcentaje(resultados2014)
por_resultados2018 = votos_porcentaje(resultados2018)

###partido ganador

winner = function(dataframe, periodo){
  x = dataframe%>%
    gather(partido, votos, -codigo) %>%
    group_by(codigo)%>%
    filter(votos==max(votos))%>%
    separate(partido, c(paste0("partido", periodo)), sep="1")%>%
    select(-votos)
  return(x)
}

winner2014=winner(por_resultados2014, 14)
winner2018 =winner(por_resultados2018, 18)

table(winner2014$partido14)
table(winner2018$partido18) 

##CAMBIO

cambio = winner2018%>%
  left_join(winner2014, by="codigo")%>%
  mutate(cambio=ifelse(partido18==partido14,"sin cambio", "cambio"),
         robo=ifelse(cambio=="cambio", paste(partido18, partido14, sep=" al "), "sin cambio"))

table(cambio$cambio)
table(cambio$robo)

#### ¿Cómo varió el porcentaje de votos obtenidos por el Partido Liberación Nacional (PLN), Partido Unidad 
### Social Cristiana (PUSC), Partido Acción Ciudadana  (PAC), Partido Restauración Nacional (PRN) y
### Partido Integración Nacional (PIN)?
  
  
grafico_votos = function(partido, color){
  x = por_resultados2018%>%
    select(codigo, paste0(partido,18))%>%
    left_join(
      (por_resultados2014%>%
         select(codigo, paste0(partido,14))),
      by="codigo")%>%
    gather(anio, votos, - codigo)%>%
    mutate(anio=ifelse(anio==paste0(partido,14), 2014, 2018))
  
  par(las=1, bty="l", family="mono", font=1, bg="transparent")
  
  return( beeswarm(votos ~ anio, data=x, col=color, pch=16, method="hex", 
             cex=0.8, horizontal=TRUE, ylab="", xlab=paste("Porcentaje de votos del", toupper(partido)), 
             main=paste("Porcentaje de votos del", toupper(partido)), xlim=c(0, 60))
  )
}

###PORCENTAJE DE VOTOS DEL PAC 

grafico_votos("pac", "black")

###PARTIDO POLITICO - GANADORES DE LOS CANTONES 

cantones= resultados2018%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)
sum(cantones$votos_validos)/sum(resultados2018$votos_validos)*100

por_resultados2018%>%
  filter(codigo==101 | codigo==103 |codigo==119|codigo==201|codigo==210|codigo==301)%>%
  gather(partido, votos, -codigo)%>%
  group_by(codigo)%>%
  filter(votos==max(votos))
