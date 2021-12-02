## Práctica 3 Bioestadística: Inferencia estadística

#creamos un subconjunto únicamente con datos de Quercus:
Fulles_Quercus<-subset(Fulles, Fulles$Especie=="Quercus")
# Estadística descriptiva

mean(Fulles_Quercus$Llargada.mm)
sd(Fulles_Quercus$Llargada.mm)
length(Fulles_Quercus$Llargada.mm)
sd(Fulles_Quercus$Llargada.mm)/sqrt(length(Fulles_Quercus$Llargada.mm))

# Crearemos una nueva función para calcular el error estándar
SE<-function(x) sd(x)/sqrt(length(x))
SE(Fulles_Quercus$Llargada.mm)

# Hagamos algún gráfico
boxplot(Fulles_Quercus$Llargada.mm, xlab= "Llargada fulles Quercus", col=15)
media<-mean(Fulles_Quercus$Llargada.mm)
rm(mitjana)
points(media, pch=18, col="red", cex=1)

hist(Fulles_Quercus$Llargada.mm, xlab= "Mides Fulles Quercus", col = 15)
abline(v=media, col="black", lwd=2)
abline(v=40.5, col="red", lwd=2)

## InFeReNcIa para una media
t.test(Fulles_Quercus$Llargada.mm, mu=40.5)
# Mirando el intervalo, podemos decir que es una posibilidad, en nuestra muestra. Está dentro del intervalo.
# Los de Madrid se han equivocado, la media era 40.5
t.test(Fulles_Quercus$Llargada.mm, mu=40.1)
# El intervalo no cambia. 40.1 queda un poco al extremo.
t.test(Fulles_Quercus$Llargada.mm, mu=39.8)
t.test(Fulles_Quercus$Llargada.mm, mu=40.1, alternative= "greater")
# Es de una cola, solo nos interesa un lado del intervalo. greater/less

## Inferencia sobre 2 medias de 2 poblaciones: Bellaterra y Madrid
boxplot(Fulles$Llargada.mm~Fulles$Especie)
t.test(Fulles$Llargada.mm~Fulles$Especie)
bartlett.test(Fulles$Llargada.mm, Fulles$Especie)
#Variable respuesta , categórica
# Rechazo la hipótesis nula
t.test(Fulles$Llargada.mm~Fulles$Especie, var.equal=FALSE)
#Mirando las medias de cada uno, ya podemos saber cuál es más grande que la otra.
t.test(Fulles$Llargada.mm~Fulles$Especie, var.equal=TRUE)

##Siguiente base de datos: Precipitación
# Son datos dependientes
boxplot(Precipitacio_Fageda$ForaBosc.mm, Precipitacio_Fageda$DinsBosc.mm, paired = TRUE)
t.test(Precipitacio_Fageda$ForaBosc.mm, Precipitacio_Fageda$DinsBosc.mm, paired = TRUE)
# La diferencia nos dice que llueve más fuera
#Rechazamos la hipótesis nula, con una probabilidad de error del 0,4%

## Creamos un test de 1 población
Precipitacio_Fageda$difer<- Precipitacio_Fageda$ForaBosc.mm- Precipitacio_Fageda$DinsBosc.mm
t.test(Precipitacio_Fageda$difer, mu=0)
describeBy(Precipitacio_Fageda)
