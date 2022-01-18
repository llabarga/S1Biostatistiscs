### Práctica 4 Bioestadística: 
## Archivo Mides Teixos
# 1) ANOVA
#H0: No hay diferencias en el diámetro entre las 3 poblaciones
#H1: Existen diferencias en el diámetro entre las 3 poblaciones
summary(Mides_Teixos)
describe(Mides_Teixos)
# Convertimos la variable en una cualitativa, no es obligatorio
Mides_Teixos$Poblacio<-factor(Mides_Teixos$Poblacio)
describe(Mides_Teixos) #ahora te da otra info
Mides_Teixos$Sexe<-factor(Mides_Teixos$Sexe)

#Hacemos el ANOVA: Primero cuantitativa, luego cualitativa
anova_teixos<- aov(Diametre.cm~Poblacio, data = Mides_Teixos)
anova_teixos
summary(anova_teixos)
#Los residuos son las diferencias
# El estadístico F es el mismo valor que el cociente entre 993.7 y 129
# Pr(>F) es el p-valor correspondiente
# Pr<0.05 por lo que podemos rechazar la H0

boxplot(Diametre.cm~Poblacio, data = Mides_Teixos)

## Fichero Pesos Curculio
Pesos_Curculio$Any<-factor(Pesos_Curculio$Any)
Pesos_Curculio$MidaLlavor<-factor(Pesos_Curculio$MidaLlavor)
aov(Pes.mg~Any, data = Pesos_Curculio) #p-value<0.05, rechazamos la H0
summary(aov(Pes.mg~Any, data = Pesos_Curculio))
boxplot(Pesos_Curculio$Pes.mg~Pesos_Curculio$Any, col=c(2:7))

## Fichero Fitofags_raw_data
#Estudia qué animales se comen las hojas
Fitofags_raw_data$Edat<- factor(Fitofags_raw_data$Edat)
Fitofags_raw_data$Herbivor<- factor(Fitofags_raw_data$Herbivor)
summary(Fitofags_raw_data)
#Convertimos los datos en una tabla de contingencia
tabla_freq<-xtabs(~Edat+Herbivor, data= Fitofags_raw_data) #da igual el orden
res2<-chisq.test(tabla_freq) #p-valor<0.05
#Los fitofars que comen un año, no son los mismos que comen en otros años
res2$observed
res2$expected
mosaicplot(tabla_freq,color = c("skyblue", "yellow"))

## Fichero Spider
#Nos preguntamos si las arañas más grandes producen más huevos.
#H0: No hay diferencias entre la medida de la araña y el número de huevos
#H1: Hay diferencias entre la medida de la araña y el número de huevos
#Haremos correlación (intensidad) y regresión (recta)
install.packages("Hmisc")
library(Hmisc)
install.packages("car")
library(car)
#correlacion
rcorr(Spider$MeanClutchSize, Spider$OpistosomaWidth.mm) #da igual el orden
#que p salga 0 es muy pequeño, rechazamos H0
#¿qué valor de r tenemos? 0.52
scatterplot(Spider$OpistosomaWidth.mm, Spider$MeanClutchSize)
#regresió  lineal: nos da la significación de r2, a y b
lm(Spider$MeanClutchSize~Spider$OpistosomaWidth.mm)
summary(lm(Spider$MeanClutchSize~Spider$OpistosomaWidth.mm))
#Multiple R-squared y p-value
#A y B en coeficientes: Intercept(A), Spider(B)
#B=23.991, recahzamos la H0 // A=0, no podemos rechazar H0