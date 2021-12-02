# Segunda práctica sobre Estadístca Descriptiva
# &&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&&

# 1) Descriptiva de variables categóricas
table(Fulles$Grup) # Tabla resumen. MIra el número de observaciones de un grupo.
#Hace frecuencias

table(Fulles$Grup)
table(Fulles$Especie)
d<- table(Fulles$Especie)
# Guardamos tabla en una variable d
d

## Barplots o diagrama de barras
barplot(table(Fulles$Especie))
barplot(d)
barplot(table(Fulles$Especie), xlab="Especie", ylab = "Frecuencia")
barplot(table(Fulles$Especie)/length(Fulles$Especie), xlab="Especie", ylab = "Frecuencia")
# Divides la frecuencia de cada especie entre la frecuencia total 
# = Frecuencia relativa

## Diagrama de quesitos
pie(table(Fulles$Grup), main="GRUP")
pie(table(Fulles$Grup), main="GRUP", col= c("lightpink", "lightblue", "Orchid"))
boxplot(table(Fulles$Llargada.mm))
stem(Fulles$Llargada.mm)

## Valores de estadística descriptiva
mean(Fulles$Llargada.mm)
median(Fulles$Llargada.mm)

quantile(Fulles$Llargada.mm)
quantile(Fulles$Llargada.mm, probs = seq(0,1,0.1)) #probabilidad de frecuencia acumulada

var(Fulles$Llargada.mm)
sqrt(var(Fulles$Llargada.mm))
sd(Fulles$Llargada.mm)

describe(Fulles$Llargada.mm)
describeBy(Fulles$Llargada.mm, Fulles$Especie)
# Te explica llargada, en función de la especie

IQR(Fulles$Llargada.mm)

## Graficación variables cuantitativas (histogramas, bigotes)
hist((Fulles$Llargada.mm),col=c("lightblue"))

Hist_fulles<-hist(Fulles$Llargada.mm)
Hist_fulles

3/659 # 3 es el número de observaciones en el priemr intervalo. 
#Divide entre 659 para sacar la frecuencia 
3/659/10
# y entre 10, para hallar la densidad.

hist((Fulles$Llargada.mm),col=c(1:3,"maroon3", "blue"))
hist((Fulles$Llargada.mm),col=c("maroon3"), breaks = c(seq(0,100,1)))

##Diagrama de cajas y bigotes
boxplot(Fulles$Llargada.mm, col = c("pink"))
boxplot(Fulles$Llargada.mm~Fulles$Especie, col = c("pink","lightblue"))
     