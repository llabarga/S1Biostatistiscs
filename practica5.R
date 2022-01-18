### Práctica 5
summary(Pp_Bolivia)
Pp_Bolivia$Habitat<-factor(Pp_Bolivia$Habitat)
Pp_Bolivia
describeBy(Pp_Bolivia~Habitat)
# ¿Llueve igual en los diferentes habitats?
# Hacemos un anova
summary(aov(Rainfall.mm~Habitat, data = Pp_Bolivia))
aov(Pp_Bolivia$Rainfall.mm~Pp_Bolivia$Habitat)

## Fichero Depredación
# ¿La depr total depende de la cobertura?
#H0: No hay relación
#H1: Existe relación
library(Hmisc)
library(car)
#Correlación
rcorr(Depredacio$CoberturaTotal,Depredacio$DepTotal)
# P es 0.0641, mayor al 0.05, por lo que no podemos rechazar la hipótesis nula.
# Pone que r vale 0.32, pero como aceptamos la nula, r=0
scatterplot(Depredacio$CoberturaTotal,Depredacio$DepTotal)
#Regresión
lm(Depredacio$CoberturaTotal~Depredacio$DepTotal)
summary(lm(Depredacio$CoberturaTotal~Depredacio$DepTotal))

# ¿Hay diferencia de depredación entre los diferentes animales?
#Creamos las 2 columnas que necesitemos, que no nos limite!
aov(Depredacio$Dep~Depredacio$Animal)
summary(aov(Depredacio$Dep~Depredacio$Animal))
boxplot(Depredacio$Dep~Depredacio$Animal)
# p value< 0.05 Rechazamos H0

##Fichero Supervivencia
#¿La mortalidad depende de la mida?
#H0: No hay relación entre la mida y la mortalidad
#H1: Sí hay relación entre la mida y la mortalidad
Supervivencia$CatSize<- factor(Supervivencia$CatSize)
Supervivencia$Supervivencia<-factor(Supervivencia$Supervivencia)
tabla_freq<-xtabs(~CatSize+Supervivencia, data= Supervivencia) 
tabla_freq
res2<-chisq.test(tabla_freq) 
res2  #P value < 0.05, podemos rechazar la H0 y por tanti
library(ggplot2)
mosaicplot(tabla_freq, color = 4:5)

## Fichero Abundacia_arbres
#¿los árboles se distribuyen por igual en las 3 zonas de la montaña?
# 2 variables cualitativas -> chi cuadrado
# En otras prácticas, teníamos raw data, cada fila era un individuo
# En esta, los datos ya están agrupados
tabla1<-xtabs(Numero~Zona+Especie, data= Abundancia_arbres)
tabla1
# Numero~Zona + Especie -> le estamos diciendo que la relación entre Zona y Especie está guardada en Numero
#Hacemos la chi cuadrado
# H0: Se distribuyen igual por las zonas
# H1: No se distribuyen igual por las zonas
chisq.test(tabla1) #pvalue< 0.05, rechazamos H0
mosaicplot(tabla1, color = 14:5)

##Fichero Amazon_drought
#¿La precipitación afecta al número de incendios?
rcorr(Amazon_drought$Precipitacio,Amazon_drought$Hotspots_foc)
rcorr(Amazon_drought$Hotspots_foc~Amazon_drought$Precipitacio)
#pvalue< 0.05, rechazamos hipótesis nula, r=-0.38
scatterplot(Amazon_drought$Precipitacio,Amazon_drought$Hotspots_foc, col = "purple")
scatterplot(Amazon_drought$Hotspots_foc~Amazon_drought$Precipitacio, col = "purple")

lm(Amazon_drought$Precipitacio~Amazon_drought$Hotspots_foc)
summary(lm(Amazon_drought$Precipitacio~Amazon_drought$Hotspots_foc))

lm(Amazon_drought$Hotspots_foc~Amazon_drought$Precipitacio)
summary(lm(Amazon_drought$Hotspots_foc~Amazon_drought$Precipitacio))
#Ahora que sabemos que hay una relación, miramos la significancia
# Viendo el gráfico vemos que a medida que disminuye la precipitación, aumentan los hotspots

#¿La deforestación afecta al número de incendios?
rcorr(Amazon_drought$Hotspots_foc,Amazon_drought$per_Desforestacio)
