# Ejercicio 1: ¿diferencia de manzanas entre comarcas?
aov(Manzanas~Especie, data= manzanas_2_)
summary(aov(Manzanas~Especie, data= manzanas_2_))
#hacemos summary para que te de los valores que necesitas para el análisis
#p value= 0.0864 > 0.05 (alfa que establezco yo) 
# por tanto, no podemos rechazar la H0
# Conclusión: No hay suficientes pruebas estadísticas para decir que 
# existen diferencias de producción de manzanas entre las comarcas


#Ejercicio 2: ¿Dónde viven las mariposas?
tabla_mariposas<-xtabs(~Especie+Habitat, data= mariposas)
#hacemos una tabla de contingencia de los datos
chisq.test(tabla_mariposas)
#p value= 0.0004322 < 0.05 (alfa que establezco yo)
#por tanto, podemos rechazar H0
#Conclusión: Existe relación entre las variables y podemos afirmar
#que las mariposas no se distribuyen por igual

#Ejercicio 3: ¿Dónde viven los pájaros?
aov(individuos~zonas, data=pajaros)
summary(aov(individuos~zonas, data=pajaros))
#p value=0.071 > 0.05 (alfa que establezco yo)
#por tanto, no podemos rechazar la H0
#Conclusión: Los pájaros se distribuyen de manera igual entre las zonas

#Ejercicio 4
library(Hmisc)
library(car)
rcorr(arbolesPrecipitacion$`Crecimiento (mm)`, arbolesPrecipitacion$`Precitipacion (mm)`)
#p value=0.0274 < 0.05 --> R= 0.66
scatterplot(arbolesPrecipitacion$`Precitipacion (mm)`, arbolesPrecipitacion$`Crecimiento (mm)`, smooth=F)
#para ver la relación
lm(arbolesPrecipitacion$`Crecimiento (mm)` ~arbolesPrecipitacion$`Precitipacion (mm)`)
summary(lm(arbolesPrecipitacion$`Crecimiento (mm)` ~arbolesPrecipitacion$`Precitipacion (mm)`))
# A (intercept) p value=0.9352 > 0.05 --> A=0
# B p value=0.0274 < 0.05 --> B=0.0016486
# R2 p value= 0.02735 < 0.05 --> R2=0.03718
        