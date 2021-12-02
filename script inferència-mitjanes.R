#creem un fitxer nom?s amb dades de "Quercus":

Fulles_Quercus<-subset(Fulles, Fulles$Especie=="Quercus")
describeBy(Fulles_Quercus)

#Una mica d'estadistica descriptiva

mean(Fulles_Quercus$Llargada.mm)
sd(Fulles_Quercus$Llargada.mm)
length(Fulles_Quercus$Llargada.mm)


#creem una funci? que calcular? l'error est?ndard

sd(Fulles_Quercus$Llargada.mm) /sqrt(length(Fulles_Quercus$Llargada.mm))


mean(Fulles_Quercus$Llargada.mm)
41.15+2*0.663
41.15+2*0.663

#El 95% de les mitjanes poblacionals es troben en aquest interval
#com m?s gran sigui la mostra , m?s petit ser? el nostre interval de confian?a

#creem una funci? que calcular? l'error est?ndard

SE<-function(x) sd(x)/sqrt(length(x))
SE(Fulles_Quercus$Llargada.mm)


#Fem algun gr?fic:
boxplot(Fulles_Quercus$Llargada.mm, xlab="LLargada Fulles Quercus", col=15)
mitjana<-mean(Fulles_Quercus$Llargada.mm)
points(mitjana, pch=18, col="red", cex=1)

#Fem un histograma:

hist(Fulles_Quercus$Llargada.mm, xlab="LLargada Fulles", col=15)
abline(v=mitjana, col="blue", lwd=2)
abline(v=40.5, col="red", lwd=2)

#contrast d'hipotesi per una mitjana:

t.test(Fulles_Quercus$Llargada.mm, mu=40.5)
#cau dins l'interval de confian?a, assumim Ho

t.test(Fulles_Quercus$Llargada.mm, mu=39.7)
#t-student passa de 2, fora interval de confian?a , assumim Ha 

t.test(Fulles_Quercus$Llargada.mm, mu=39.9)
#no acceptem Ha amb un npossibilitat d'error del 5,9%

t.test(Fulles_Quercus$Llargada.mm, mu=40.5, conf.level = 0.9)


#suposem a priori(abans de prendre la mostra)(p-value molt alt)
t.test(Fulles_Quercus$Llargada.mm, mu=40.5, alternative = "greater")
t.test(Fulles_Quercus$Llargada.mm, mu=40.5, alternative = "less")


#ara farem inferencia sobre dues mitjanes
#test/contrast per a dues mitjanes
boxplot(Fulles$Llargada.mm~Fulles$Especie, col=11)
print(c("Mitjanes", tapply(Fulles$Llargada.mm, Fulles$Especie, mean)))

t.test(Llargada.mm~Especie,data=Fulles)

#efectivament puc dir que les fulles de quercus i rhamnus son diferents

#fem un test sobre igualtat de vari?ncies
bartlett.test(Fulles$Llargada.mm,Fulles$Especie)
#acceptem Ha
t.test(Llargada.mm~Especie,data=Fulles, var.equal=F)
t.test(Llargada.mm~Especie,data=Fulles, var.equal=T)

boxplot(Precipitacio_Fageda$DinsBosc.mm, Precipitacio_Fageda$ForaBosc.mm, col=2:3)

#test Student aparellat
t.test(Precipitacio_Fageda$ForaBosc.mm, Precipitacio_Fageda$DinsBosc.mm, paired=T)
#el 0 esta fora de l'interval de confian?a, rebutjem Ho, hi ha mes a fora q a dins

Precipitacio_Fageda$difer<-Precipitacio_Fageda$ForaBosc.mm~Precipitacio_Fageda$DinsBosc.mm
View(Precipitacio_Fageda) 
t.test(Fulles_Quercus$Llargada.mm, mu=40.5, conf.level = 0.9)
