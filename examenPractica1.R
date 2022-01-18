# Ejercicio 1
describe(Farms$SOM_Spring, quant = c(0.25,0.5,0.75))

#Ejercicio 2
boxplot(Farms$SOM_Spring~Farms$Localitat)

#Ejercicio 3
describeBy(Farms$SOM_Spring,Farms$Localitat)
bartlett.test(Farms$SOM_Spring,Farms$Localitat)
t.test(Farms$SOM_Spring~Farms$Localitat, var.equal=T)
qt(p=0.025, df=46, lower.tail = T)

# Ejercicio 4
describe(Farms$SOM_Fall)
describe(Farms$SOM_Spring)
bartlett.test(Farms$SOM_Fall,Farms$SOM_Spring)
t.test(Farms$SOM_Spring,Farms$SOM_Fall, paired= T)

# Ejercicio 5
describeBy(Farms$SOM_Spring,Farms$Gestio)
#pvalue>0.05
bartlett.test(Farms$SOM_Spring,Farms$Gestio)
t.test(Farms$SOM_Spring~Farms$Gestio, var.equal = T)
# pvalue<0.05 y no está en el intervalo

#Ejercicio 6
RG<-subset(Farms,Farms$Gestio=="REG")
t.test(RG$SOM_Spring, mu=2.25, alternative = "greater")
mean(RG$SOM_Spring)
