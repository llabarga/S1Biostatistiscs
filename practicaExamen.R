### Ejercicios de practica
## Bloque 1
# 1.Estadística descriptiva per a la llargada de les fulles de l’espècie SP1:
describe(fullesSP1$llarg_fulla)
llarg_fulla<-fullesSP1$llarg_fulla
describeBy(llarg_fulla)

# 2.Histograma de la llargada de les fulles de l’espècie SP1:
hist(llarg_fulla, col = c("pink"), xlab="Llarg fulla (mm)", ylab = "Frecuencia")

# 3.Diagrama de caixes (boxplot) de la llargada de les fulles de l’espècie SP1:
boxplot(fullesSP1$llarg_fulla, col = c("skyblue"), ylab= "Llarg Fulla (mm)")

# 4.Estadística descriptiva de la llargada de les fulles de l’espècie SP1 per a cada població:
describeBy(fullesSP1)
P1<-subset(fullesSP1,fullesSP1$poblacio=="P1")
describeBy(fullesSP1, group = "poblacio") # DescribeBy será para un subconjutno
# entero, la cosa es que también aparecerá especie y poblacio
describe(P1$llarg_fulla) # Aquí no me da ningún warning porque es para una variable
describeBy(P1$llarg_fulla) #Este me da un error "no grouping variable" 
# porque no hay nada que agrupar, es una columna
describeBy(fullesSP1$llarg_fulla,fullesSP1$poblacio) #variable a describir por la columna a agrupar
# 5. Diagrama de caixes (boxplot) de la llargada de les fulles de l’espècie SP1 per a cada població:
boxplot(fullesSP1$llarg_fulla~fullesSP1$poblacio, col=c("skyblue","red", "green"))
boxplot(fullesSP1$llarg_fulla~fullesSP1$poblacio, col=c(7:2))

# 6.Comprovem si la llargada mitjana de la població de fulles de l’espècie SP1 és de 34 cm:
t.test(fullesSP1$llarg_fulla, mu=34)

# 7.Alternativament ens pot interessar comprovar si la llargada mitjana de les fulles de l’espècie SP1 és més gran que 32 cm :
t.test(fullesSP1$llarg_fulla, mu=32, alternative = "greater")
qt(p=0.05, df=289, lower.tail=FALSE)

## Bloque 2
# 1. Volem saber si la llargada de les fulles de les espècies (SP1 i SP2) són diferents:
boxplot(fulles_SP1_SP2$llarg_fulla~fulles_SP1_SP2$especie, xlab = "Poblaciones", ylab = "Llarg fulles (mm)")
bartlett.test(fulles_SP1_SP2$llarg_fulla, fulles_SP1_SP2$especie)
bartlett.test(SP1$llarg_fulla, SP2$llarg_fulla) # no sé cual es el problema
# p-value<0.05 por lo que rechazamos la hipótesis nula, las varianzas son diferentes

# Hacemos un t-test para las dos poblaciones
describeBy(fulles_SP1_SP2$llarg_fulla, fulles_SP1_SP2$especie)
t.test(fulles_SP1_SP2$llarg_fulla~fulles_SP1_SP2$especie, mu=0, var.equal=FALSE)

## Bloque 3
# 1.Estadística descriptiva dels diàmetres l’any 2005 per a ambdós sexes i en totes les poblacions:
describe(arbres$dn_cm_2005)

# 2.Histograma de diàmetres dels arbres (mascles i femelles) el 2005 en totes les poblacions:
hist(arbres$dn_cm_2005, col = c("skyblue2"), xlab = "Diametre Normal 2005", ylab = "Frecuencia", main = "Diámetro del tronco 2005")

# 3.Diagrama de caixes (boxplot) dels diàmetres el 2005 dels mascles i de les femelles en totes les poblacions:
boxplot(arbres$dn_cm_2005~arbres$sexe, col= c("white", "grey"))

# 4.Estadística descriptiva dels diàmetres dels arbres en cada població l’any 2010:
describeBy(arbres$dn_cm_2010, arbres$pop, quant=c(0.25,0.75))

# 5. Diagrama de caixes dels diàmetres dels arbres (mascles i femelles) en cada població l’any 2010:
boxplot(arbres$dn_cm_2010~arbres$pop, ylab = "Dn 2010 (cm)", xlab = "Población", col=c("pink","grey"))

# 6. Comprovem si el 2010 els arbres del Montseny són diferents dels de Prades:
bartlett.test(arbres$dn_cm_2010, arbres$area)
t.test(arbres$dn_cm_2010~arbres$area, var.equal = T) #si son 2 poblaciones , utilizamos ~

# 7. Tenint en compte les estimes del diàmetre obtingudes el 2010 en totes les poblacions, podem dir que els mascles són més grans que les femelles?
bartlett.test(arbres$dn_cm_2010, arbres$sexe)
t.test(arbres$dn_cm_2010~arbres$sexe, var.equal= T, alternative="less")

t.test(arbres$dn_cm_2010~arbres$sexe, var.equal= T)
t.test(arbres$dn_cm_2010~arbres$sexe, var.equal= T, alternative="greater")
# 8.Ens preguntem si existeix un increment de diàmetre significatiu entre ambdós períodes de mesura (sense tenir en compte ni el sexe ni la població):
t.test(arbres$dn_cm_2010,arbres$dn_cm_2005, paired=T, var.equal = T, alternative= "greater")

boxplot(arbres$dn_cm_2005)
