#Realizado por
# Elizabeth Casta�o Ram�rez 
# Jhon Gesmer Mendez Avalos

# Librerias
library(stringr)
library(dplyr)

# Bases de datos
# 30 parcelas de 0,2-ha cada una,gradiente altitudinal entre 50 y 2.300 msnm

dt<- read.csv2("DatosTaller2_202108.csv")
str(dt)

ap<- read.csv2("AltitudParcelas.csv")
str(ap)

#-------------------------------------------------------------------------
  
## Punto 1. Tabla resumen por parcela

length(unique(dt$Parcela))
m<- merge(dt, ap, by= "Parcela")
length(unique(m$Parcela))
str(m)
summary(m)

m1<- subset(m, Especie !="")
sinespecie <- subset(m,Especie=="")
round((length(sinespecie$Especie)/length(m$Especie)*100),2) #�rboles sin identificaci�n

# Numero de individuos por hectarea
ni<- tapply(m$DAP_cm, m$Parcela, length)/0.2
ni<- data.frame(ni)

# Area basal por hectarea
m$G<- (pi/40000)*(m$DAP_cm)^2
G<- tapply(m$G,m$Parcela,sum)/0.2
G<- data.frame(G)

# Diametro promedio cuadratico por hectarea
Dq<- sqrt(tapply(m$DAP_cm^2, m$Parcela,mean))/0.2
Dq<- data.frame(Dq)

# Numero de especies
ne<- table(m1$Parcela, m1$Especie)
ne<- apply(ne>0, 1, sum)
ne<- data.frame(ne)
length(unique(m1$Especie))

# Numero de generos
# word funcion de libreria stringr
m1$Genero<-word(m1$Especie,1)
ng<- table(m1$Parcela, m1$Genero)
ng<- apply(ng>0, 1, sum)
ng<- data.frame(ng)
length(unique(m1$Genero))


# Numero de familias
nf<- table(m1$Parcela, m1$Familia)
nf<- apply(nf>0, 1, sum)
nf<- data.frame(nf)
length(unique(m1$Familia))
# Altitud
a<- tapply(m1$Altitud, m1$Parcela, mean)
a<- data.frame(a)

# Parcela
p<- c("P1", "P10", "P11", "P12", "P13", "P14", "P15", "P16", 
      "P17", "P18", "P19", "P2", "P20", "P21",  "P22",  "P23",  "P24",  "P25",  "P26",  "P27",
      "P28",  "P29",  "P3", "P30",  "P4", "P5", "P6", "P7", "P8", "P9")
p<- data.frame(p)

# Resultado tabla resumen por parcela ordenada en funcion de la altura
# arrange funcion de libreria dplyr
rp<- c(p, a, round(ni,2), round(G,2), round(Dq,2), nf, ng, ne)
rp<- data.frame(rp)
colnames(rp)<- c("Parcela", "Altitud", "N_ha", "G_ha", "Dq_ha", "N_Familias", "N_Generos", "N_Especies")
rp<- arrange(rp, Altitud)
rp

write.table(rp, "Tabla_resumen")

# Individuos identificados
length(m1$Especie)
#Individuos sin clasificacion taxonomica
nn<- subset(dt, Especie == "")
length(nn$Especie)

# Analisis descriptivo
round(mean(rp$N_ha),2)
round(sd(rp$N_ha),2)
t.test(rp$N_ha)

round(mean(rp$G_ha),2)
round(sd(rp$G_ha),2)
t.test(rp$G_ha)

round(mean(rp$Dq_ha),2)
round(sd(rp$Dq_ha),2)
t.test(rp$Dq_ha)

mean(rp$N_Especies)
round(sd(rp$N_Especies),2)
t.test(rp$N_Especies)

#--------------------------------------------------------------------------------
  
## Punto 2. Relacion entre variables (N_ha, G_ha, Dq_ha, N_Especies) con la altura


#test de normalidad
qqnorm(rp$N_ha, col = "Yellow")
qqline(rp$N_ha, col = "Gray")
shapiro.test(rp$N_ha) # No son normales

qqnorm(rp$G_ha, col = "Blue")
qqline(rp$G_ha, col = "Gray")
shapiro.test(rp$G_ha) # Son normales

qqnorm(rp$Dq_ha, col = "Red")
qqline(rp$Dq_ha, col = "Gray")
shapiro.test(rp$Dq_ha) # No son normales

qqnorm(rp$N_Especies, col = "Green")
qqline(rp$N_Especies, col = "Gray")
shapiro.test(rp$N_Especies) # Son normales

# histogramas de frecuencia
x11()
par(mfrow=c(2,2))
hist(rp$N_ha, xlab= "Numero de individuos por ha", ylab ="Frecuencia", main = "")
hist(rp$G_ha, xlab= "Area basal promedio por ha", ylab ="Frecuencia", main = "")
hist(rp$Dq_ha, xlab= "Diametro cuadratico promedio por ha", ylab ="Frecuencia", main = "")
hist(rp$N_Especies, xlab= "Numero de especies promedio por ha", ylab ="Frecuencia", main = "")

# Normalidad para datos no parametricos
ks.test(rp$N_ha, "pnorm", mean= mean(rp$N_ha), sd = sd(rp$N_ha)) # Son normales
ks.test(rp$Dq_ha, "pnorm", mean= mean(rp$Dq_ha), sd = sd(rp$Dq_ha)) # Son normales

#Correlacion (independencia)
cor.test(rp$N_ha, rp$Altitud, method = "spearman") # No existe una relacion significativa entre la variables 
cor.test(rp$G_ha, rp$Altitud) # Relacion significativa
cor.test(rp$Dq_ha, rp$Altitud, method = "spearman")  # No significativa
cor.test(rp$N_Especies, rp$Altitud) # No significativa

# Homogeneidad de varianzas
var.test(rp$N_ha)

#---------------------------------------------------
#Punto 3.
# Datos por bosque
b1<- subset(m, Altitud <= 1000)
b2<- subset(m, Altitud > 1000 & Altitud < 2000)
b3<- subset(m, Altitud >= 2000)

length(b1$Parcela) + length(b2$Parcela) + length(b3$Parcela)
length(m$Parcela)

# Bosque 1
nib1<- tapply(b1$DAP_cm, b1$Parcela, length)/0.2
nib1<- data.frame(nib1)

Gb1<- tapply(b1$G,b1$Parcela,sum)/0.2
Gb1<- data.frame(Gb1)

Dqb1<- sqrt(tapply(b1$DAP_cm^2, b1$Parcela,mean))/0.2
Dqb1<- data.frame(Dqb1)

# Bosque 2
nib2<- tapply(b2$DAP_cm, b2$Parcela, length)/0.2
nib2<- data.frame(nib2)

Gb2<- tapply(b2$G,b2$Parcela,sum)/0.2
Gb2<- data.frame(Gb2)

Dqb2<- sqrt(tapply(b2$DAP_cm^2, b2$Parcela,mean))/0.2
Dqb2<- data.frame(Dqb2)

#  Bosque 3
nib3<- tapply(b3$DAP_cm, b3$Parcela, length)/0.2
nib3<- data.frame(nib3)

Gb3<- tapply(b3$G,b3$Parcela,sum)/0.2
Gb3<- data.frame(Gb3)

Dqb3<- sqrt(tapply(b3$DAP_cm^2, b3$Parcela,mean))/0.2
Dqb3<- data.frame(Dqb3)

# Analisi descriptivo
mean_ni<- c(mean(nib1[,1]), mean(nib2[,1]), mean(nib3[,1]))
sd_ni<- c(sd(nib1[,1]), sd(nib2[,1]), sd(nib3[,1]))

mean_G<- c(mean(Gb1[,1]), mean(Gb2[,1]), mean(Gb3[,1]))
sd_G<- c(sd(Gb1[,1]), sd(Gb2[,1]), sd(Gb3[,1]))

mean_Dq<- c(mean(Dqb1[,1]), mean(Dqb2[,1]), mean(Dqb3[,1]))
sd_Dq<- c(sd(Dqb1[,1]), sd(Dqb2[,1]), sd(Dqb3[,1]))

# Tabla de resumen

tb1<- round(data.frame(mean(nib1[,1]),sd(nib1[,1]), mean(Gb1[,1]), sd(Gb1[,1]), mean(Dqb1[,1]), 
                 sd(Dqb1[,1])),2)
colnames(tb1)<- c("Promedio_Ni", "Sd_Ni", "Promedio_G", "Sd_G","Promedio_Dq", "Sd_Dq") 
  
tb2<- round(data.frame(mean(nib2[,1]),sd(nib2[,1]), mean(Gb2[,1]), sd(Gb2[,1]), mean(Dqb2[,1]), 
                       sd(Dqb2[,1])),2)
colnames(tb2)<- c("Promedio_Ni", "Sd_Ni", "Promedio_G", "Sd_G","Promedio_Dq", "Sd_Dq") 

tb3<- round(data.frame(mean(nib3[,1]),sd(nib3[,1]), mean(Gb3[,1]), sd(Gb3[,1]), mean(Dqb3[,1]), 
                       sd(Dqb3[,1])),2)
colnames(tb3)<- c("Promedio_Ni", "Sd_Ni", "Promedio_G", "Sd_G","Promedio_Dq", "Sd_Dq") 


Tipo_bosque<- c("Bosque 1", "Bosque 2", "Bosque 3")
rb<- rbind(tb1, tb2, tb3)
rb<- data.frame(Tipo_bosque, rb)
rb

write.table(rb, "Tabla_resumen2")

#-------------------------------------------------------------------------------

# Punto 4. Supuestos del ANAVA para dos variables entre los tipos de bosque

b1<- subset(m, Altitud <= 1000)
b2<- subset(m, Altitud > 1000 & Altitud < 2000)
b3<- subset(m, Altitud >= 2000)

b1$Tipo_bosque<- c(rep("Bosque 1", 963))

b2$Tipo_bosque<- c(rep("Bosque 2", 1642))

b3$Tipo_bosque<- c(rep("Bosque 3", 1776))

bosques<- rbind(b1, b2, b3)

bosques$G <- bosques$G/0.2
dim(bosques)
bosques$ID <- 1:4381
library(doBy)
G.p <- summaryBy(G ~ Parcela + Tipo_bosque, FUN = sum, data = bosques, keep.names = T)
str(G.p)
length(G.p)
G.p$Dq_p <- sqrt(tapply(bosques$DAP_cm^2, bosques$Parcela, mean))/0.2
G.p$Ni_ha <- tapply(bosques$ID, bosques$Parcela, length)/.2

#ANAVA

# Idependencia
# Normalidad
x11()
par(mfrow=c(1,2))
qqnorm(G.p$G, col = "Green")
qqline(G.p$G, col = "Gray")
hist(G.p$G,main="Histograma �rea basal")
shapiro.test(G.p$G) 
ks.test(G.p$G, "pnorm", mean = mean(G.p$G), sd = sd(G.p$G))

x11()
par(mfrow=c(1,2))
qqnorm(G.p$Ni_ha, col = "Green")
qqline(G.p$Ni_ha, col = "Gray")
hist(G.p$Ni_ha, main = "Histograma Ni_ha")
shapiro.test(G.p$Ni_ha) 
ks.test(G.p$Ni_ha, "pnorm", mean = mean(G.p$Ni_ha), sd = sd(G.p$Ni_ha))

#Homocedasticidad: 
bartlett.test(G.p$G ~ G.p$Tipo_bosque)
bartlett.test(G.p$Ni_ha ~ G.p$Tipo_bosque)

x11()
par(mfrow=c(1,1))

#An�lisis de varianza

G_anava<- aov(G ~ Tipo_bosque, G.p)

summary(G_anava)
TukeyHSD(G_anava)
plot(TukeyHSD(G_anava), col="red")

x11()
par(mfrow=c(1,1))
Ni_anava<- aov(Ni_ha ~ Tipo_bosque, G.p)
summary(Ni_anava)
TukeyHSD(Ni_anava)
plot(TukeyHSD(Ni_anava), col="red")

#

#-------------------------------------------------------------------------------

#Punto 5 acumulaci�n de especies vs individuos
install.packages("vegan")
library(vegan)

bosque1<- (subset(bosques, Tipo_bosque =="Bosque 1" & Especie!=""))

n <- NULL
spi<- vector(length = 100)
n.ind <- seq(10,600,10)
sp.mean <- sp.sd <- NULL

for(n in n.ind){
    spi<- vector(length = 100)
    for(i in 1:length(spi)){
      bdi<- bosque1[sample(1:dim(bosque1)[1],n),]
      spi[i] <-specnumber(table(bdi$Especie))
    }
  sp.mean <- c(sp.mean,mean(spi))
  sp.sd <- c(sp.sd,sd(spi))
}
x11()
plot(n.ind,sp.mean,type="l",lwd=2, main = "Bosque 1")
lines(n.ind,sp.mean+sp.sd,lty=3,lwd=2)
lines(n.ind,sp.mean-sp.sd,lty=3,lwd=2)



##Bosque 2 
bosque2<- (subset(bosques, Tipo_bosque =="Bosque 2" & Especie!=""))



n2 <- NULL
spi2<- vector(length = 100)
n.ind2 <- seq(20,1600,20)
sp.mean2 <- sp.sd2 <- NULL

for(n in n.ind2){
  spi2<- vector(length = 100)
  for(i in 1:length(spi2)){
    bdi2<- bosque2[sample(1:dim(bosque2)[1],n),]
    spi2[i] <-specnumber(table(bdi2$Especie))
  }
  sp.mean2 <- c(sp.mean2,mean(spi2))
  sp.sd2 <- c(sp.sd2,sd(spi2))
}
x11()
plot(n.ind2,sp.mean2,type="l",lwd=2, main = "Bosque 2")
lines(n.ind2,sp.mean2+sp.sd2,lty=3,lwd=2)
lines(n.ind2,sp.mean2-sp.sd2,lty=3,lwd=2)

