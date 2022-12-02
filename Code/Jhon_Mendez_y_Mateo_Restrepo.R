dt<-read.csv2("parcial1.csv")
str(dt)
range(dt$DAP_cm)


dt$Ab_m2<-with(dt,(pi/40000)*DAP_cm^2)

#Estracion por bosque
bosque1<-droplevels(subset(dt,Bosque=="bosque1"))
bosque2<-droplevels(subset(dt,Bosque=="bosque2"))
bosque3<-droplevels(subset(dt,Bosque=="bosque3"))
bosque4<-droplevels(subset(dt,Bosque=="bosque4"))

# Estructura horizontal por parcela-bosque #

#Punto 1: Numero de individuos promedio por parcela - bosque
Nb1<-with(bosque1,tapply(DAP_cm,Parcela,length))
Nb2<-with(bosque2,tapply(DAP_cm,Parcela,length))
Nb3<-with(bosque3,tapply(DAP_cm,Parcela,length))
Nb4<-with(bosque4,tapply(DAP_cm,Parcela,length))
mean(Nb1)
mean(Nb2)
mean(Nb3)
mean(Nb4)

#Punto 2: Area basal promedio por parcela - bosque
Ab1<-with(bosque1,tapply(Ab_m2,Parcela,sum))
Ab2<-with(bosque2,tapply(Ab_m2,Parcela,sum))
Ab3<-with(bosque3,tapply(Ab_m2,Parcela,sum))
Ab4<-with(bosque4,tapply(Ab_m2,Parcela,sum))
mean(Ab1)
mean(Ab2)
mean(Ab3)
mean(Ab4)

#Punto 3: Diametro cuadratico promedio por parcela - bosque
Dqb1<-sqrt(with(bosque1,tapply(DAP_cm^2,Parcela,mean)))
Dqb2<-sqrt(with(bosque2,tapply(DAP_cm^2,Parcela,mean)))
Dqb3<-sqrt(with(bosque3,tapply(DAP_cm^2,Parcela,mean)))
Dqb4<-sqrt(with(bosque4,tapply(DAP_cm^2,Parcela,mean)))
Dqb1
Dqb2
Dqb3
Dqb4
mean(Dqb1)
mean(Dqb2)
mean(Dqb3)
mean(Dqb4)

#Punto 4: Distribuccion clases diametricas por bosque
range(bosque1$DAP_cm)
range(bosque2$DAP_cm)
range(bosque3$DAP_cm)
range(bosque4$DAP_cm)

par(mfrow=c(2,2))

frb1<-cut(bosque1$DAP_cm,seq(10,20,10))
frb12<-table(frb1)
names(frb12)<-as.character(10)
barplot(frb12,xlab = "DAP (cm)",ylab = "Numero de individuos",space = 0)

frb2<-cut(bosque2$DAP_cm,seq(0,40,10))
frb22<-table(frb2)
names(frb22)<-as.character(seq(5,35,10))
barplot(frb22,xlab = "DAP (cm)",ylab = "Numero de individuos",space = 0)

frb3<-cut(bosque3$DAP_cm,seq(10,40,10))
frb32<-table(frb3)
names(frb32)<-as.character(seq(15,35,10))
barplot(frb32,xlab = "DAP (cm)",ylab = "Numero de individuos",space = 0)

frb4<-cut(bosque4$DAP_cm,seq(30,130,10))
frb42<-table(frb4)
names(frb42)<-as.character(seq(35,125,10))
barplot(frb42,xlab = "DAP (cm)",ylab = "Numero de individuos",space = 0)

#Punto 7 
#Comparaciones multiples area basal promedio por bosques
Abt<- c(Ab1, Ab2, Ab3, Ab4)
Abt<- as.table(Abt) 
Abt<- as.data.frame(Abt)
Abt$Bosque<- c(rep("b1",5),rep("b2",5),rep("b3",4),rep("b4",7))
Ab_anava<- aov(Freq~Bosque, Abt) 
summary(Ab_anava)
TukeyHSD(Ab_anava)
plot(TukeyHSD(Ab_anava), col="red")
#Comparaciones multiples diametro cuadratico promedio por bosques
Dqt<- c(Dqb1, Dqb2, Dqb3, Dqb4)
Dqt<- as.table(Dqt) 
Dqt<- as.data.frame(Dqt)
Dqt$Bosque<- c(rep("b1",5),rep("b2",5),rep("b3",4),rep("b4",7))
Dq_anava<- aov(Freq~Bosque, Dqt) 
summary(Dq_anava)
TukeyHSD(Dq_anava)
plot(TukeyHSD(Dq_anava), col="red")
