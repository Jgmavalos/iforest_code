## Punto 1
(v1<- c(seq(17,24,1),26))
(v2<- c(rep(10,5),rep(11,3)))
(v3<- seq(4,5,0.2))
(v4<- letters[seq(13,20,1)])
(V5<- c(17,19,21,23))
v1pi<- ifelse (v1%%2==0,"par","impar")
#####################################################################################

library(dplyr) 
# Libreria de ayuda
# El correcto funcionamiento de mi script depende de esta libreria
# Por favor instalar si no la tiene 

###################################################################################

## Punto 2
bd<- read.csv2("BD_Taller.csv")
str(bd)
attach(bd)

#Se crea la variable DAP
bd$DAP<- ifelse(is.na(CAP), (dap_1+dap_2)/2,
                CAP/pi)

#Se crea la variable de area basal
bd$AB<- (pi*(bd$DAP^2))/40000

## Se selecionan los que cumplen DAP>= 10 ##
bd10<- subset(bd, DAP>=10)

#Se seleccionan 4 parcelas aleatorias
set.seed(1)
(p<-sample(bd10$Parcela,4))

#Se selecciona la informacion de interes de las 4 parcelas
attach(bd10)
bd1<- subset(bd10, Parcela == "PP1")
bd6<- subset(bd10, Parcela == "PP6")
bd3<- subset(bd10, Parcela == "PP3")
bd2<- subset(bd10, Parcela == "PP2")

minD<- c(summary(bd1$DAP)[1],summary(bd6$DAP)[1],summary(bd3$DAP)[1],summary(bd2$DAP)[1] )
maxD<- c(summary(bd1$DAP)[6],summary(bd6$DAP)[6],summary(bd3$DAP)[6],summary(bd2$DAP)[6] )
minA<- c(summary(bd1$Altura)[1],summary(bd6$Altura)[1],summary(bd3$Altura)[1],summary(bd2$Altura)[1] )
maxA<- c(summary(bd1$Altura)[6],summary(bd6$Altura)[6],summary(bd3$Altura)[6],summary(bd2$Altura)[6] )

TablaMinMax<- data.frame(p, minD, maxD, minA, maxA)
colnames(TablaMinMax)<- c("Parcela", "min DAP", "max DAP", "min Altura", "max Altura")
TablaMinMax


############################################################################################

## Punto 3
# Aunque se pueden determinar los minimos y maximos de las variables DAP y Altura
# es de recalcar que Altura posee 2731 campos vacios (NA), por lo cual, para 
# determinar la media y desviacion estandar real de esta variable debemos extraer dichos datos
# Ademas, asi no interferira los NA en la creacion de las graficas (punto 6 y 7)
summary(bd10$Altura) # Comprobamos que los datos poseen NA
summary(bd10$DAP)    # sin NA
summary(bd10$AB)     # sin NA

#Se extraen ("eliminan") los NA de la variable Altura del data frame db10
bda<- subset(bd10, Altura!=is.na(Altura))

bdp1<- subset(bda, Parcela == "PP1")
bdp6<- subset(bda, Parcela == "PP6")
bdp3<- subset(bda, Parcela == "PP3")
bdp2<- subset(bda, Parcela == "PP2")

# medias
(meanD <- c (mean(bdp1$DAP), mean(bdp6$DAP),mean(bdp3$DAP), mean(bdp2$DAP))) # media DAP por parcela
(meanAl<- c (mean(bdp1$Altura), mean(bdp6$Altura),mean(bdp3$Altura), mean(bdp2$Altura))) # media Altura por parcela
(meanAB <- c (mean(bdp1$AB), mean(bdp6$AB),mean(bdp3$AB), mean(bdp2$AB))) # media Area Basal por parcela

# desviacion estandar
(sdD<- c (sd(bdp1$DAP), sd(bdp6$DAP), sd(bdp3$DAP), sd(bdp2$DAP))) # sd DAP por parcela
(sdAl<- c (sd(bdp1$Altura), sd(bdp6$Altura), sd(bdp3$Altura), sd(bdp2$Altura))) # sd Altura por parcela
(sdAB<- c (sd(bdp1$AB), sd(bdp6$AB), sd(bdp3$AB), sd(bdp2$AB))) # sd Area Basal por parcela

# se aplica tapply para extraer el numero de individuos por familia para cada parcela
nf1<-with(bdp1,tapply(Parcela, Familia, length))
n1<- data.frame(nf1) # Numero individuos por familia
dim(n1) # Numero de familias
sum(n1$nf1) # Numero de individuos

nf6<-with(bdp6,tapply(Parcela, Familia, length))
n6<- data.frame(nf6)

nf3<-with(bdp3,tapply(Parcela, Familia, length))
n3<- data.frame(nf3)

nf2<-with(bdp2,tapply(Parcela, Familia, length))
n2<- data.frame(nf2)

nf<- c(dim(n1)[1],dim(n6)[1], dim(n3)[1], dim(n2)[1]) # Numero de familias por parcela
ni<- c (sum(n1$nf1), sum(n6$nf6), sum(n3$nf3), sum(n2$nf2)) # Numero de individuos por parcela

######################################################################

## Punto4
dim(bdp1)*0.2
dh1<- order(-bdp1$Altura) %>% head(100)
dx1<- bdp1[dh1,]

dim(bdp6)*0.2
dh6<- order(-bdp6$Altura)%>%head(85)
dx6<- bdp6[dh6,]

dim(bdp3)*0.2
dh3<- order(-bdp3$Altura)%>%head(56)
dx3<- bdp3[dh3,]

dim(bdp2)*0.2
dh2<- order(-bdp2$Altura)%>%head(43)
dx2<- bdp2[dh2,]

(meandAl<- c(mean(dx1$Altura), mean(dx6$Altura), mean(dx3$Altura), mean(dx2$Altura))) # media Altura de arboles dominantes

####################################################################################################################

## Punto 5 
TablaResumen1<- data.frame (p, meanD, sdD, meanAl, sdAl, meandAl, meanAB, sdAB, nf, ni)
colnames(TablaResumen1)<- c("Parcela", "media DAP", "sd DAP", "media Altura", "sd Altura", "media Altura dominantes", "media Area Basal", "sd Area Basal", "N de familias", "N de individuos")
TablaResumen1

############################################################################################################################


## Punto 6
### Grafica de la distribuccionde alturas ###
layout(matrix(c(1:4), nrow = 2))

title("Distribucion de la Altura")

(range(bdp1$Altura))
bdp1$cat.dap<-cut(bdp1$DAP,c(seq(7,30,5),30),labels=c(seq(7,30,5)))
barplot(table(bdp1$cat.dap), ylim = c(0,200), xlab = "Altura", 
        ylab = "Frecuencia", main = "Parcela PP1")
abline(h=0)

(range(bdp3$Altura))
bdp3$cat.dap<-cut(bdp3$DAP,c(seq(7,30,5),30),labels=c(seq(7,30,5)))
barplot(table(bdp3$cat.dap), ylim = c(0,200), xlab = "Altura", 
        ylab = "Frecuencia", main = "Parcela PP3")
abline(h=0)

(range(bdp2$Altura))
bdp2$cat.dap<-cut(bdp2$DAP,c(seq(2,46,5),46),labels=c(seq(2,46,5)))
barplot(table(bdp2$cat.dap), ylim = c(0,200), xlab = "Altura", 
        ylab = "Frecuencia", main = "Parcela PP2")
abline(h=0)

(range(bdp6$Altura))
bdp6$cat.dap<-cut(bdp6$DAP,c(seq(7,31,5),31),labels=c(seq(7,31,5)))
barplot(table(bdp6$cat.dap), ylim = c(0,200), xlab = "Altura", 
        ylab = "Frecuencia", main = "Parcela PP6")
abline(h=0)


################################################################################

## Punto 7
### Grafico DAP vs Altura de las 4 parcelas, condicion DAP>=20  ###
#se extraen los datos con DAP>=20 que a su vez poseen datos de altura 
# bda es un subset de los datos bd10, donde para todos los individuos se conoce la altura)
bd20<- subset(bda, DAP>=20)

#Subset por parcela
p1<- subset(bd20, Parcela == "PP1")
p6<- subset(bd20, Parcela == "PP6")
p3<- subset(bd20, Parcela == "PP3")
p2<- subset(bd20, Parcela == "PP2")

h4<- c(p1$Altura,p6$Altura,p3$Altura,p2$Altura)
dap4<- c(p1$DAP,p6$DAP,p3$DAP,p2$DAP)

#Se organiza la informacion en un data frame
dim(p1)
dim(p6)
dim(p3)
dim(p2)
parce<- c(rep("1",210),rep("6",124),rep("3",73),rep("2",94))

bdf<- data.frame(parce,dap4, h4)
colnames(bdf)<- c("Parcela", "DAP", "Altura")

layout(matrix(c(1:1)))
attach(bdf)
plot(DAP, Altura, col = Parcela, pch = 16, xpd = 2, main = "DAP vs Altura")
legend("bottomright", pch = 16, col = 1:4, legend = levels(bdf$Parcela), trace = TRUE)
abline(h = mean(Altura), col = "blue", lty=3 )
text(70, mean(Altura), "Promedio Altura", col = "Orange")
abline(v = mean(DAP), col = "blue", lty=3)
text(mean(DAP), 40, "Promedio DAP", col = "Orange")

################################################################################

## Punto 8
### Grafica de las 10 familias mas abundantes por parcela  ###
n1
n6
n3
n2


# con la funcion arrange de dplyr selecciono las 10 mas abundantes
n_1<- arrange(n1,-nf1)%>% head(10)
n_6<- arrange(n6,-nf6)%>% head(10)
n_3<- arrange(n3,-nf3)%>% head(10)
n_2<- arrange(n2,-nf2)%>% head(10)


# se crean variables con los nombres de la familias, esto con el fin de crear un data.frame que se deje manipular en funcion de mi necesidad
fn1<- c("Lauraceae","Cunoniaceae", "Euphorbiaceae","Sapindaceae", 
        "Clusiaceae", "Clethraceae", "Lacistemataceae", 
        "Melastomataceae", "Rubiaceae", "Styracaceae")

f1<-data.frame(fn1,n_1$nf1)
colnames(f1)<-c("Familia","N")
#
fn6<- c("Moraceae","Arecaceae", "Melastomataceae","Urticaceae", 
        "Euphorbiaceae", "Annonaceae", "Bignoniaceae", 
        "Sapindaceae", "Boraginaceae", "Fabaceae")

f6<-data.frame(fn6,n_6$nf6)
colnames(f6)<-c("Familia","N")
#
fn3<- c("Fabaceae","Burseraceae", "Lauraceae","Fagaceae", 
        "Sapotaceae", "Myristicaceae", "Melastomataceae", 
        "Linaceae", "Clusiaceae", "Sapindacea")

f3<-data.frame(fn3,n_3$nf3)
colnames(f3)<-c("Familia","N")
#
fn2<- c("Arecaceae","Myristicaceae", "Fabaceae","Moraceae", 
        "Sapotaceae", "Salicaceae", "Apocynaceae", 
        "Humiricaceae", "Bignoniaceae", "Araliaceae")

f2<-data.frame(fn2,n_2$nf2)
colnames(f2)<-c("Familia","N")

# se organiza toda la informacion en un solo data frame
fp<-c(rep("1",10),rep("6",10),rep("3",10),rep("2",10))
fx<-c(fn1,fn6,fn3,fn2)
fy<-c(n_1$nf1,n_6$nf6,n_3$nf3,n_2$nf2)
fn<-data.frame(fp,fx,fy)
colnames(fn)<-c("Parcela","Familia","N")


layout(matrix(c(1:4), nrow = 2))
pie(f1$N, labels = f1$Familia, main = "PP1", radius = 1.2, col = c(1:10))
pie(f3$N, labels = f3$Familia, main = "PP3", radius = 1.2, col = c(10:20))
pie(f2$N, labels = f2$Familia, main = "PP2", radius = 1.2, col = c(20:30))
pie(f6$N, labels = f6$Familia, main = "PP6", radius = 1.2, col = c(30:40))







  
  


