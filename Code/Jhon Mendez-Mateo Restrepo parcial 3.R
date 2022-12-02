datos <- read.csv2("Parcial_3.csv" )
newton <- function (N,d,h) {
  x <- data.frame(N,d,h)
  n.trees <- unique(x$N)
  x$basal_area_i <- with(x, (pi/40000)*d^2)
  v.trees <- NULL
  vnew <- 0
  for (i in 1:length(n.trees)) {
    tree_i <- x[x$N==i,]
    for(j in 1:floor(nrow(tree_i)/2)){
      v.new.i <- with(tree_i, 2*(h[2*j]-h[2*j-1])/6*
                        (basal_area_i[2*j-1] +
                           4*basal_area_i[2*j]+basal_area_i[2*j+1]))
      vnew <- sum(vnew,v.new.i)
    }
    v.trees <- cbind(v.trees,vnew)
    vnew <- 0
  }
  colnames(v.trees) <- c(1:length(v.trees))
  return(v.trees)
}
volumenes <- t(with(datos,newton(No_.arbol,di,hi)))
x <- seq(1,825,11)
arboles <- datos[x,]
arboles$volumenes <-volumenes 

summary(arboles)


set.seed(200)
y <- sample(1:75,60)
datosa <- arboles[y,]
datosv <- arboles[-y,]

summary(datosa)

datosa$g  <- with(datosa, (pi/40000)*D^2)
datosa$ff <- with(datosa,(volumenes/(g*H))) 
factorforma <- mean(datosa$ff)  

modelo1 <- with(datosa,lm(volumenes~D+I(D^2)+H+I(H^2)))  
summary(modelo1) 

modelo2<- with(datosa,lm(volumenes~I(D^2*H)))
summary(modelo2) 

modelo3 <-with(datosa,lm(log(volumenes)~log(D)+log(H)))
summary(modelo3) 

modelo4 <-with(datosa,lm(log(volumenes)~I(log((D^2)*H))))
summary(modelo4)



AIC(modelo1)
AIC(modelo2)
AIC(modelo3)
AIC(modelo4)

datosa$predichos3<- exp(predict(modelo3))*exp(anova(modelo3)$"Mean Sq"[3]/2)
RSE3<- sqrt(sum((datosa$volumenes-datosa$predichos3)^2)/57)

datosv$volumenff <- with(datosv,0.508738*H*(D/200)^2*pi)
datosv$volumenmodelo <- exp(-10.21704 +1.81791*log(datosv$D)+ 1.22329*log(datosv$H))*exp(anova(modelo3)$"Mean Sq"[3]/2)

sesgoff <- ((datosv$volumenff-datosv$volumenes)/datosv$volumenes)*100
mean(sesgoff)
sd(sesgoff)

sesgomodelo <- ((datosv$volumenmodelo-datosv$volumenes)/datosv$volumenes)*100
mean(sesgomodelo)
sd(sesgomodelo)

parcelas <- read.csv2("Parcelas_P3.csv")

View(parcelas)
library(BIOMASS)
with(parcelas,modelHD(D=DAP_cm,H=Ht_m,method="weibull"))


parcelas$Ht_m[is.na(parcelas$Ht_m)] <- (24.244989*(1-exp(-(parcelas$DAP_cm[is.na(parcelas$Ht_m)]/17.571)^ 1.181 )))
parcelas$volumenes <-exp(-10.21704 +1.81791*log(parcelas$DAP_cm)+ 1.22329*log(parcelas$Ht_m))*exp(anova(modelo3)$"Mean Sq"[3]/2) 
volumenbosque <- with(parcelas,tapply(volumenes,Tipo.de.bosque,sum))*20
anava<-aov(volumenes~Tipo.de.bosque, parcelas)
summary(anava)
TukeyHSD(anava)




