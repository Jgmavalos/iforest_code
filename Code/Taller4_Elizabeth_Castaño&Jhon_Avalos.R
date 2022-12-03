#Elizabeth Castaño Ramírez
#Jhon Gesmer Méndez Ávalos

# Bases de datos 

base<- read.csv2("Datos_H-DAP2021.csv")
str(base)
colnames(base)<- c("Placa","DAP","Altura","Habito","Observaciones")

# Selecionamos los datos que nos permitan modelar la relacion DAP-Altura
base<- subset(base, Habito =="Arbol")
base<- subset(base, DAP != "")
set.seed(1152704780)
mod<- sample(dim(base)[1],150)
basem<- data.frame(base[mod,])

150*0.7

set.seed(1152704780)
artificio<- sample(dim(basem)[1],105)

train<- basem[artificio,]
validation<- basem[-artificio,]

#-----------------------------------------

# 1. Ajustes de modelos lineales y logarigmicos

# Modelo lineal 1
ml1<- lm(Altura ~ DAP, train)
summary(ml1)
anova(ml1)

## Supuestos
#-Normalidad
shapiro.test(residuals(ml1)) #Se rechaza H0, no existe normalidad
ks.test(residuals(ml1), "pnorm", mean = mean(residuals(ml1)),
        sd = sd(residuals(ml1))) # No se rechaza H0, los datos siguen una distribuccion

#identify(train$DAP, train$Altura)

# Posibles outliers
train[c(35,36,29,46,6,96,91,100,76),]

#Detección de observaciones influyentes
library(car)
outlierTest(ml1)
#Residuales estudentizados (detecta observaciones atípicas)
student_r <- rstudent(ml1)
which(abs(student_r) > 4)
which(abs(student_r) > 2)
#Outliers
influence.measures(ml1)
out <- influence.measures(ml1)$is.inf
lm.influence(ml1)
out<- data.frame(out)
str(out)
(out<- subset(out, cov.r == "TRUE" ))

# Modelo lineal 2, eliminando posibles outliers

ml2<- lm(Altura~ DAP + I(DAP^2), train)
summary(ml2)
anova(ml2)

## Supuestos
#-Normalidad
shapiro.test(residuals(ml2)) #No rechaza H0, existe normalidad

# Modelo logaritmico 1

mlog1 <- lm(log(Altura) ~ log(DAP), train)
summary(mlog1)

H_est <- exp(predict(mlog1))
mse <- sigma(mlog1)^2 
fc <- exp(mse/2)
H_est_m.ex <- exp(predict(mlog1))*fc
rangoDAP<- seq(min(train$DAP), max(train$DAP),0.2)
pred.m.ex <- exp(predict(mlog1, list(DAP = rangoDAP)))*fc
b0<- exp(0.7642+anova(mlog1)$"Mean Sq"[2]/2)  

## Supuesto
#Normalidad 
shapiro.test(residuals(mlog1))# No se rechaza H0, existe normalidad

# Modelo logarítmico 2

mlog2 <- lm(log(Altura) ~ log(DAP) + I((log(DAP))^2), train)
summary(mlog2)

fc2 <- exp(sigma(mlog2)^2/2)
H_est_m.ex2 <- exp(predict(mlog2))*fc2
pred.m.ex2 <- exp(predict(mlog2, list(DAP = rangoDAP)))*fc2
b0_<- exp(1.4139+anova(mlog2)$"Mean Sq"[2]/2)

## Supuesto
# Normalidad 
shapiro.test(residuals(mlog2))# No se rechaza H0, existe normalidad

#-----------------------------

# 2. Graficado de los modelos

plot(train$DAP, train$Altura, xlab = "DAP (cm)", ylab = "Altura (m)")
abline(ml1, col = 10)
abline(ml2, col = 11)
lines(rangoDAP, pred.m.ex, col = 12)
lines(rangoDAP, pred.m.ex2, col = 13)
legend("bottomright", legend = c("ml1", "ml2", "mlog1", "mlog2"), col = 10:13, lty = 1)

#----------------------------------------------

# 3. Comparacion y selecion de un modelo de cada tipo

#Lineales
AIC(ml1)
AIC(ml2) #Mejor modelo
#Logaritmicos
AIC(mlog1) #Mejor modelo
AIC(mlog2)

#- RSE = sqrt(residuales^2/gl)
#modelo lineal 1
summary(ml1)$sigma
#modelo lineal 1
summary(ml2)$sigma
#modelo logaritmico 1
resid1<- train$Altura - H_est_m.ex
RSE.exp1 <- sqrt(sum(resid1^2)/summary(mlog1)$df[2])
RSE.exp1
#modelo logaritmico 2
resid2 <- train$Altura - H_est_m.ex2
RSE.exp2 <- sqrt(sum(resid2^2)/summary(mlog2)$df[2])
RSE.exp2

#--------------------------------------------------------

# 4. comparacion de los valores de altura observados 
#    vs los predichos por cada modelo seleccionado

range(train$Altura)
x11()
par(mfrow=c(1,2))
plot(train$Altura, predict(ml2), las = 1, xlim = c(0,60), ylim = c(0,60),
     xlab = "Alturas observadas", ylab = "Alturas predichas", main = "Modelo lineal")
abline(0,1)
plot(train$Altura, H_est_m.ex, las = 1, xlim = c(0,60), ylim = c(0,60),
     xlab = "Alturas observadas", ylab = "Alturas predichas", main = "Modelo logaritmico")
abline(0,1)

#-------------------------------------------------

# 5. Validación

summary(ml2)
H_ml<- 3.6617 + 0.5928*validation$DAP - 0.0024*validation$DAP*validation$DAP

E1<-100*((H_ml-validation$Altura)/validation$Altura)
mean(E1)
sd(E1)

summary(mlog1)
H_mlog<- B0*validation$DAP^0.6253

E2<-100*((H_mlog-validation$Altura)/validation$Altura)
mean(E2)
sd(E2)



