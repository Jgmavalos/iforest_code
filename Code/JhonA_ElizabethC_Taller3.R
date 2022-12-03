#Elizabeth Castaño Ramírez
#Jhon Gesmer Méndez Ávalos

#-------------------------------

base <- read.csv2("Datos_H-DAP2021.csv")
set.seed(10)
a1 <- sample(dim(base)[1],150)

bd <- data.frame(base[a1,])
names(bd) <- c("Placa","DAP_cm","Altura_m","Habito","Codigos_observaciones")
row.names(bd) <- 1:length(bd$Placa)
str(bd)
summary(bd)

#---------------------------------

# 1. Análisis exploratorio de los datos

#Relación entre las variables DAP y Altura
x11()
plot(bd$DAP_cm, bd$Altura_m, xlab = "DAP (cm)", ylab = "Altura (m)")
#identify(bd$DAP_cm, bd$Altura_m)

bd[c(18,24,27,48,59,63,105,110,124),]

par(mfrow = c(2,2))
x11()
#Hay valores atípicos en X?

#Cleveland
dotchart(bd$DAP_cm, pch = 16, xlab = "DAP (cm)",
         ylab = "Orden de los datos")
#identify(bd$DAP_cm)


#Hay valores atípicos en Y?
dotchart(bd$Altura_m, pch = 16, xlab = "Altura (m)",
         ylab = "Orden de los datos")
#identify(bd$Altura_m)


# Valores atípicos: li = leverage
hi <- 1/length(bd$DAP_cm) + (bd$DAP_cm - mean(bd$DAP_cm))^2 / sum((bd$DAP_cm - mean(bd$DAP_cm))^2)
summary(hi)

plot(hi, type = "h")
abline(h = 4/length(bd$DAP_cm), lty = 2, col = "red")
#identify(hi)
a <- hi>4/length(bd$DAP_cm)
(a.1 <- bd[a,])


# Si se tienen muchas variables puede se mejor usar una función
leverage <- function(x){
  hi <- 1/length(x)+(x-mean(x))^2/sum((x-mean(x))^2)
  return(hi)
}
hi1 <- leverage(bd$Altura_m)
summary(hi1)

plot(hi, type = "h")
abline(h = 4/length(bd$Altura_m), lty = 2, col = "red")
#identify(bd$DAP_cm, bd$Altura_m)
bd[50,]


par(mfrow = c(1,2))
boxplot(bd$DAP_cm, ylab = "DAP (cm)")
boxplot(bd$Altura_m, ylab = "Altura (m)")

# 2. Ajuste de un modelo lineal simple

m1 <- lm(Altura_m ~ DAP_cm, data = bd)
summary(m1)
anova(m1)
x11()
par(mfrow=c(1,1))
plot(bd$DAP_cm, bd$Altura_m, xlab="DAP (cm)", ylab="Altura (m)", las=1)
abline(m1, col = 2)
mtext(paste0("H = ",round(coef(m1)[1],2), "+", round(coef(m1)[2],2),
             "DAP: R2=", round(summary(m1)$adj.r.squared,2)))

plot(bd$Altura_m, predict(m1), las = 1, xlim = c(5,30), ylim = c(5,30),
     xlab = "Alturas observadas", ylab = "Alturas predichas")
abline(0,1, col = "Red")

plot(residuals(m1))
abline(h=0)

par(mfrow=c(2,2))
plot(m1)

str(m1)
m1$call
summary(m1)[[1]]
coef(m1)
fitted(m1)

predict(m1)
predict(m1,list(DAP_cm=10))
resid(m1)
rstandard(m1)



a2 <- bd[c(-18,-24,-27,-48,-59,-63,-105,-110,-124),]
bd2 <- as.data.frame(a2)

m2 <- lm(Altura_m ~ DAP_cm, data = bd2)
summary(m2)
anova(m2)

par(mfrow=c(1,3))
plot(bd2$DAP_cm, bd2$Altura_m, xlab="DAP (cm)", ylab="Altura (m)", las=1)
abline(m2, col = 2)
mtext(paste0("H = ",round(coef(m2)[1],2), "+", round(coef(m2)[2],2),
             "DAP: R2=", round(summary(m2)$adj.r.squared,2)))

plot(bd2$Altura_m, predict(m2), las = 1, xlim = c(5,30), ylim = c(5,30),
     xlab = "Alturas observadas", ylab = "Alturas predichas")
abline(0,1, col = "Red")

plot(residuals(m2))
abline(h=0)

par(mfrow=c(2,2))
plot(m2)

#------------------------------------------------

# 3. Supuestos

# Normalidad

shapiro.test(residuals(m1))
shapiro.test(residuals(m2))

# Homogeneidad de varianza entre grupos

res<- data.frame(Estimados = fitted(m1), Res.stand = rstandard(m1))

resigrupos<- cut(res$Estimados, fivenum(res$Estimados), include.lowest = 1)

summary(resigrupos)

bartlett.test(res$Res.stand~resigrupos) # Rechazo H0 no hay homogeneidad de varianzas

par(mfrow=c(1,1))
plot(res$Estimados, res$Res.stand)
abline(h = 0, lty =2, col = "red")

res$grupos1<- NA
res$grupos1[res$Estimados<5] <- 1
res$grupos1[res$Estimados>=5 & res$Estimados<15] <- 2
res$grupos1[res$Estimados>=15 & res$Estimados<25] <- 3
res$grupos1[res$Estimados>=25] <- 4
table(res$grupos1)

bartlett.test(res$Res.stand ~ res$grupos1)

library(lmtest)

#Breusch-Pagan test
bptest(m1) # Rechazo H0. No homogeneidad
#Goldfeld-Quandt Test
gqtest(m1) # No rechazo H0. Homogeneidad
#Harrison-McCabe Test
hmctest(m1) # No rechazo H0. Homogeneidad

par(mfrow = c(1,1))
plot(res$Estimados, res$Res.stand, col = res$grupos1, pch = 16)

#----------------------

# 4. Outliers

par(mfrow= c(1,1))
plot(bd$DAP_cm, bd$Altura_m)
abline(m1)

par(mfrow= c(2,2))
plot(m1)

influence.measures(m1)
influence.measures(m1)$is.inf
lm.influence(m1)

outliers <- bd[c(18,24,27,46,48,54,59,63,68,105,110),]

#Posibles errores identificados previamente bd[c(18,24,27,48,59,63,105,110,124),]

#---------------------------

# Recta de regresion con intervalos de confianza

summary(m1)
summary(bd$DAP_cm)

D<- data.frame(DAP_cm = seq(min(bd$DAP_cm), max(bd$DAP_cm)))
ic<- predict(m1, D, interval = "prediction")
head(ic)
x11()
par(mfrow= c(1,1))

plot(bd$DAP_cm, bd$Altura_m, xlab = "DAP (cm)", ylab = "Altura (m)")
abline(m1, col = "red")
lines(D$DAP_cm, ic[,2], lty =2, col = "red")
lines(D$DAP_cm, ic[,3], lty =2, col = "red")

