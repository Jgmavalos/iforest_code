dt<-read.csv2("Datos_P2.csv")
str(dt)
colnames(dt)<-c("No","DAP","H")

set.seed(150)
w<- sample(1:358,251)
range(dt$DAP)

#Datos para la determinacion del modelo
dt1<- dt[w,]
range(dt$DAP)

modc<-lm(H~DAP+I(DAP^2),dt1)
mode2<-lm(log(H)~log(DAP)+I((log(DAP))^2),dt1)
menten<-with(dt1, nls(H~SSmicmen(DAP,a,b)))

shapiro.test(residuals(modc))
shapiro.test(residuals(mode2))
shapiro.test(residuals(menten))

#RSE para determinar mejor modelo
#modelo lineal cuadratico
rc<-residuals(modc)
RSEc<-sqrt(sum(rc^2, na.rm = TRUE)/summary(modc)$df[2])
#modelo expoinencial cuadratico
mode2
MSEe2<-anova(mode2)$"Mean Sq"[2]/2
b0e2<-0.34282 + MSEe2
b0e2 
FCe2<-exp(anova(mode2)$"Mean Sq"[2]/2)
He2<- exp(predict(mode2))*FCe2
RSEe2<- with(dt1, sqrt(sum(((He2-H)^2)/summary(mode2)$df[2])))
#modelo Michaelis-Menten
Hmenten<- predict(menten)
resid.men <- dt1$H- Hmenten
RSEmenten <- sqrt(sum(resid.men^2, na.rm = TRUE)/summary(menten)$df[2])
#RSE
RSEc
RSEe2
RSEmenten

#Mejor modelo Michaelis-Menten
menten
with(dt1, plot(DAP,H))
mx<-seq(5,118,0.1)
my.2.2<-53.383 *mx/(30.096+mx)
lines(mx,my.2.2, col="blue")
#Validacion 
#error en base al archivo de validacion
dtv<-dt[-w,]
range(dtv$DAP)
Hmm<-53.383*dtv$DAP/(30.096+dtv$DAP)
errormentenv<-100*((Hmm-dtv$H)/dtv$H)
mean(errormentenv)
sd(errormentenv)

