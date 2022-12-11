#Este código parece ser un script en R que utiliza diferentes técnicas de modelado para analizar un conjunto de datos y determinar cuál de los modelos es el más adecuado para describir los datos. En primer lugar, se cargan los datos del archivo "Datos_P2.csv" en un data frame de R llamado "dt" y se imprime un resumen de los datos utilizando la función "str". Luego se cambian los nombres de las columnas del data frame para reflejar los datos que contienen. Se establece una semilla aleatoria utilizando la función "set.seed" y se selecciona un subconjunto aleatorio de los datos originales utilizando la función "sample".

#Luego se utilizan tres diferentes modelos para ajustar los datos: un modelo lineal cuadrático, un modelo exponencial cuadrático y un modelo de Michaelis-Menten. Para cada uno de estos modelos, se realiza una prueba de Shapiro-Wilk para verificar la normalidad de los residuos. Luego se calcula el error cuadrático medio (MSE) para cada modelo y se utiliza para determinar cuál es el mejor modelo.

#Finalmente, se utiliza el mejor modelo para hacer predicciones sobre un conjunto de datos de validación y se calcula el error medio y la desviación estándar del error.

dt<-read.csv2("Datos_P2.csv")
str(dt)
colnames(dt)<-c("No","DAP","H")
set.seed(150)
w<- sample(1:358,251)
range(dt$DAP)
#Datos para la determinacion del modelo
dt1<- dt[w,]
range(dt1$DAP)

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

