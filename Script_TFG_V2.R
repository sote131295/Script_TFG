rm(list=ls())
cat("\014")

#vamos a cambiar de directorio de trabajo
setwd("C:/Users/MSI/Documents/TFG")
source('C:/Users/MSI/Documents/TFG/ur.test.R')

library(urca)
library(zoo)
library(tseries)
library(readxl)
library(normtest)
library(DistributionUtils)
library(forecast)
library(ggplot2)
library(ggfortify)
library(scales)
library(strucchange)
library(faraway)
library(lmtest)

#vamos a importar series y nombrarlas
series = read_excel("Selecció20.xlsx", sheet = "Todas las Series")

lleg = series$TOT

#Vamos a eliminar los valores faltantes
lleg = na.omit(lleg)
lleg = ts(lleg, start = c(1999,1), end = c(2018,11), frequency = 12)
str(lleg)
d.lleg = diff(log(lleg))

#plot de nº de llegadas de turistas
autoplot(lleg, ts.colour="black")+
  scale_y_continuous(breaks=seq(0,3500000,500000))+
  scale_x_date(date_breaks = "2 years", labels = date_format("%Y"))+
  labs(x="tiempo",y="nº de llegadas")+
  theme(axis.title.x = element_text(size=rel(1.5)))+
  theme(axis.title.y = element_text(size=rel(1.5)))+
  theme(panel.background = element_rect(fill ="white"))+
  theme_test()

############################################################  
# contrastes de cambio estructural
############################################################
bp_lleg = breakpoints(lleg~1)
summary(bp_lleg)# esto debería darte los puntos y suintérvalo de confianza 
ic_lleg = confint(bp_lleg)# se guarda los intervalos de confianza

#gráfico del cambio estructural seleccionado por el contraste de Bai y Perron
plot(lleg,type = "1", xlim=c(1999,2019))
axis(1, at=2000:2018, labels = 2000:2018);axis(2);box()
lines(lleg, col=9)
lines(bp_lleg)
lines(ic_lleg)
title(xlab = "años", ylab = "nº de llegadas")


nchow = length(lleg)
Trend = c(1:nchow)
sctest(lleg~Trend, type="Chow", point=196)

#####################################################################
# calculamos estadisticos descriptivos de la serie
#####################################################################
summary(lleg)
sd(lleg)
skewness(lleg)

kurtosis(lleg)#contraste del 3er momento si es mayor que tres significa que la cola es muy pesada


# gráfico de la curva de densidad serie de nº de llegadas
ggplot(series, aes(x=TOT))+
  geom_density(alpha =.2, fill="blue")+
  labs(x="llegadas de turistas",y="distribución")+
  theme(axis.title.x = element_text(size=rel(3)),axis.text.x = element_text(size = rel(2)))+
  theme(axis.title.y = element_text(size=rel(3)),axis.text.y = element_text(size = rel(2)))+
  theme(panel.background = element_rect(fill ="white"))+
  scale_x_continuous(breaks=seq(0,3500000,500000))

# pvalor casi 0 rechazamos hipotesis nula de la serie es normal
jb.norm.test(lleg, nrepl = 3000) #

# para la diferencia de llegadas 
D.lleg=series$DIF
D.lleg = na.omit(D.lleg)
D.lleg = ts(D.lleg, start = c(1999,2), end = c(2018,11), frequency = 12)

# gráfico de la curva de densidad serie de la diferencia del nº de llegadas
ggplot(series, aes(x=DIF))+
  geom_density(alpha =.3, fill="red")+
  labs(x=" variación de llegadas de turistas",y="distribución")+
  theme(axis.title.x = element_text(size=rel(3)))+
  theme(axis.title.y = element_text(size=rel(3)))+
  theme(panel.background = element_rect(fill ="white"))+
  scale_x_continuous(breaks=seq(-1500000, 1500000,500000))


summary(D.lleg)
sd(D.lleg)
skewness(D.lleg)

kurtosis(D.lleg)
jb.norm.test(D.lleg, nrepl = 3000)

# para la diferencia del logaritmo de llegadas

sd(diff(log(lleg)))
mean(diff(log(lleg)))
skewness(diff(log(lleg)))
kurtosis(diff(log(lleg)))
summary(d.lleg)

# gráfico de la curva de densidad serie de la diferencia del logaritmo del  nº de llegadas
ggplot(series, aes(x=DL))+
  geom_density(alpha =.3, fill="red")+
  labs(x="diferencia logarítmica de llegadas",y="densidad")+
  theme(axis.title.x = element_text(size=rel(3)), axis.text.x = element_text(size = rel(2)))+
  theme(axis.title.y = element_text(size=rel(3)), axis.text.y =element_text(size = rel(2)))+
  theme(panel.background = element_rect(fill ="white"))+
  scale_x_continuous(breaks=seq(-2,1, 0.5))+
  xlim(-2,2)

#####################################################################
# Vamos a hacer contrastes de raíces unitarias
#####################################################################

#ADF
lleg.df = ur.df(y = log(lleg), type = "trend", lags = 24, selectlags = "BIC")
summary(lleg.df)
#la nula hay raices unitarias= serie no es estacionaria

adf.test(lleg)

adf.test(d.lleg)

d.lleg.df = ur.df(y = diff(log(lleg)), type = "trend", lags = 24, selectlags = "BIC")

#ZA
#vamos a seleccionar el retardo optimo

bic.test<- matrix(NA, 12,1)
for (i in 1:12) {
  za<-ur.za(y=diff(log(lleg)), model = 'both', lag = i)
  bic.test[i]=BIC(eval(attributes(za)$testreg))
}

which(bic.test==min(bic.test))

lleg.za<-ur.za(y=log(lleg), model = 'both', lag = 12)
summary(lleg.za)
#no rechazamos Ho presenta raices unitariaas -3.67<-5.08 valor absoluto al 5%
d.lleg.za<-ur.za(y=diff(log(lleg)), model = 'both', lag = 11)
summary(d.lleg.za)
# rechazamos Ho, no presenta raices unitariaas -8.63>-5.08 valor absoluto al 5%

#####################################################################
# vamos a crear vables binarias estacionarias
#####################################################################
meses = seasonaldummy(lleg)
meses

d.lleg = diff(log(lleg))
meses =seasonaldummy(d.lleg)
summary(lm(d.lleg ~meses))
M1=lm(d.lleg~meses)

#####################################################################
# vamos a descomponer la serie mediante holt-winters
#####################################################################

# decompese descmpone la serie en una tendencia, estacionalidad, componente irregular
mod = decompose(lleg)
mod
mod$figure
plot(mod)

# vamos a hacer lo mismo pero con la descomposioción multiplicativa

mod2 = decompose(lleg, "multiplicative")
mod2
mod2$figure
plot(mod2)



# descomponer en tablas 
# vamos a hacer un gráfico de la serie original con el valor predicho
plot(lleg, type="1", xlim=c(1999,2019))
axis(1, at=2000:2018, labels=2000:2018);axis(2);box()
lines(lleg, col=9)
lines(seasadj(mod), col = 4)
lines(seasadj(mod2), col = 10)
legend("topleft",col=c(9, 4, 10),
       legend =c("serie original","descomposición aditiva","descomposición multiplicativa"),
       lwd=3, bty = "n")
title(xlab = "años", ylab = "nº de llegadas")




# hacemos la descomposición exponencial HW Aditivo
mod.hw = hw(lleg)
mod.hw

# gráfico con la predicción de holt-winters aditivo
autoplot(mod.hw, ts.colour="black")+
  scale_y_continuous(breaks=seq(0,3500000,500000))+
  labs(x="tiempo",y="nº visitantes")+
  theme(axis.title.x = element_text(size=rel(1.5)))+
  theme(axis.title.y = element_text(size=rel(1.5)))+
  theme(panel.background = element_rect(fill ="white"))+
  scale_x_date(date_breaks = "2 years", labels = date_format("%Y"))+
  theme_test()

mod.hw$fitted

#Predicción fuera de la muestra (h = 11 períodos)
n = length(lleg)
hh = 11
lleg.2 = lleg[1:(n-hh)]
lleg.2 = ts(lleg.2, start = c(1999,1), end = c(2017,12), frequency = 12)
pred.f.hw = hw(lleg.2, h = hh)
plot(pred.f.hw)
plot(lleg)
lines(pred.f.hw$mean, col=2)

error.hw = pred.f.hw$mean - lleg[(n-hh+1):n]
rmse.hw = sqrt(mean(error.hw^2))

# hacemos la descomposición exponencial HW Multiplicativo
mod.hw2 = hw(lleg, seasonal = "multiplicative")
mod.hw2

autoplot(mod.hw2, ts.colour="black")+
  scale_y_continuous(breaks=seq(0,10000000,500000))+
  labs(x="año",y="nº visitantes")+
  theme(axis.title.x = element_text(size=rel(1.5)))+
  theme(axis.title.y = element_text(size=rel(1.5)))+
  theme(panel.background = element_rect(fill ="white"))+
  scale_x_date(date_breaks = "2 years", labels = date_format("%Y"))+
  theme_test()


mod.hw2$fitted


plot(lleg, type="1", xlim=c(1999,2019))
axis(1, at=2000:2018, labels=2000:2018);axis(2);box()
lines(lleg, col=9)
lines(seasadj(mod), col = 4)
lines(seasadj(mod2), col = 10)
lines(mod.hw$fitted, col = 6)
lines(mod.hw2$fitted, col = 11)
legend("topleft",col=c(9, 4, 10, 6, 11),legend =c("serie original", "datos sin el componente estacional de la descomposición aditiva","datos sin el componente estacional de la descomposición multiplicativa", "valores ajustados descomposición H-W aditivo", "valores ajustados descomposición H-W multiplicativa"),
       lwd=3, bty = "n")
title(xlab = "tiempo", ylab = "nº de llegadas")

#Predicción fuera de la muestra (h = 11 períodos)
n = length(lleg)
hh = 11
lleg.2 = lleg[1:(n-hh)]
lleg.2 = ts(lleg.2, start = c(1999,1), end = c(2017,12), frequency = 12)
pred.f.hw2 = hw(lleg.2, h = hh, seasonal = "multiplicative")
plot(pred.f.hw2)
plot(lleg)
lines(pred.f.hw2$mean, col=2)

error.hw2 = pred.f.hw2$mean - lleg[(n-hh+1):n]
rmse.hw2 = sqrt(mean(error.hw2^2))



#####################################################################
# Arima - SARIMA (seasonality)
#####################################################################
d.lleg = diff(log(lleg))

acf(d.lleg)#cada 6 periodos hay estacionalidad
d.lleg = ts(d.lleg, start = c(1999,2), end = c(2018,11), frequency = 12)

pacf(d.lleg) # sugiere poner un ar1( se pone retardos hasta que desaparecen los otro)

par(mfrow=c(1,2))
acf(d.lleg)
pacf(d.lleg)
par(mfrow=c(1,1))

fit1=Arima(d.lleg, order=c(1,0,1))
summary(fit1)
coeftest(fit1)

acf(resid(fit1))

# SARIMA
fit2=arima(d.lleg, order=c(1,0,12), seasonal=list(order=c(0,1,1), period=12))
summary(fit2)
coeftest(fit2)

par(mfrow=c(1,2))
acf(resid(fit2))
pacf(resid(fit2))
par(mfrow=c(1,1))

#gráfico de predicción del SARIMA
fit2 %>% forecast %>% 
  autoplot+
  labs(x="años",y="variación visitantes relativa")+
  theme(axis.title.x = element_text(size=rel(1.5)))+
  theme(axis.title.y = element_text(size=rel(1.5)))+
  theme(panel.background = element_rect(fill ="white"))+
  scale_x_date(date_breaks = "2 years", labels = date_format("%Y"))+
  theme_test()


#Predicción fuera de la muestra (h = 11 períodos)

fit2=Arima(window(d.lleg, end= c(2017,12)), order=c(1,0,12), seasonal=list(order=c(0,1,1), period=12),
           lambda =NULL)
accuracy(forecast(fit2, h = hh, lambda = NULL), window(d.lleg, start= c(2018,1))) 
pred.f.fit2 = forecast(fit2, h = hh, lambda = NULL)$mean
predicciones_2019<-forecast(fit2,h=13)$mean


diciembre_arima18=exp(series$LOG[239]+predicciones_2019[1])
enero_arima19 = exp(log(diciembre_arima18)+(predicciones_2019[2]))
febrero_arima19 = exp(log(enero_arima19)+(predicciones_2019[3]))
marzo_arima19 = exp(log(febrero_arima19)+(predicciones_2019[4]))
abril_arima19=exp(log(marzo_arima19)+(predicciones_2019[5]))
mayo_arima19=exp(log(abril_arima19)+(predicciones_2019[6]))
junio_arima19=exp(log(mayo_arima19)+(predicciones_2019[7]))
julio_arima19=exp(log(junio_arima19)+(predicciones_2019[8]))
agosto_arima19=exp(log(julio_arima19)+(predicciones_2019[9]))
septiembre_arima19=exp(log(agosto_arima19)+(predicciones_2019[10]))
octubre_arima19=exp(log(septiembre_arima19)+(predicciones_2019[11]))
noviembre_arima19=exp(log(octubre_arima19)+(predicciones_2019[12]))
diciembre_arima19=exp(log(noviembre_arima19)+predicciones_2019[13])

pr<-c(enero_arima19,febrero_arima19,marzo_arima19, abril_arima19, mayo_arima19, junio_arima19,
      julio_arima19, agosto_arima19, septiembre_arima19, octubre_arima19, noviembre_arima19,diciembre_arima19)
table(pr)

enero_arima = exp(series$LOG[228]+(pred.f.fit2[1]))
febrero_arima = exp(log(enero_arima)+(pred.f.fit2[2]))
marzo_arima = exp(log(febrero_arima)+(pred.f.fit2[3]))
abril_arima=exp(log(marzo_arima)+(pred.f.fit2[4]))
mayo_arima=exp(log(abril_arima)+(pred.f.fit2[5]))
junio_arima=exp(log(mayo_arima)+(pred.f.fit2[6]))
julio_arima=exp(log(junio_arima)+(pred.f.fit2[7]))
agosto_arima=exp(log(julio_arima)+(pred.f.fit2[8]))
septiembre_arima=exp(log(agosto_arima)+(pred.f.fit2[9]))
octubre_arima=exp(log(septiembre_arima)+(pred.f.fit2[10]))
noviembre_arima=exp(log(octubre_arima)+(pred.f.fit2[11]))


matriz_prediccionesArima =  matrix(nrow = 11, ncol = 1, c(enero_arima,febrero_arima,marzo_arima,
                                                          abril_arima,mayo_arima,junio_arima,julio_arima,
                                                          agosto_arima,septiembre_arima,octubre_arima,
                                                          noviembre_arima))
pred.f.fit2.ent = ts(matriz_prediccionesArima, start = c(2018,1), frequency=12)

############################################################
#Gráficos fuera de la muestra ARIMA
############################################################

plot(lleg, type="1", xlim=c(1999,2019), ylim=c(0,3600000))
axis(1, at=2000:2018, labels=2000:2018);axis(2);box()
lines(lleg, col=9)
lines(pred.f.fit2.ent, col='red')
legend("topleft",col=c(9, 10),legend =c("serie original", "predicción fuera de la muestra"),
       lwd=3, bty = "n")
title(xlab = "tiempo", ylab = "nº de llegadas")


plot(d.lleg, type="1", xlim=c(1999,2019), ylim=c(-2,1))
axis(1, at=2000:2018, labels=2000:2018);axis(2);box()
lines(d.lleg, col=9)
lines(pred.f.fit2, col='red')
legend("topleft",col=c(9, 10),legend =c("serie original", "predicción fuera de la muestra"),
       lwd=3, bty = "n")
title(xlab = "tiempo", ylab = "variación relativa visitantes")


error.ar = pred.f.fit2.ent-lleg[(n-hh+1):n]
rmse.ar = sqrt(mean(error.ar^2))

########Gráfico predicción fuera de la muestra H-W2 y SARIMA############################################################

plot(lleg, type="1", xlim=c(1999,2019), ylim=c(0,5300000))
axis(1, at=2000:2018, labels=2000:2018);axis(2, at=c(1500000,3000000,4500000));box()
lines(lleg, col=9)
lines(pred.f.hw2$mean, col='red')
lines(pred.f.hw$mean, col=4)
lines(pred.f.fit2.ent, col=3)
legend("topleft",col=c(9, 10, 4, 3),legend =c("serie original", "predicción fuera de la muestra H-W Multiplicativo",
                                              "predicción fuera de la muestra H-W Aditivo","predicción fuera de la muestra del modelo SARIMA" ),
       lwd=3, bty = "n")
title(xlab = "tiempo", ylab = "nº de llegadas")

#############lm con diflog.lleg##############

lm_est<-lm(series$DL~series$X__1)
summary(lm_est)
M2=lm_est

anova(M1,M2)

AIC(M1, M2)
BIC(M1,M2)
logLik(M1)
logLik(M2)
