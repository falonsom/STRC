library(vars)
multiN<-Dat_AN[,c("Casos","Hospitalizados","UCI")][26:ndias,]
multiN<-diff(ts(log(multiN)))
EQ1<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#summary(EQ1)
#rcasos<-EQ1$varresult$Casos$residuals
#rHosp<-EQ1$varresult$Hospitalizados$residuals
#rUCI<-EQ1$varresult$UCI$residuals
#
#  horizonte de predicción
#
hpred<-7
library(forecast)
ppp<-forecast(EQ1,hpred)
#
# Primera predicción
#
ForeCasos<-exp(ppp$forecast$Casos$mean[1])*Dat_AN$Casos[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeCasos<-rbind(ForeCasos,exp(ppp$forecast$Casos$mean[I])*ForeCasos[I-1])
}

#
#  Para la predicción de los Hospitalizados y UCI usamos los datos desde el 24
#
multiN<-Dat_AN[,c("Casos","Hospitalizados","UCI")][24:ndias,]
multiN<-diff(ts(log(multiN)))
EQ2<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#summary(EQ2)
pppN<-forecast(EQ2,hpred)
#
# Primera predicción
#
ForeHospi<-exp(pppN$forecast$Hospitalizados$mean[1])*Dat_AN$Hospitalizados[ndias]
ForeUCI<-exp(pppN$forecast$UCI$mean[1])*Dat_AN$UCI[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeHospi<-rbind(ForeHospi,exp(pppN$forecast$Hospitalizados$mean[I])*ForeHospi[I-1])
   ForeUCI<-rbind(ForeUCI,exp(pppN$forecast$UCI$mean[I])*ForeUCI[I-1])
}
#
#
#  Lo mismo para Madrid
#
#
multiN<-Dat_MD[,c("Casos","Hospitalizados","UCI")][23:ndias,]
multiN<-diff(ts(log(multiN)))
EQ1<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#summary(EQ1)
#rcasos<-EQ1$varresult$Casos$residuals
#rHosp<-EQ1$varresult$Hospitalizados$residuals
#rUCI<-EQ1$varresult$UCI$residuals
#
#  horizonte de predicción
#
hpred<-7
library(forecast)
ppp<-forecast(EQ1,hpred)
#
# Primera predicción
#
ForeCasos<-exp(ppp$forecast$Casos$mean[1])*Dat_MD$Casos[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeCasos<-rbind(ForeCasos,exp(ppp$forecast$Casos$mean[I])*ForeCasos[I-1])
}

#
#  Para la predicción de los Hospitalizados y UCI usamos los datos desde el 24
#
multiN<-Dat_MD[,c("Casos","Hospitalizados","UCI")][22:ndias,]
multiN<-diff(ts(log(multiN)))
EQ2<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#summary(EQ2)
pppN<-forecast(EQ2,hpred)
#
# Primera predicción
#
ForeHospi<-exp(pppN$forecast$Hospitalizados$mean[1])*Dat_AN$Hospitalizados[ndias]
ForeUCI<-exp(pppN$forecast$UCI$mean[1])*Dat_AN$UCI[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeHospi<-rbind(ForeHospi,exp(pppN$forecast$Hospitalizados$mean[I])*ForeHospi[I-1])
   ForeUCI<-rbind(ForeUCI,exp(pppN$forecast$UCI$mean[I])*ForeUCI[I-1])
}
#
#
#  Lo mismo para Cataluña
#
#
multiN<-Dat_CT[,c("Casos","Hospitalizados","UCI")][33:ndias,]
multiN<-diff(ts(log(multiN)))
EQ1<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#summary(EQ1)
#rcasos<-EQ1$varresult$Casos$residuals
#rHosp<-EQ1$varresult$Hospitalizados$residuals
#rUCI<-EQ1$varresult$UCI$residuals
#
#  horizonte de predicción
#
hpred<-7
library(forecast)
ppp<-forecast(EQ1,hpred)
#
# Primera predicción
#
ForeCasos<-exp(ppp$forecast$Casos$mean[1])*Dat_CT$Casos[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeCasos<-rbind(ForeCasos,exp(ppp$forecast$Casos$mean[I])*ForeCasos[I-1])
}

#
#  Para la predicción de los Hospitalizados y UCI usamos los datos desde el 24
#
multiN<-Dat_MD[,c("Casos","Hospitalizados","UCI")][33:ndias,]
multiN<-diff(ts(log(multiN)))
EQ2<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#summary(EQ2)
pppN<-forecast(EQ2,hpred)
#
# Primera predicción
#
ForeHospi<-exp(pppN$forecast$Hospitalizados$mean[1])*Dat_AN$Hospitalizados[ndias]
ForeUCI<-exp(pppN$forecast$UCI$mean[1])*Dat_AN$UCI[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeHospi<-rbind(ForeHospi,exp(pppN$forecast$Hospitalizados$mean[I])*ForeHospi[I-1])
   ForeUCI<-rbind(ForeUCI,exp(pppN$forecast$UCI$mean[I])*ForeUCI[I-1])
}