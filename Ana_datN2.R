rm(list = ls())
#setwd("D:/trab/Covid19/")
library(vars)
library(forecast)
#
#  https://datos.gob.es/es/catalogo/e05070101-evolucion-de-enfermedad-por-el-coronavirus-covid-19
#
#  salvar los datos csv en Dat_CA
#
library(readr)
serie_historica_acumulados <- read.csv("serie_historica_acumulados.csv")
Dat_CA<-(serie_historica_acumulados[,1:7])
#Dat_CA[Dat_CA$FECHA=='16/4/2020',]-Dat_CA[Dat_CA$FECHA=='15/4/2020',]
#
#  Tamaño del fichero en DD
#
DD<-dim(Dat_CA)

#
#  Elimina la última fila que son comentarios, recalcula la dimensión (revisar en cada fichero de entrada)
#
AA<-DD[1]-0
Dat_CA<-Dat_CA[1:AA,1:DD[2]]
DD<-dim(Dat_CA)
#
#   Reemplazo los valores perdidos "missing" por 0
#
Dat_CA[is.na(Dat_CA)] = 0
Dat_CA$Fallecidos<-as.integer(Dat_CA$Fallecidos)
#
# Calcula el número de días que tiene el fichero y fija a 19 las comunidades autónomas
#
NCA<-19
ndias<-DD[1]/NCA
#
#  Fecha en los gráficos
#
FechaF<-as.Date(Dat_CA$FECHA[DD[1]],format='%d/%m/%Y')
FechaF<-format(FechaF,"%d %B")
#
# Genero el fichero de datos agrupados para España
#
Dat_ESP<-Dat_CA[1,2:DD[2]]
#
#  Recalculamos el primer día
#
Dat_ESP[1,2]<-sum(Dat_CA$CASOS[1:NCA])
Dat_ESP[1,3]<-sum(Dat_CA$Hospitalizados[1:NCA])
Dat_ESP[1,4]<-sum(Dat_CA$UCI[1:NCA])
Dat_ESP[1,5]<-sum(Dat_CA$Fallecidos[1:NCA])
Dat_ESP[1,6]<-sum(Dat_CA$Recuperados[1:NCA])
Dat_ESP[is.na(Dat_ESP)] = 0
#
#  Calculamos el resto de días
#
for (I in 2:ndias){
INI<-(I-1)*NCA+1
FIN<-I*NCA
Dat_ESP[I,2]<-sum(Dat_CA$CASOS[INI:FIN])
Dat_ESP[I,3]<-sum(Dat_CA$Hospitalizados[INI:FIN])
Dat_ESP[I,4]<-sum(Dat_CA$UCI[INI:FIN])
Dat_ESP[I,5]<-sum(Dat_CA$Fallecidos[INI:FIN])
Dat_ESP[I,6]<-sum(Dat_CA$Recuperados[INI:FIN])
Dat_ESP$FECHA[I]<-Dat_CA$FECHA[FIN]
}
   DDN<-dim(Dat_ESP)
TSCasos<-ts(Dat_ESP$CASOS)
TSHospitalizados<-ts(Dat_ESP$Hospitalizados)
TSUCI<-ts(Dat_ESP$UCI)
TSFallecidos<-ts(Dat_ESP$Fallecidos)
TSRecuperados<-ts(Dat_ESP$Recuperados)
#
#  Selecciona los datos de Andalucía (comunidad 1) Madrid (14)Cataluña (9)
#
NCAN<-1
NCMD<-14
NCCT<-9
Dat_AN<-Dat_CA[NCAN,2:DD[2]]
Dat_MD<-Dat_CA[NCMD,2:DD[2]]
Dat_CT<-Dat_CA[NCCT,2:DD[2]]
for (I in 2:ndias){
   Dat_AN<-rbind(Dat_AN,Dat_CA[(I-1)*NCA+NCAN,2:DD[2]])
   Dat_MD<-rbind(Dat_MD,Dat_CA[(I-1)*NCA+NCMD,2:DD[2]])
   Dat_CT<-rbind(Dat_CT,Dat_CA[(I-1)*NCA+NCCT,2:DD[2]])
}
#
#  Diferencio la serie de logaritmos y almaceno el número de casos en NCDL
#
DLcasos<-diff(log(TSCasos), differences=1)
NCDL<-length(DLcasos)
DLHosp<-diff(log(TSHospitalizados), differences=1)
DLUCI<-diff(log(TSUCI), differences=1)
DLFallecidos<-diff(log(TSFallecidos), differences=1)
#
#
#  Gráficos de los datos observados y de las tasas log(Xt/Xt-1)
#
#

png(filename = "./Graficos/TSCASOS.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(TSCasos,main="Casos contagiados en España", ylab="Casos", xlab="Observación",cex.main=2) 
abline(h=c(50000,100000,150000,200000),col="blue")
dev.off()

png(filename = "./Graficos/TSHOSP.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(TSHospitalizados,main="Hospitalizados en España", ylab="Casos", xlab="Observación",cex.main=2) 
abline(h=c(20000,40000,60000),col="blue")
dev.off()

png(filename = "./Graficos/TSUCI.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(TSUCI,main="Personas en UCI en España", ylab="Casos", xlab="Observación",cex.main=2) 
abline(h=c(2000,4000,6000),col="blue")
dev.off()

png(filename = "./Graficos/DLCASOS.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(DLcasos,main="ln(Casos_{t}/Casos_{t-1}) en España      (nivel 0.05 en rojo)", ylab="Tasa", xlab="Observación",cex.main=2) 
abline(h=c(0.2,0.1),col="blue")
abline(h=c(0.05),col="red")
abline(h=c(0.0),col="green")
dev.off()

png(filename = "./Graficos/DLHOSP.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(DLHosp,main="ln(Hospitalizados_{t}/Hospitalizados_{t-1}) en España      (nivel 0.05 en rojo)", ylab="Tasa", xlab="Observación",cex.main=2) 
abline(h=c(0.2,0.1),col="blue")
abline(h=c(0.05),col="red")
abline(h=c(0.0),col="green")
dev.off()

png(filename = "./Graficos/DLUCI.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(DLUCI,main="ln(UCI_{t}/UCI_{t-1}) en España      (nivel 0.05 en rojo)", ylim=c(-0.025,0.35), ylab="Tasa", xlab="Observación",cex.main=2) 
abline(h=c(0.2,0.1),col="blue")
abline(h=c(0.05),col="red")
abline(h=c(0.0),col="green")
dev.off()


#
#  Predicción modelos VAR
#
NCO<-30
#multiN<-Dat_ESP[,c("CASOS","Hospitalizados","UCI","Fallecidos")][NCO:ndias,]
multiN<-Dat_ESP[,c("CASOS","Hospitalizados","UCI")][NCO:ndias,]
multiN<-diff(ts(log(multiN)))
#EQ1<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#EQ1<-VAR(multiN,p=3,type=c("none"),ic="AIC")
EQ1<-VAR(multiN,p=3,type=c("const"),ic="AIC")
#summary(EQ1)
#rcasos<-EQ1$varresult$Casos$residuals
#rHosp<-EQ1$varresult$Hospitalizados$residuals
#rUCI<-EQ1$varresult$UCI$residuals
#
#  horizonte de predicción
#
hpred<-7
ppp<-forecast(EQ1,hpred)

final<-ndias-NCO+1
plot(ppp$forecast$CASOS,main="Predicción de la tasa de casos en España")
abline(h=c(0.0),col="green")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")

png(filename = "./Graficos/PTCASOS.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(ppp$forecast$CASOS,main="Predicción de la tasa de casos en España")
abline(h=c(0.0),col="green")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")
dev.off()

#
# Para deshacer la transformación Pred=Dato_{t-1}*Exp(Pred_t)
#
# Primera predicción
#
ForeCasos<-exp(ppp$forecast$CASOS$mean[1])*Dat_ESP$CASOS[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeCasos<-rbind(ForeCasos,exp(ppp$forecast$CASOS$mean[I])*ForeCasos[I-1])
}

MPRED<-round(ForeCasos)

m1<-arima(TSCasos,order = c(1,1,0))
PCasos<-forecast(m1,hpred)
for (I in 1:hpred){
  PCasos$mean[I]<-ForeCasos[I]
}
ppp<-forecast(EQ1,hpred)
#
# Primera predicción límites
#
PCasos$lower[1,1]<-exp(ppp$forecast$CASOS$lower[1,1])*Dat_ESP$CASOS[ndias]
PCasos$lower[1,2]<-exp(ppp$forecast$CASOS$lower[1,2])*Dat_ESP$CASOS[ndias]
PCasos$upper[1,1]<-exp(ppp$forecast$CASOS$upper[1,1])*Dat_ESP$CASOS[ndias]
PCasos$upper[1,2]<-exp(ppp$forecast$CASOS$upper[1,2])*Dat_ESP$CASOS[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
PCasos$lower[I,1]<-exp(ppp$forecast$CASOS$lower[I,1])*PCasos$mean[I-1]
PCasos$lower[I,2]<-exp(ppp$forecast$CASOS$lower[I,2])*PCasos$mean[I-1]
PCasos$upper[I,1]<-exp(ppp$forecast$CASOS$upper[I,1])*PCasos$mean[I-1]
PCasos$upper[I,2]<-exp(ppp$forecast$CASOS$upper[I,2])*PCasos$mean[I-1]
}

final<-length(TSCasos)
plot(PCasos,main="Predicción casos en España")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")

png(filename = "./Graficos/PCASOS.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(PCasos,main="Predicción casos en España")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")
dev.off()

#
#  Para la predicción de los Hospitalizados y UCI usamos los datos desde el 30
#
NCO<-30
multiN<-Dat_ESP[,c("CASOS","Hospitalizados","UCI")][NCO:ndias,]
multiN<-diff(ts(log(multiN)))
#EQ2<-VAR(multiN,p=3,type=c("both"),ic="AIC")
EQ2<-VAR(multiN,p=3,type=c("none"),ic="AIC")
#summary(EQ2)
ppp<-forecast(EQ2,hpred)

final<-ndias-NCO+1
plot(ppp$forecast$Hospitalizados,main="Predicción de la tasa de Hospitalizados en España")
abline(h=c(0.0),col="green")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")
plot(ppp$forecast$UCI,main="Predicción de la tasa de UCI en España")
abline(h=c(0.0),col="green")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")

png(filename = "./Graficos/PTHOSP.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(ppp$forecast$Hospitalizados,main="Predicción de la tasa de Hospitalizados en España")
abline(h=c(0.0),col="green")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")
dev.off()

png(filename = "./Graficos/PTUCI.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(ppp$forecast$UCI,main="Predicción de la tasa de UCI en España")
abline(h=c(0.0),col="green")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")
dev.off()



# Incluye puntos points(c(22,23),c(0.02,-0.03),pch=20,cex=0.8,col=10)
#
# Primera predicción
#
ForeHospi<-exp(ppp$forecast$Hospitalizados$mean[1])*Dat_ESP$Hospitalizados[ndias]
ForeUCI<-exp(ppp$forecast$UCI$mean[1])*Dat_ESP$UCI[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeHospi<-rbind(ForeHospi,exp(ppp$forecast$Hospitalizados$mean[I])*ForeHospi[I-1])
   ForeUCI<-rbind(ForeUCI,exp(ppp$forecast$UCI$mean[I])*ForeUCI[I-1])
}

MPRED<-cbind(MPRED,round(ForeHospi),round(ForeUCI))

#
#   Para Hospitalizados
#
m1<-arima(TSHospitalizados,order = c(1,1,0))
m2<-arima(TSUCI,order = c(1,1,0))
PHospi<-forecast(m1,hpred)
PUCI<-forecast(m2,hpred)
for (I in 1:hpred){
  PHospi$mean[I]<-ForeHospi[I]
  PUCI$mean[I]<-ForeUCI[I]
}
#
# Primera predicción límites
#
ppp<-forecast(EQ2,hpred)
PHospi$lower[1,1]<-exp(ppp$forecast$Hospitalizados$lower[1,1])*Dat_ESP$Hospitalizados[ndias]
PHospi$lower[1,2]<-exp(ppp$forecast$Hospitalizados$lower[1,2])*Dat_ESP$Hospitalizados[ndias]
PHospi$upper[1,1]<-exp(ppp$forecast$Hospitalizados$upper[1,1])*Dat_ESP$Hospitalizados[ndias]
PHospi$upper[1,2]<-exp(ppp$forecast$Hospitalizados$upper[1,2])*Dat_ESP$Hospitalizados[ndias]
PUCI$lower[1,1]<-exp(ppp$forecast$UCI$lower[1,1])*Dat_ESP$UCI[ndias]
PUCI$lower[1,2]<-exp(ppp$forecast$UCI$lower[1,2])*Dat_ESP$UCI[ndias]
PUCI$upper[1,1]<-exp(ppp$forecast$UCI$upper[1,1])*Dat_ESP$UCI[ndias]
PUCI$upper[1,2]<-exp(ppp$forecast$UCI$upper[1,2])*Dat_ESP$UCI[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
PHospi$lower[I,1]<-exp(ppp$forecast$Hospitalizados$lower[I,1])*PHospi$mean[I-1]
PHospi$lower[I,2]<-exp(ppp$forecast$Hospitalizados$lower[I,2])*PHospi$mean[I-1]
PHospi$upper[I,1]<-exp(ppp$forecast$Hospitalizados$upper[I,1])*PHospi$mean[I-1]
PHospi$upper[I,2]<-exp(ppp$forecast$Hospitalizados$upper[I,2])*PHospi$mean[I-1]

PUCI$lower[I,1]<-exp(ppp$forecast$UCI$lower[I,1])*PUCI$mean[I-1]
PUCI$lower[I,2]<-exp(ppp$forecast$UCI$lower[I,2])*PUCI$mean[I-1]
PUCI$upper[I,1]<-exp(ppp$forecast$UCI$upper[I,1])*PUCI$mean[I-1]
PUCI$upper[I,2]<-exp(ppp$forecast$UCI$upper[I,2])*PUCI$mean[I-1]
}

final<-length(TSHospitalizados)
plot(PHospi,main="Predicción Hospitalizados en España")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")

png(filename = "./Graficos/PHOSP.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(PHospi,main="Predicción Hospitalizados en España")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")
dev.off()

plot(PUCI,main="Predicción UCI en España")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")

png(filename = "./Graficos/PUCI.png",height = 550, width = 1200, units = "px",bg="white",antialias = "cleartype")
plot(PUCI,main="Predicción UCI en España")
abline(v=final,lty=2,col="magenta")
text(x=final, y=0, labels=FechaF,cex=1,col="magenta")
dev.off()
#
#   Lee la base de datos para posteriormente escribir sobre ella
#
library(readxl)
Predtot <- read_excel("D:/trab/Covid19/Predicciones/FJAM_01_04.xlsx", 
    col_types = c("numeric", "text", "text", 
        "numeric", "numeric", "numeric", 
        "numeric", "numeric"))
fila<-1        
for (I in 1:hpred){ 
    Predtot$uci[fila+(I-1)*20]<-round(ForeUCI[I])
    Predtot$hospitalizados[fila+(I-1)*20]<-round(ForeHospi[I])
    Predtot$confirmados[fila+(I-1)*20]<-round(ForeCasos[I])
    if (I >1) Predtot$nuevos[fila+(I-1)*20] <- round(ForeCasos[I]) - round(ForeCasos[I-1])
}        
Predtot$nuevos[fila]<- round(ForeCasos[1]) - Dat_ESP$CASOS[ndias]
        
library(xlsx)
#write.xlsx(Predtot,"D:/trab/Covid19/Predicciones/FJAM_Pred.xlsx")

#
#   Para Andalucía
#
multiN<-Dat_AN[,c("CASOS","Hospitalizados","UCI")][26:ndias,]
multiN<-diff(ts(log(multiN)))
EQ1<-VAR(multiN,p=3,type=c("none"),ic="AIC")
#EQ1<-VAR(multiN,p=3,type=c("both"),ic="AIC")
#summary(EQ1)
#rcasos<-EQ1$varresult$Casos$residuals
#rHosp<-EQ1$varresult$Hospitalizados$residuals
#rUCI<-EQ1$varresult$UCI$residuals
#
#  horizonte de predicción
#
hpred<-7
ppp<-forecast(EQ1,hpred)
#
# Primera predicción
#
ForeCasos<-exp(ppp$forecast$CASOS$mean[1])*Dat_AN$CASOS[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeCasos<-rbind(ForeCasos,exp(ppp$forecast$CASOS$mean[I])*ForeCasos[I-1])
}
#
#  Para guardar los datos en la hoja Excel
#
fila<-2  

for (I in 1:hpred){ 
    Predtot$confirmados[fila+(I-1)*20]<-round(ForeCasos[I])
    if (I >1) Predtot$nuevos[fila+(I-1)*20] <- round(ForeCasos[I]) - round(ForeCasos[I-1])
}        
Predtot$nuevos[fila]<- round(ForeCasos[1]) - Dat_AN$CASOS[ndias]

#
#  Para Madrid
#
multiN<-Dat_MD[,c("CASOS","Hospitalizados","UCI")][23:ndias,]
multiN<-diff(ts(log(multiN)))
EQ1<-VAR(multiN,p=3,type=c("none"),ic="AIC")
#
#  horizonte de predicción
#
hpred<-7
ppp<-forecast(EQ1,hpred)
#
# Primera predicción
#
ForeCasos<-exp(ppp$forecast$CASOS$mean[1])*Dat_MD$CASOS[ndias]
#
# Predicciones del 2 al hpred
#
for (I in 2:hpred){
   ForeCasos<-rbind(ForeCasos,exp(ppp$forecast$CASOS$mean[I])*ForeCasos[I-1])
}
#
#  Para guardar los datos en la hoja Excel
#
fila<-16  

for (I in 1:hpred){ 
    Predtot$confirmados[fila+(I-1)*20]<-round(ForeCasos[I])
    if (I >1) Predtot$nuevos[fila+(I-1)*20] <- round(ForeCasos[I]) - round(ForeCasos[I-1])
}        
Predtot$nuevos[fila]<- round(ForeCasos[1]) - Dat_MD$CASOS[ndias]

Predtot2<-as.data.frame(Predtot)
write.xlsx(Predtot2,"D:/trab/Covid19/Predicciones/FJAM_Pred.xlsx",row.names = FALSE, showNA = FALSE)
