#TRATAMENTO DOS DADOS BRUTOS DAS VARIAVEIS DA ESTAÇÃO METEROLOGICA DE VARGINHA
## ================================================#
#  Tratamento de dados (Clima) de Varginha
## Autor: Taylor Fidels, Deive Oliveira
## data: agotso de 2020
## ================================================
library(randomizeBE)
library(tseries)
library(pspearman)
library(tcltk2)
library(dplyr)
library(randtests)
library(Kendall)
library(lubridate)
library(ggplot2)
library(tidyverse)
theme_set(theme_bw())
#---------------------------------------------------------------------------
#Abrindo banco de dados
setwd("C:/Users/Rodrigo/Desktop/Análise clima/")
dados_vga = read.csv("vga-data-02.csv")

temp = paste(dados_vga$Data," ",dados_vga$Hora_UTC,sep="")
dados_vga$tempo = strptime(temp,format="%Y-%m-%d %H:%M")
tempo <- dados_vga$tempo
TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.<- dados_vga$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.
UMIDADE_RELATIVA <- dados_vga$UMIDADE_RELATIVA_DO_AR_HORARIA
VELOCIDADE_VENTO <- dados_vga$VENTO_VELOCIDADE_HORARIA_m.s.
dados_umidade <- data.frame(tempo,UMIDADE_RELATIVA)
dados_vento <- data.frame(tempo,VELOCIDADE_VENTO )
dados_temp <- data.frame(tempo,TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.,UMIDADE_RELATIVA,VELOCIDADE_VENTO)
dados_umidade <- data.frame(tempo,TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.)
#=====================================================================================================
#tratando dados faltantes
dados_temp["mes"] <- ""
dados_temp["ano"] <- ""
dados_temp["dia"] <- ""
dados_temp["hora"] <- ""
for(i in 1:length(dados_temp$tempo)){
  dados_temp$mes[i] <-month(dados_temp$tempo[i]) 
  dados_temp$ano[i] <-year(dados_temp$tempo[i])
  dados_temp$dia[i] <- day(dados_temp$tempo[i])
  dados_temp$hora[i] <- hour(dados_temp$tempo[i])
  
}
#Retirando datas faltantes
dados_temp <- subset(dados_temp,!is.na(dados_temp$tempo))
#colocando dados faltantes por meio da média dos meses , dias e horas
for(i in 1:length(dados_temp$tempo)){
  if(is.na(dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.[i])){
    dat <- subset(dados_temp,dados_temp$mes==dados_temp$mes[i]&dados_temp$dia==dados_temp$dia[i]&dados_temp$hora==dados_temp$hora[i])
    dat <- subset(dat,!is.na(dat$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.))
    dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.[i]<- mean(dat$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.) 
  }
  if(is.na(dados_temp$UMIDADE_RELATIVA[i])){
    dat <- subset(dados_temp,dados_temp$mes==dados_temp$mes[i]&dados_temp$dia==dados_temp$dia[i]&dados_temp$hora==dados_temp$hora[i])
    dat <- subset(dat,!is.na(dat$UMIDADE_RELATIVA))
    dados_temp$UMIDADE_RELATIVA[i]<- mean(dat$UMIDADE_RELATIVA) 
  }
  if(is.na(dados_temp$VELOCIDADE_VENTO[i])){
    dat <- subset(dados_temp,dados_temp$mes==dados_temp$mes[i]&dados_temp$dia==dados_temp$dia[i]&dados_temp$hora==dados_temp$hora[i])
    dat <- subset(dat,!is.na(dat$VELOCIDADE_VENTO))
    dados_temp$VELOCIDADE_VENTO[i]<- mean(dat$VELOCIDADE_VENTO) 
  }
  
}

# Arrumando banco para análise, e simplificando nomeclaturas 
temp <- dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.
Umid <- as.numeric(dados_temp$UMIDADE_RELATIVA)
Velo <- as.numeric(dados_temp$VELOCIDADE_VENTO)
mes <- as.numeric(dados_temp$mes)
dia <- yday(dados_temp$tempo)
ano <- as.numeric(dados_temp$ano)
hora <- as.numeric(dados_temp$hora)
Data <-dados_temp$tempo
dados_horarios<- data.frame(temp,Umid,Velo,mes,dia,ano,hora,Data)

setwd("C:/Users/Rodrigo/Desktop/Análise clima/")
write.csv(dados_horarios,"Dados_horario.csv")
#==============================================================================================
#construindo nova tabela com a media diaria das variaveis

dad <- dados_horarios
dad[["data"]] <- ""
for(i in 1:length(dad$Data)){
  dad$data[i]<-as.character(str_split(dad$Data[i]," ")[[1]][1])
}

dad_diario <- split(dad,dad$data)
Data <- as.character()
Temp <- as.numeric()
Umidade <- as.numeric()
velocidade_vento <- as.numeric()
Ano<- as.character()
Mes<- as.character()
Dia<- as.character()
for(i in 1:length(dad_diario)){
  Data[i]<- dad_diario[[i]]$data[1]
  Temp[i] <-mean(as.numeric(dad_diario[[i]]$temp))
  Umidade[i] <-mean(as.numeric(dad_diario[[i]]$Umid))
  velocidade_vento[i] <-mean(as.numeric(dad_diario[[i]]$Velo))
  Ano[i]<- dad_diario[[i]]$ano[1]
  Mes[i]<- dad_diario[[i]]$mes[1]
  Dia[i]<- dad_diario[[i]]$dia[1]
}
Dados_diario_med <- data.frame(Data,Temp,Umidade,velocidade_vento,Ano,Mes,Dia)
setwd("C:/Users/Rodrigo/Desktop/Análise clima/")
write.csv(Dados_diario_med,"Dados_diario_med.csv")
#==========================================================================================