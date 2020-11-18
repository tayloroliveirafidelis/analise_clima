## ================================================
## ANALISE DESCRITIVA (Clima) de Varginha
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
#==================================================

#ABRINDO BASE DE DADOS
setwd("C:/Users/Rodrigo/Desktop/Análise clima/")
#DADOS HORARIOS
dados_horarios <- read.csv("Dados_horario.csv")
#DADOS DIARIOS(MÉDIA HORARIA)
dados_diario_med <-read.csv("Dados_diario_med.csv") 

#Análise exploratoria dos dados
summary(dados_horarios)
summary(dados_diario_med)
# desvio padrão
attach(dados_horarios)
sd(temp)
sd(Umid)
sd(Velo)
attach(dados_diario_med)
sd(velocidade_vento)
sd(Temp)
sd(Umidade)
#=====================================================================================
####===================Analise descritiva=============================================
#temperatura horario
#GRAFICO-1 serie historica
jpeg("figuras/serie_historica_media_diaria.jpeg")
ggplot(data =  dados_horarios, aes(x = as.Date(Data), y = temp)) + 
  geom_line()+ 
  xlab("TEMPO") +
  ylab("TEMPERATURA DO AR BULBO SECO MÉDIA HORÁRIA(C°)")
dev.off()
#grafico-2-Boxplot
jpeg("figuras/boxplot.jpeg")
p<-ggplot(dados_horarios, aes(x=as.character(mes), y=temp, color=as.character(mes))) +
  geom_boxplot()+ 
  xlab("Mês") +
  ylab("TEMPERATURA DO AR BULBO SECO MÉDIA HORÁRIA(C°)")
print(p)
dev.off()
#======================================================================================
#velocidade vento horario

#GRAFICO-1 serie historica
jpeg("figuras/serie_historica_media_diaria.jpeg")
ggplot(data =  dados_horarios, aes(x = as.Date(Data), y = Velo)) + 
  geom_line()+ 
  xlab("TEMPO") +
  ylab("VELOCIDADE DO VENTO m/s)")
dev.off()
#grafico-2-Boxplot
jpeg("figuras/boxplot.jpeg")
p<-ggplot(dados_horarios, aes(x=as.character(mes), y=Velo, color=as.character(mes))) +
  geom_boxplot()+ 
  xlab("Mês") +
  ylab("VELOCIDADE DO VENTO m/s")
print(p)
dev.off()
#======================================================================================
#Umidade horario
#GRAFICO-1 serie historica
jpeg("figuras/serie_historica_media_diaria.jpeg")
ggplot(data =  dados_horarios, aes(x = as.Date(Data), y = Umid)) + 
  geom_line()+ 
  xlab("TEMPO") +
  ylab("UMIDADE DO RELATIVA DO AR(g/m³)")
dev.off()
#grafico-2-Boxplot
jpeg("figuras/boxplot.jpeg")
p<-ggplot(dados_horarios, aes(x=as.character(mes), y=Umid, color=as.character(mes))) +
  geom_boxplot()+ 
  xlab("Mês") +
  ylab("UMIDADE DO RELATIVA DO AR(g/m³)")
print(p)
dev.off()
#======================================================================================
