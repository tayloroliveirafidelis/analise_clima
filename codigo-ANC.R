## ================================================
## 2a Etapa Tratamento de dados (Clima) de Varginha
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
write.csv(dad_diario_med1,"dados_brutos/dados_med_horaria.csv")

dados_vga = read.csv("dados_brutos/vga-data-02.csv")

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
dados_temp <- subset(dados_temp,!is.na(dados_temp$tempo))

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
# vendo series
ggplot(data=dados_temp) + geom_line(aes(tempo, TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.))


ggplot(data=dados_temp) + geom_line(aes(tempo, VELOCIDADE_VENTO))
ggplot(data=dados_temp) + geom_line(aes(tempo, UMIDADE_RELATIVA))
# arrumando banco para análise
temp <- dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.
Umid <- as.numeric(dados_temp$UMIDADE_RELATIVA)
Velo <- as.numeric(dados_temp$VELOCIDADE_VENTO)
mes <- as.numeric(dados_temp$mes)
dia <- yday(dados_temp$tempo)
ano <- as.numeric(dados_temp$ano)
hora <- as.numeric(dados_temp$hora)
tempo <-dados_temp$tempo
dad <- data.frame(temp,Umid,Velo,mes,dia,ano,hora,tempo)

dad[["data"]] <- ""
for(i in 1:length(dad$temp)){
  dad$data[i]<-as.character(str_split(dad$tempo[i]," ")[[1]][1])
}

dad_diario <- split(dad,dad$data)

Data <- as.character()
Temp <- as.numeric()
Ano<- as.character()
Mes<- as.character()
Dia<- as.character()
for(i in 1:length(dad_diario)){
  Data[i]<- dad_diario[[i]]$data[1]
  Temp[i] <-mean(as.numeric(dad_diario[[i]]$temp))
  Ano[i]<- dad_diario[[i]]$ano[1]
  Mes[i]<- dad_diario[[i]]$mes[1]
  Dia[i]<- dad_diario[[i]]$dia[1]
}
dad_diario_med <- data.frame(Data,Temp,Ano,Mes,Dia)

#==========================================================
#Análise exploratoria dos dados
summary(dad_diario_med)
attach(dad_diario_med )
var(Temp)
sd(Temp)

#===============================================================================
#box plot anual
Mes1 <-dad_diario_med$Mes  
dad_diario_med$Mes <- factor(dad_diario_med$Mes, ordered = TRUE, levels = c(1:12))


####===================Analise descritiva==================================
#No Grafico-1  temos a serie original da temperatura do ar bulbo seco horario
#é visivel a variabilidade dos dados em torno dos mess assim para fins de sinplificação na modelagem , 
#optamos por trabalhar com a media dos dados diarios ,assim resultando em uma nova serie como pode ser
#apresentada no Grafico-2 
summary(dados_vga$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.)
te <- dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.
plot(sin(te),type="l")
summary(dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.)
sd(dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.)
#GRAFICO-1
jpeg("figuras/serie_historica_original.jpeg")
ggplot(data = dados_temp, aes(x = tempo, y = TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.)) + 
  geom_line()+ 
  xlab("TEMPO") +
  ylab("TEMPERATURA DO AR BULBO SECO HORÁRIA(C°)")
dev.off()
#GRAFICO-2
jpeg("figuras/serie_historica_media_diaria.jpeg")
ggplot(data = dad_diario_med, aes(x = as.Date(Data), y = Temp)) + 
  geom_line()+ 
  xlab("TEMPO") +
  ylab("TEMPERATURA DO AR BULBO SECO MÉDIA HORÁRIA(C°)")
dev.off()
summary(dad_diario_med$Temp)
sd(dad_diario_med$Temp)

#segue na tabela1o resumo estatistico dos dados originais e da nova serie gerada
#RESUMO DAS SERIES 

serie_1<- summary(dados_temp$TEMPERATURA.DO.AR.BULBO.SECO..HORARIA.C.)
serie_2<- summary(Temp)
tabela_1 <- rbind(serie_1,serie_2)
print(tabela_1 )


#para evidenciar a sazonalidade aparente  no Grafico-2 é apresentado no grafico_3 os boxplots da 
#temperatura em relação aos meses.
jpeg("figuras/boxplot.jpeg")
p<-ggplot(dad_diario_med, aes(x=Mes, y=Temp, color=Mes)) +
  geom_boxplot()+ 
  xlab("Mês") +
  ylab("TEMPERATURA DO AR BULBO SECO MÉDIA HORÁRIA(C°)")
print(p)
dev.off()
# Assim fica evidente a existencia  de um comportamento sazonal ao decorrer dos meses, o que revela tambem 
#um comportamento ciclico ao decorrer dos anos.

#================================================================================

MannKendall(dad_diario_med$Temp)
# Ajuste de um modelo linear multiplo
dad_diario_med1 <- dad_diario_med
dad_diario_med <-dad_diario_med1[1:4545,] 
attach(dad_diario_med)
#ajuste 1,pvalue =0.53013,R-squared:0.4738,W = 0.99812, p-value = 2.613e-05
ajuste <- lm(Temp~as.numeric(Dia)+(as.numeric(Dia)^2)+as.numeric(Ano)+cos((2*pi*as.numeric(Mes))/12)+sin((2*pi*as.numeric(Mes))/12))
#ajuste 2,pvalue =0.28285,R-squared:  0.4833,W = 0.99811, p-value = 2.459e-05
ajuste <- lm(Temp~cos((2*pi*as.numeric(Dia))/365)+sin((2*pi*as.numeric(Dia))/365)+as.numeric(Ano)+cos((2*pi*as.numeric(Mes))/12)+sin((2*pi*as.numeric(Mes))/12))
#ajuste 3,pvalue =0.35673,R-squared:   0.33,W = 0.99754, p-value = 1.018e-06
ajuste <- lm(Temp~as.numeric((cos((2*pi*as.numeric(Dia))/365)+sin((2*pi*as.numeric(Dia))/365))*(1/pi))+as.numeric(Ano)+as.numeric((cos((2*pi*as.numeric(Mes))/365)+sin((2*pi*as.numeric(Mes))/365))*(1/pi)))
#ajuste 4,pvalue =0.00035107,R-squared:  0.4847,W = 0.99766, p-value = 2.005e-06
ajuste <- lm(Temp~as.numeric(Dia)^2+cos((2*pi*as.numeric(Dia))/365)+sin((2*pi*as.numeric(Mes))/12))
#ajuste 5,pvalue =0.00066197,R-squared:  0.481,W = 0.99791, p-value = 7.751e-06
ajuste <- lm(Temp~cos((2*pi*as.numeric(Dia))/365)+sin((2*pi*as.numeric(Dia))/365)+sin((2*pi*as.numeric(Mes))/12))
#ajuste 6***pvalue =0.23658,R-squared:  0.4825,W = 0.99809, p-value = 2.206e-05
ajuste <- lm(Temp~cos((2*pi*as.numeric(Dia))/365)+as.numeric(Ano))

#Teste F da Significâcnia da Regressão e os Testes t individuais,
x <-ajuste$residuals
summary(ajuste)
MannKendall(x)
shapiro.test(x)
qqPlot(x, dist='norm',envelope=.95)
var(x)
ajuste$coefficients
library(e1071) #Comando library para se carregar pacotes; e1071 necessário
ks.test(x, "pnorm", 0,4)
skewness(x,type=1) #Comando skewness para se calcular a assimetria


#-----------------------------------------------------------------------------
library(nortest)
#===========================================================================
#------------------------------------------------
#
x1 <- ajuste$residuals
MannKendall(x1)

summary(x1)
sd(x1)
# existe tendencia pois P-valor menor que 5%, 
# assim para tirar a tendencia deve realizar a diferença dos valores reais com os valores 
#estimados do modelo de regressão e conferirassim temos:

dad_diario_med[["residuos_ajuste1"]] <- x
jpeg("figuras/Grafico_Residuos_regressao.jpeg")


ggplot(data = dad_diario_med, aes(x=as.Date(Data), y =ajuste$residuals )) + 
  geom_line()+ 
  xlab("TEMPO") +
  ylab("Resíduos")+col=
  dev.off()


par(mfrow=c(1,1))
plot(as.Date(dad_diario_med$Data),dad_diario_med$Temp,type = "l",
     main='',
     xlab='Ano', ylab='TEMPERATURA DO AR BULBO SECO MÉDIA HORÁRIA(C°)',
     col='blue',
     bty='l')
par(new=TRUE)
plot(ajuste$fitted.values,type = "l",
     axes=F, ann=F,
     col='red',lty=2)
legend('topleft',
       c('Temperatura original', 'Temperatura Estimada'),
       col=c('blue', 'red'), lty=1:2,
       bty='n')
grid(col='darkgrey')





#como o p-valor foi maior que 5% a tendencia foi retirada 
#segundo o teste de Mann Kendall.assim podemos seguir para análise da série temporal 

#=================================================
# Estimando e removendo o efeito da sazonalidade
# Validando a hipótese de sazonalidade
y_sem_trend_org<-x
# =================================================
library("forecast")
library("lmtest")
library("nortest")
hist(x)
par(mfrow=c(1,2))
Acf(y_sem_trend_org,main="FAC")
Pacf(y_sem_trend_org,main="FACP")
#O FAC não apresenta decaimento progresivo e o 
#FACP um pico no primeiro lag, indicativos 
#de um processo autorregressivo de ordem 1. 
#O decaimento no FAC, no entanto, é lento, 
#o que sugere não estacionariedade. 
#O aparecimento de autocorrelações significativas 
#por volta dos lags 12, 24 e 36 sugerem sazonalidade.
#================================================
dif1 <- diff(x,dif=1)
length(dif1)

plot(dif1,type="l")
par(mfrow=c(1,2))
Acf(dif1,lag=48,main="FAC série diferenciada1 ")
Pacf(dif1,lag=48,main="FACP série diferenciada1")

auto.arima(dif1[1:4098],d = 1,max.p = 5,max.q = 5)
Best_model<- arima(dif1,c(3,1,2))
summary(Best_model)

Box.test(Best_model$residuals, type="Ljung-Box",)
Acf(Best_model$residuals,lag=48,main="FAC Resíduos ARMA(3,1,2)")
Pacf(Best_model$residuals,lag=48,main="FACP Resíduos ARMA(3,1,2)")

#ar1 significativo
coeftest(Best_model)


# este comportamento não pode acontecer , assim tem algo errado na 
# modelagem da tendencia 

#================================================================

#fazendo a previsão dos residuos
autoplot(previsao_final)






dad_diario_med$Temp[4098:4555]

dad_diario_med$Temp[1:4098]

#457
componente_serie<- forecast(Best_model,h = 10)
media<- as.data.frame(componente_serie[[4]])
pre<- as.data.frame(componente_serie[[5]])
lower <- as.data.frame(componente_serie[[6]])
componente_s <- cbind(media,upper)
componente_s <- cbind(componente_s,lower)


componente_regressao<- as.data.frame(predict(ajuste$fitted.values))

PRE <-cbind(componente_s,componente_regressao)
componente_serie$mean


`Point Forecast` <- PRE$x+PRE$`Point Forecast`
`Lo 95` <- lower$`95%`+componente_regressao$`Lo 95`
`Hi 95` <- upper$`95%`+componente_regressao$`Hi 95`

previsao_final <-data.frame(`Point Forecast`,`Lo 95`,`Hi 95`)

previsao_final$dado_real <-dad_diario_med1$Temp[4546:4555] 

previsao_final$data <-c(4546:4555)
previsao_final$EQM <-(previsao_final$Point.Forecast-previsao_final$dado_real)^2

EQM <- sum(previsao_final$EQM)/10
#avprevisao_finalaliação preditiva do modelo

dad1
dad1 <- dad_diario_med1[3500:4555,1:2]
dad1$prev <- " "
dad1$prev[1046:1056] <- as.numeric(previsao_final$Point.Forecast)
dad1$Lo <- " "
dad1$Lo[1046:1056] <- as.numeric(previsao_final$Lo.95)
dad1$Hi <- " "
dad1$Hi[1046:1056] <- as.numeric(previsao_final$Hi.95)

dad2$prev <- ""
dad2$prev[1046:1056] <-dad2$dad2[1046:1056] 
dad1$dados[1046:1056] <-dad1$Temp[1046:1056] 
dad1$Temp[1046:1056] <- ""

dad1<- dad1[21:31,]

par(mfrow=c(1,1))
plot(as.Date(dad1$Data),dad1$dados,type = "l",    
     main='',
     xlab='Dia', ylab='TEMPERATURA DO AR BULBO SECO MÉDIA HORÁRIA(C°)',
     col='black',
     bty='l')

par(new=TRUE)
plot(dad1$prev,type = "l",
     axes=F, ann=F,
     col='green')
par(new=TRUE)

legend('topleft',
       c('Dados original', 'Dados Estimados'),
       col=c('Black','green'), lty=1:2,
       bty='n')
grid(col='darkgrey')







#erro quadratico medio de 1.1224





checkresiduals(Best_model)










# previsão da serie
Best_model



#o modelo






#Após a dferenciação para a sazonalidade, os correlogramas parecem indicar um processo autorregressivo de ordem 1.
#Vamos propor e testar os seguintes modelos:
#  * modelo1: dados1 ~ ARMA (1,1,0)(1,1,0)
#  * modelo2: dados1 ~ ARMA (1,0,0)(0,1,1)1

ts.plot(x, Best_model$residuals,
        ylab='', xlab='Tempo',
        main='',
        col=c('blue', 'red'),
        lwd=2)
grid(col='darkgrey', lwd=2)
legend('bottom',c('Passeio aleatório','Ruído branco'),
       lwd=2, col=c('blue','red'), bty='n')










modelo1 <- Arima(y_sem_trend_org,order=c(1,0,0),seasonal=list(order=c(1,1,0),period=12),include.constant=F)
modelo1
coeftest(modelo1)
tsdiag(modelo1)
tsdisplay(residuals(modelo1))
hist(scale(modelo1$residuals),main="Histograma dos resíduos",xlab="Resíduos padronizados")
qqnorm(modelo1$residuals)
#==========================
modelo2 <- Arima(y_sem_trend_org,order=c(1,0,0),seasonal=list(order=c(0,1,1),period=12),include.constant=F)
modelo2
coeftest(modelo2)
tsdiag(modelo2)
tsdisplay(residuals(modelo2))
Box.test (modelo2$residuals, lag = 48, fitdf=4, type = "Ljung")
hist(scale(modelo2$residuals),main="Histograma dos resíduos",xlab="Resíduos padronizados")
qqnorm(modelo2$residuals)




## aplicando um metodo de steepwise para compor um modelo que apresente somente as
## variáveis que estão influenciando significativamente na explicação da
## variável resposta pode ser acessado através da função step() do R.
library(car)
vif(ajuste)
# autores do livro Mixed Effects Models and Extensions in Ecolgy with R (2009) o valor de VIF para remoção da variável é de 3 a 5 
#(valor usado nos exemplos do livro VIF = 3). 
#como nenhuma variavel apresentou vif maior que três não é nescessario tirar nenhuma
#ligo não temos problemas de colinearidade entre as variaveis

#Método Stepwise
step(ajuste, direction = "both")
#Método backward
step(ajuste, direction = "backward")
#Método forward
step(ajuste, direction = "forward")
#   a temperatura é nossa variavel dependente e Umid+Velo+mes+dia+ano+hora
#   são as variaveis independentes, e ao aplicar o resumo do modelo é possível 
#   ver que a variavel que a variavel dia não foi significativo assim deve ser retirada 
#   do modelo e tambem ao ser significativo em relação as variaveis Mes ano e hora
#   apresenta a existencia de sazonalidade da série.
#==================================================================================
#Análise de Variância
anova(ajuste)
#intervalo de confiança
confint(ajuste)
#===================================================================================
#Para avaliar as suposições de que os erros possuem variância constante e são não correlacionados entre
#si, é necessaria a construção dos graficos de “Resíduos versus Valores Ajustados da Variável Resposta” e “Resíduos versus
#Valores da Variável Explicativa"

plot(ajuste, which = 1)
plot(ajuste, which = 2)

#observa-se a violação da suposição de homocedasticidade dos erros. Para corroborar esse
#resultado, pode-se dividir o conjunto de dados em duas partes, utilizando a mediana por exemplo, e
#realizar um teste para comparar as variâncias de cada subconjunto:

median(temp)

var.test(residuals(ajuste)[temp>=19.9],residuals(ajuste)
         [temp<19.9])
#Observe que o Valor P do teste  é menor que os níveis de significância mais usuais
#(0,01; 0,05; 0,10). Portanto, conclui-se que a variância dos dois subconjuntos não
#é igual, o que implica
#a heterocedasticidade dos erros
#==========================================================================
#  avaliar a suposição de normalidade dos erros
plot(ajuste, which = 2)

#observa-se a violação da suposição de que os erros 
#aleatórios têm distribuição Normal.para confirmar 
#fiz dois testes o do Shapiro-Wilk com 5000 amostras dados seu 
#limite de teste e para comparar realizei o teste de Anderson-Darling
#que realiza o teste para quantidades grandes de informações 

# Install required packages:
install.packages('nortest')

#Model data tho use
ModelData = ajuste$residuals
#Do shapiro test with only the first 5000 records
shapiro.test(ModelData[0:5000])$p.value
#Anderson-Darling normality test
library(nortest)
ad.test(ModelData)$p.value

#Portanto, como o Valor P do teste é pequeno, rejeita-se a hipótese de normalidade dos resíduos e, por
#consequência, conclui-se que os erros não são normalmente distribuídos





#==========================================================================================================