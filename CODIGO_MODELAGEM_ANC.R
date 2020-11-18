library(forecast)
library(lmtest)
library(nortest)
library(e1071) #Comando library para se carregar pacotes; e1071 necessário
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


# Ajuste de um modelo linear multiplo
dad_diario_med <- dados_diario_med
attach(dados_diario_med)
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
ks.test(x, "pnorm", 0,4)
skewness(x,type=1) #Comando skewness para se calcular a assimetria
#--------------------------------------------------------------
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
Best_
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



#Model data tho use
ModelData = ajuste$residuals
#Do shapiro test with only the first 5000 records
shapiro.test(ModelData[0:5000])$p.value
#Anderson-Darling normality test
library(nortest)
ad.test(ModelData)$p.value

#Portanto, como o Valor P do teste é pequeno, rejeita-se a hipótese de normalidade dos resíduos e, por
#consequência, conclui-se que os erros não são normalmente distribuídos

