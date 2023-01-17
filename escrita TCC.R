
# PACOTES -----------------------------------------------------------------

library(astsa) # para analisar séries temporais
library(tsibble)  # para trabalhar com séries temporais
library(tidyverse) ### para manipulação e tratamento dos dados
library(forecast)
library(zoo)
library(readxl)
library(ggfortify)
library(zoo)
library(magrittr)
library(GGally)
library(corrplot)
library(gridExtra)

### BASE DE DADOS --------------------------------------------------------------------
MMM_data <- read_excel("MMM_data_excel.xlsx", 
                       col_types = c("text", "date", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric","numeric"))


##------------------------------------------------------------------------------------

head(MMM_data)
summary(MMM_data)
str(MMM_data)


# data_new_escala <-
#   MMM_data %>% 
#   transform(DEMAND = DEMAND/1000,
#             CPI  = CPI/1000,
#             CCI = CCI/1000,
#             PPI = PPI/1000,
#             m_cost_sms = m_cost_sms/1000,
#             m_cost_newspapers = m_cost_newspapers/1000,
#             m_cost_radio = m_cost_radio/1000,
#             m_cost_tv  = m_cost_tv/1000,
#             m_cost_internet  = m_cost_internet/1000,
#             m_CPI = m_CPI/1000,
#             m_CCI  = m_CCI/1000,
#             m_PPI  = m_PPI/1000)


# ANALISE EXPLORATÓRIO ----------------------------------------------------



MMM_nonnumeric <- MMM_data[,-c(1,9)] ### apenas valores númericos
tb <- MMM_nonnumeric %>% 
  gather(key = "variable", value = "value", -DATE)

str(tb)

ggplot(tb,
       mapping = aes( y= value, x = DATE)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  geom_line() 

MMM_cost <- MMM_data[, -c(1,4,5,6,15,16,17,18,19)] # sem os dados de GRP
ggpairs(MMM_cost)


## RESPOSTA ------------------------------------------

Sales <- ggplot(MMM_data, aes(x = DATE, y = SALES)) +
  geom_line()
supply <- ggplot(MMM_data, aes(x = DATE, y = Supply_Data)) +
  geom_line()
unit <- ggplot(MMM_data, aes(x = DATE, y = Unit_Price)) +
  geom_line()
demand <- ggplot(MMM_data, aes(x = DATE, y = DEMAND)) +
  geom_line()

grid.arrange(Sales, supply, unit, demand,  nrow = 4)


## MACROECONOMICA -----------------------------------

PPI <-ggplot(MMM_data, aes(x = DATE, y = PPI)) +
  geom_line()

CCI <- ggplot(MMM_data, aes(x = DATE, y = CCI)) +
  geom_line()

CPI <- ggplot(MMM_data, aes(x = DATE, y = CPI)) +
  geom_line()

grid.arrange(PPI, CCI, CPI,  nrow = 3)


# Como é possível observar as varipaveis estão com agregações diferentes, as variáveis macroeconomicas 
# estão no formato mês e as demais estão diárias.
# 
# Optamos em transformar a base para o formato **mês** devido ao comportamente mensal das variaveis 
# macroeconomicas observadas,combinado a ideia de que a série traz muitos ruídos em suas variaveis, talvez
# a agregação pudesse ajudar a suavizar o comportamento das outras variáveis e reduzir o ruído.


dia <- ggplot(MMM_data, aes(x = DATE, y = DEMAND)) +
  geom_line()
# semana <- ggplot(teste, aes(x = indice_semana, y = weekly_demand)) +
#   geom_line()
mes <- ggplot(dados_mensais, aes(x = data, y = m_demand)) +
  geom_line()
grid.arrange(dia, mes,  nrow = 2)


# Aqui podemos ver o efeito da agregação diante da variável resposta escolhida para a modelagem
# no caso a variável Demanda.


#  VARIAVEIS DE CUSTO -----------------------------------------------------

##Investimento em canais
mes_SMS <- ggplot(dados_mensais, aes(x = data, y = m_cost_sms)) +
  geom_line()

mes_TV <- ggplot(dados_mensais, aes(x = data, y = m_cost_tv)) +
  geom_line()

mes_NEWS <- ggplot(dados_mensais, aes(x = data, y = m_cost_newspapers)) +
  geom_line()

mes_RADIO <- ggplot(dados_mensais, aes(x = data, y = m_cost_radio)) +
  geom_line()

mes_INTERNET <- ggplot(dados_mensais, aes(x = data, y = m_cost_internet)) +
  geom_line()

grid.arrange(mes_SMS, mes_TV, mes_NEWS, mes_RADIO, mes_INTERNET,  nrow = 5)


# Analisando os dados de investimento nos canais de midia da empresa em questão, notamos um 
# comportamente semelhante ao de ruído branco na nossa base de dados, chamando nossa atenção
# para como são feitas as distribuições de custo ao longo dos anos para essa empresa.

# Olhando para a série temporal vemos um comportamento mais estacionário quando olhando para 
# os dados de custo



# ACF E CCF ---------------------------------------------------------------

# ACF ---

# custo
acf2(dados_mes$m_cost_sms)
acf2(dados_mes$m_cost_tv) 
acf2(dados_mes$m_cost_newspapers)
acf2(dados_mes$m_cost_radio)
acf2(dados_mes$m_cost_internet)

# Fazendo a analise da autocorrelação podemos ver um comportamento sazonal nos dados
# quando analisamos o grafico ACF no lag 4

# Quando olhamos para o lag 2 do PACF vemos que existe uma forte autocorrelação ali sendo 
# indicativo para o AR


# venda
acf2(dados_mes$m_demand)
acf2(dados_mes$m_sales)
acf2(dados_mes$m_supply_data)

## diferença vendas
acf2(diff(dados_mes$m_demand))
acf2(diff(dados_mes$m_sales))
acf2(diff(dados_mes$m_supply_data))

# Olhando os dados, podemos ver que talvez seja necessário realizar uma diferança para tornar
# os dados estacionários


# macroeconomica
acf2(dados_mes$m_PPI)
acf2(dados_mes$m_CCI)
acf2(dados_mes$m_CPI)

## diferença
acf2(diff(dados_mes$m_PPI))
acf2(diff(dados_mes$m_CCI)) # lag 2
acf2(diff(dados_mes$m_CPI)) # lag 3

# Olhando os dados, podemos ver que talvez seja necessário realizar uma diferança para tornar
# os dados estacionários




# CCF ---

ccf2(dados_mes$m_CPI, dados_mes$m_demand)
ccf2(dados_mes$m_supply_data, dados_mes$m_demand)
ccf2(dados_mes$m_cost_sms, dados_mes$m_demand)
ccf2(dados_mes$m_cost_newspapers, dados_mes$m_demand)
ccf2(dados_mes$m_cost_tv, dados_mes$m_demand)



# DECOMPOSIÇÃO ------------------------------------------------------------

## --------- PARA VARIAVEL RESPOSTA 

ts_demanda<-ts(dados_mes$m_demand, frequency=12, start=c(2010,1))
str(ts_demanda)
head(ts_demanda)

dadossazonais<- decompose(ts_demanda)
plot(dadossazonais)

# Aqui podemos ver um comportamento de tendencia e sazonalidade na nossa 
# variavel resposta



## --------- VERIFICAR PARA OUTRAS VARIÁVEIS 


#####   trocar variavel para verificar decomposição
dados_mes_ts<-ts(dados_mes$m_cost_internet, frequency=12, start=c(2010,1))
str(dados_mes_ts)
head(dados_mes_ts)

sazidata<- decompose(dados_mes_ts)
plot(sazidata)



# ARIMA PARA DEMANDA ------------------------------------------------------

############## MÉDIAS MÓVEIS #########################################

autoplot(ts(data_new_escala$m_demand)) +
  autolayer(ma(data_new_escala$m_demand,7), series="5-MA") +
  xlab("Year") + ylab("Demand") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

########### AUTOCORRELAÇÃO #########################

acf(data_new_escala$m_demand)
acf2(data_new_escala$m_demand)

#################### plotando ajuste do modelo ############################

lag1.plot(data_new_escala$m_demand,5)
fit<- lm(data_new_escala$m_demand ~ data_new_escala$data, na.action = NULL)
summary(fit)

# lag 1 para autoregressão
# Problema com a diferença, transformar os dados diff() para os dados ficarem estacionários

#################

## retifição e diferença
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(data_new_escala$m_demand-fitted(fit),x= data_new_escala$data, xlab="Tempo", ylab="Série do resto", pch=19, col="skyblue3", type = "l")
grid()
plot(diff(data_new_escala$m_demand,1), xlab="Tempo", ylab="Série diferenciada", pch=19, col="skyblue3", type = "l")
grid()

### AQUI PODEMOS VER QUE COM A DIFERENÇA A SERIE PASSA A SER ESTACIONÁRIA
### TALVEZ SEJA UMA DAS ABORDAGENS UTILIZADAS NA NOSSA ANÁLISE

########## correlação cruzada para a diferença

acf2(data_new_escala$m_demand-fitted(fit))


acf2(diff(data_new_escala$m_demand))

#########

sarima(data_new_escala$m_demand,0,1,0)
sarima(data_new_escala$m_demand,0,1,1) #### melhor modelo
sarima(data_new_escala$m_demand,1,1,0)

auto.arima(data_new_escala$m_demand)



#  ANALISE ARIMA COM COVARIAVEIS ------------------------------------------

dim(data_new_escala)
str(data_new_escala)
num_data <- data_new_escala[,c(-1,-16,-17)]
str(num_data)

## separação de treino e teste
train <-  num_data[1:80, ]
test <- num_data[-c(1:80), ]

dim(train)
dim(test)


str(train)
# head(midiacov)
head(train)



head(test)
# par(mfrow = c(3,1))
# plot(y=macrocov$m_CPI, x=data_new_escala$data, type = "l")
# plot(y=macrocov$m_PPI, x=data_new_escala$data, type = "l")
# plot(y=macrocov$m_CCI, x=data_new_escala$data, type = "l")
# dev.off()


macrocov <- train[, c(10:12)]
vendacov <- train[, c(2:4)]
midiacov <- train[, c(5:9)]

autoplot(ts(midiacov))
autoplot(ts(vendacov))
autoplot(ts(macrocov))
plot.ts(midiacov)

###### Aqui podemos ver que as varivaeis de custo são estacionárias e apresentam
##### comportamento ciclico, quanto as macroeconomicas e de vendas possue
##### tendencia e sazonalidade, logo são não estacionárias


cov <- train[,c(2:12)]
str(cov)
cov<-as.matrix(cov) ### precisa transformar em matriz antes de colocar no modelo


######## DECOMPOSIÇÃO DAS VARIAVEIS DE MIDIA ########################
ts_cost<-ts(train$m_cost_tv, frequency=12, start=c(2010,1))
str(ts_cost)
head(ts_cost)

dsazonais<- decompose(ts_cost)
plot(dsazonais)

######################################################################

arima(train$m_demand, xreg = cov, order = c(0,1,1))
sarima(train$m_demand, xreg = cov, 0,1,1)

#### fazemos a verificação com os argumentos do modelo apenas para demanda
#### Apesar de ter funcionado para a demanda não é um bom modelo para as
#### covariaveis, pois apresenta um valor abaixo do p-valor 




##### ANALISANDO MÉDIA MÓVEL ###########

autoplot(ts(data_new_escala$m_cost_radio)) +
  autolayer(ma(data_new_escala$m_cost_radio,2), series="2-MA") +
  xlab("Year") + ylab("Demand") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))



#### A estacionaridade tem que ficar na linha do zero, com o acrescimo da média
#### móvel em midia, podemos suavizar a linha e aproximar de zero
#########################################################



fit1<- arima(train$m_demand, xreg = cov, order = c(1,0,1))

checkresiduals(fit1)

sarima(train$m_demand, xreg = cov, 1,0,1)

fit2 <- arima(train$m_demand, xreg = cov, order = c(2,0,1))

checkresiduals(fit2)

sarima(train$m_demand, xreg = cov, 2,0,1)


fit3 <- arima(train$m_demand, xreg = cov, order = c(3,0,1))

checkresiduals(fit3)

sarima(train$m_demand, xreg = cov, 3,0,1)

fit4 <- arima(train$m_demand, xreg = cov, order = c(4,0,1))

checkresiduals(fit4)
#### entender a saída
sarima(train$m_demand, xreg = cov, 4,0,1)
#### ENTENDER A SAÍDA DO SARIMA

fit5 <- arima(train$m_demand, xreg = cov, order = c(3,0,2))

checkresiduals(fit5)

sarima(train$m_demand, xreg = cov, 3,0,2)

fit6 <- arima(train$m_demand, xreg = cov, order = c(5,0,1))

checkresiduals(fit6)

sarima(train$m_demand, xreg = cov, 5,0,1)

fit7 <- arima(train$m_demand, xreg = cov, order = c(0,0,1))

checkresiduals(fit7)

sarima(train$m_demand, xreg = cov, 0,0,1)

AIC(fit3)  #### melhor modelo ajustado
AIC(fit4)
AIC(fit5)
AIC(fit6)
AIC(fit7)

sarima(train$m_demand, xreg = cov, 3,0,2)  ####  
auto.arima(train$m_demand, xreg = cov)

##### PREDIÇÃO

cov_test <- test[,c(2:12)]
str(cov_test)
cov_test<-as.matrix(cov_test)


dev.off()
sarima.for(train$m_demand,
           xreg = cov, 
           newxreg = cov_test[c(1:6),], 6,
           3,0,2)


test$m_demand


ggplot(test, aes(x = data , y = m_demand)) +
  geom_line()
