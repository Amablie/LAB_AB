library(astsa) # para analisar séries temporais
library(tsibble)  # para trabalhar com séries temporais
library(tidyverse) ### para manipulação e tratamento dos dados
#install.packages("forecast")
library(forecast)
library(zoo)



#### BASE  DE DADOS ####################################################
library(readxl)
dados_mensais <- read_excel("dados_mensais.xlsx")
head(dados_mensais)
str(dados_mensais)
summary(dados_mensais)


########### NOVA ESCALA

data_new_escala <-
  dados_mensais %>% 
  transform(m_demand = m_demand/1000,
            m_supply_data  = m_supply_data/1000,
            m_mean_unit_price = m_mean_unit_price/1000,
            m_sales = m_sales/1000,
            m_cost_sms = m_cost_sms/1000,
            m_cost_newspapers = m_cost_newspapers/1000,
            m_cost_radio = m_cost_radio/1000,
            m_cost_tv  = m_cost_tv/1000,
            m_cost_internet  = m_cost_internet/1000,
            m_CPI = m_CPI/1000,
            m_CCI  = m_CCI/1000,
            m_PPI  = m_PPI/1000)


str(data_new_escala)


summary(data_new_escala)


str(data_new_escala)
summary(data_new_escala)


data_ts_scale<-ts(data_new_escala, frequency=12, start=c(2010,1))
str(data_ts_scale)
head(data_ts_scale)
str(data_ts_scale)

############################## ARIMA PARA DEMANDA ####################################################

##### ANALISE DE TENDENCIA

ts_demanda<-ts(data_new_escala$m_demand, frequency=12, start=c(2010,1))
str(ts_demanda)
head(ts_demanda)

dadossazonais<- decompose(ts_demanda)
plot(dadossazonais)




######################## MÉDIAS MÓVEIS #########################################

autoplot(ts(data_new_escala$m_demand)) +
  autolayer(ma(data_new_escala$m_demand,7), series="5-MA") +
  xlab("Year") + ylab("Demand") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))

################### AUTOCORRELAÇÃO #########################

acf(data_new_escala$m_demand)
acf2(data_new_escala$m_demand)

#################### plotando ajuste do modelo ############################

lag1.plot(data_new_escala$m_demand,5)
fit<- lm(data_new_escala$m_demand ~ data_new_escala$data, na.action = NULL)
summary(fit)

###lag 1 para autoregressão

#################

## retifica??o e diferen?a
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
sarima(data_new_escala$m_demand,0,1,1)


auto.arima(data_new_escala$m_demand)




#############################################################################

################ MODELO ARIMA COM AS COVARIAVEIS ############################

#############################################################################

macrocov <- data_new_escala[, c(11:13)]
midiacov <- data_new_escala[, c(3:10)]
# head(midiacov)

# par(mfrow = c(3,1))
# plot(y=macrocov$m_CPI, x=data_new_escala$data, type = "l")
# plot(y=macrocov$m_PPI, x=data_new_escala$data, type = "l")
# plot(y=macrocov$m_CCI, x=data_new_escala$data, type = "l")
# dev.off()


cov <- data_new_escala[,c(3:13)]
str(cov)
cov<-as.matrix(cov) 

### precisa transformar em matriz antes de colocar no modelo

autoplot(ts(midiacov))

autoplot(ts(macrocov))

plot.ts(midiacov)
###### Aqui podemos ver que as varivaeis de custo são estacionárias e apresentam
##### comportamento ciclico, quanto as macroeconomicas e de vendas possue
##### tendencia e sazonalidade, logo são não estacionárias




######## DECOMPOSIÇÃO DAS VARIAVEIS DE MIDIA ########################
ts_cost<-ts(data_new_escala$m_cost_tv, frequency=12, start=c(2010,1))
str(ts_cost)
head(ts_cost)

dsazonais<- decompose(ts_cost)
plot(dsazonais)

######################################################################




arima(data_new_escala$m_demand, xreg = cov, order = c(0,1,1))
sarima(data_new_escala$m_demand, xreg = cov, 0,1,1)

#### fazemos a verificação com os argumentos do modelo apenas para demanda
#### Apesar de ter funcionado para a demanda não é um bom modelo para as
#### covariaveis, pois apresenta um valor abaixo do p-valor 




##### ANALISANDO MÉDIA MÓVEL ###########

autoplot(ts(data_new_escala$m_cost_radio)) +
  autolayer(ma(data_new_escala$m_CPI,2), series="5-MA") +
  xlab("Year") + ylab("Demand") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))



#### A estacionaridade tem que ficar na linha do zero, com o acrescimo da média
#### móvel em midia, podemos suavizar a linha e aproximar de zero
#########################################################



fit1<- arima(data_new_escala$m_demand, xreg = cov, order = c(1,0,1))

checkresiduals(fit1)

sarima(data_new_escala$m_demand, xreg = cov, 1,0,1)

fit2 <- arima(data_new_escala$m_demand, xreg = cov, order = c(2,0,2))

checkresiduals(fit2)

sarima(data_new_escala$m_demand, xreg = cov, 2,0,2)


fit3 <- arima(data_new_escala$m_demand, xreg = cov, order = c(3,0,2))

checkresiduals(fit3)

sarima(data_new_escala$m_demand, xreg = cov, 3,0,2)

fit4 <- arima(data_new_escala$m_demand, xreg = cov, order = c(4,0,2))

checkresiduals(fit4)
#### entender a saída
sarima(data_new_escala$m_demand, xreg = cov, 4,0,2)
#### ENTENDER A SAÍDA DO SARIMA

AIC(fit3)  #### melhor modelo ajustado
AIC(fit4)


sarima(data_new_escala$m_demand, xreg = cov, 3,0,2)  ####  



