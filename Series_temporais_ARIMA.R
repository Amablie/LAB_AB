library(astsa) # para analisar séries temporais
library(tsibble)  # para trabalhar com séries temporais
library(tidyverse) ### para manipulação e tratamento dos dados
#install.packages("forecast")
library(forecast)
library(zoo)



#### BASE  DE DADOS
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

mm<-rollmean(x =data_new_escala$m_demand, k = 3, fill = NA, align = "right")
plot(y= data_new_escala$m_demand,x = data_new_escala$data, type = "l")
lines(mm, col="red", lty=1)
grid()

################### AUTOCORRELAÇÃO #########################

acf(data_new_escala$m_demand)
acf2(data_new_escala$m_demand)

#################### plotando ajuste do modelo ############################

lag1.plot(data_new_escala$m_demand,5)
fit<- lm(data_new_escala$m_demand ~ data_new_escala$data, na.action = NULL)
summary(fit)

#################

## retifica??o e diferen?a
par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(data_new_escala$m_demand-fitted(fit),x= data_new_escala$data, xlab="Tempo", ylab="Série do resto", pch=19, col="skyblue3", type = "l")
grid()
plot(diff(data_new_escala$m_demand,1), xlab="Tempo", ylab="Série diferenciada", pch=19, col="skyblue3", type = "l")
grid()

##########

sarima(data_new_escala$m_demand,0,1,0)
sarima(data_new_escala$m_demand,0,1,1)


auto.arima(data_new_escala$m_demand)
############ 
