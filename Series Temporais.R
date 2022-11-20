## https://towardsdatascience.com/building-a-marketing-mix-model-in-r-3a7004d21239

#### Tutorial de aplicação de series temporais para MMM

library(astsa) # para analisar séries temporais
library(tsibble)  # para trabalhar com séries temporais
library(tidyverse) ### para manipulação e tratamento dos dados
install.packages("forecast")
library(forecast)


### foi decidido que a abordagem seria mensal, por conta dos dados macro economicos e do ruído dos dados
##### BASE DE DADOS MENSAL ####################################


library(readxl)
dados_mensais <- read_excel("dados_mensais.xlsx")

str(dados_mensais)
summary(dados_mensais)

##############################################################


library(lubridate) #incerindo indice para observações do mês
dados_mensais$ind_mes<-seq.int(nrow(dados_mensais))
dados_mensais

### variaveis resposta
ggplot(dados_mensais, aes(x = ind_mes, y = m_demand)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_sales)) +
  geom_line()


hist(dados_mensais$m_demand)
hist(dados_mensais$m_cost_sms)
hist(dados_mensais$m_cost_internet)
hist(dados_mensais$m_cost_radio)
hist(dados_mensais$m_cost_tv)

dados_mensais %>% 
  mutate(mes = as.numeric(somes))
str(dados_mensais)


#facet
dados_mensais %>% 
  ggplot(aes(x = somes, y =m_demand, color = factor(soano)))+
  geom_line()

###########################################################################

auto.arima(dados_mensais$m_demand)
