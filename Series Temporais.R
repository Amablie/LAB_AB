## https://towardsdatascience.com/building-a-marketing-mix-model-in-r-3a7004d21239

#### Tutorial de aplicação de series temporais para MMM

library(astsa) # para analisar séries temporais
library(tsibble)  # para trabalhar com séries temporais
library(tidyverse) ### para manipulação e tratamento dos dados
#install.packages("forecast")
library(forecast)
library(zoo)

### foi decidido que a abordagem seria mensal, por conta dos dados macro economicos e do ruído dos dados
##### BASE DE DADOS MENSAL ####################################


library(readxl)
dados_mensais <- read_excel("dados_mensais.xlsx")
 head(dados_mensais)
str(dados_mensais)
summary(dados_mensais)


# dados em formato de séries temporais
str(dados_mes)
dados_mes_ts<-ts(dados_mes, frequency=12, start=c(2010,1))
str(dados_mes_ts)
head(dados_mes_ts)

str(dados_mes)

############################################################################

##### ANÁLISE DE REGRESSÃO MULTIPLA











rm1 <- lm(m_demand ~ m_PPI + m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_radio + m_cost_tv + m_cost_internet,
            data = dados_mes)

summary(rm1)

rm2 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_radio + m_cost_tv + m_cost_internet,
          data = dados_mes)

summary(rm2)


rm3 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv + m_cost_internet,
          data = dados_mes)

summary(rm3)

rm4 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv,
          data = dados_mes)

summary(rm4)

rm5 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv,
          data = dados_mes)

summary(rm5)

rm6 <- lm(m_demand ~ m_CPI +m_supply_data + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv,
          data = dados_mes)

summary(rm6)


#install.packages("mctest")
#install.packages("lmtest")





#### Ultimo modelo contem 5 variaveis mais o intercepto que explicam a demanda (variável resposta)

#### Vale comentar que esse não tem as melhores medidas de seleção do modelo,
#### o erro padrão residual aumenta e o R- quadrado dminui nessa análise


library(mctest)
library(lmtest)

imcdiag(rm6, "VIF")

# modelo apresenta multicolinearidade

# -------------------------------------------------------------------------

##############################################################
#### ANÁLISE DE SÉRIES TEMPORAIS

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


str(dados_mes)
dados_mes_ts<-ts(dados_mes, frequency=12, start=c(2010,1))
str(dados_mes_ts)
head(dados_mes_ts)

dadossazonais<- decompose(dados_mes_ts)

season <-dadossazonais$seasonal
head(season)


dadossazonais$


dadossazonais$random
head(dadossazonais$trend)

plot(dadossazonais$seasonal$"x")



###########################################################################

ts1 <- tslm(m_demand ~ season + m_PPI + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers + m_cost_radio 
            + m_cost_tv + m_cost_internet, 
            data = dados_mes_ts)

summary(ts1)




ts2 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers + m_cost_radio 
            + m_cost_tv + m_cost_internet, 
            data = dados_mes_ts)

summary(ts2)


ts3 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers
            + m_cost_tv + m_cost_internet, 
            data = dados_mes_ts)

summary(ts3)

ts4 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers
            + m_cost_tv, 
            data = dados_mes_ts)

summary(ts4)


ts5 <- tslm(m_demand ~ season  + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers
            + m_cost_tv, 
            data = dados_mes_ts)

summary(ts5)

ts6 <- tslm(m_demand ~ season  + m_CPI + m_sales + m_supply_data +m_cost_sms
            + m_cost_newspapers + m_cost_tv, 
            data = dados_mes_ts)

summary(ts6)

ts7 <- tslm(m_demand ~ season  + m_CPI + m_supply_data +m_cost_sms
            + m_cost_newspapers + m_cost_tv, 
            data = dados_mes_ts)

summary(ts7)



#### Aqui podemos ver que com o acrescimo da sazonalidade o ajuste por 
#### series temporais apresenta uma melhora comparado ao modelo por regressão multipla
#### as variaveis que explicam seguem sendo as mesmas, porém assim com no modelo 
#### anterior podemos ver que o ultimo modelo não tem o melhor ajuste



imcdiag(ts6, "VIF")

# CCF ---------------------------------------------------------------------

ccf2(dados_mes$m_CPI, dados_mes$m_demand)
ccf2(dados_mes$m_supply_data, dados_mes$m_demand)
ccf2(dados_mes$m_cost_sms, dados_mes$m_demand)
ccf2(dados_mes$m_cost_newspapers, dados_mes$m_demand)
ccf2(dados_mes$m_cost_tv, dados_mes$m_demand)





###########  PROXIMOS PASSOS

############### 1. ANÁLISE DE RESÍDUOS
############### 2. ANÁLISE DE SELEÇÃO DOS MODELOS
############### 3. AN´LAISE SEGUINDO O MODELO ARIMA
