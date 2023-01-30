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
str(dados_mes_ts)

############################################################################

##### ANÁLISE DE REGRESSÃO MULTIPLA


rm1 <- lm(m_demand ~ m_PPI + m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_radio + m_cost_tv + m_cost_internet,
            data = dados_mes)

summary(rm1)

par(mfrow = c(2,2))
plot(rm1, which = c(1:4), pch = 20)

rm2 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_radio + m_cost_tv + m_cost_internet,
          data = dados_mes)

summary(rm2)

par(mfrow = c(2,2))
plot(rm2, which = c(1:4), pch = 20)


rm3 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv + m_cost_internet,
          data = dados_mes)

summary(rm3)

par(mfrow = c(2,2))
plot(rm3, which = c(1:4), pch = 20)

rm4 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + m_mean_unit_price + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv,
          data = dados_mes)

summary(rm4)

par(mfrow = c(2,2))
plot(rm4, which = c(1:4), pch = 20)

rm5 <- lm(m_demand ~ m_CCI + m_CPI + m_sales + m_supply_data + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv,
          data = dados_mes)

summary(rm5)

par(mfrow = c(2,2))
plot(rm5, which = c(1:4), pch = 20)

rm6 <- lm(m_demand ~ m_CPI +m_supply_data + 
            m_cost_sms +  m_cost_newspapers + m_cost_tv,
          data = dados_mes)

summary(rm6)

par(mfrow = c(2,2))
plot(rm5, which = c(1:4), pch = 20)

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

ggplot(dados_mensais, aes(x = ind_mes, y = m_CCI)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_PPI)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_CPI)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_sales)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_cost_sms)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_cost_newspapers)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_cost_internet)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_cost_radio)) +
  geom_line()

ggplot(dados_mensais, aes(x = ind_mes, y = m_cost_tv)) +
  geom_line()

hist(dados_mensais$m_demand)

hist(dados_mensais$m_cost_sms)
hist(dados_mensais$m_cost_internet)
hist(dados_mensais$m_cost_radio)
hist(dados_mensais$m_cost_tv)


str(dados_mes)

########################################################################################
######## DECOMPOSE PARA DEMANDA https://rpubs.com/davoodastaraky/TSA1

ts_demanda<-ts(dados_mes$m_demand, frequency=12, start=c(2010,1))
str(ts_demanda)
head(ts_demanda)

dadossazonais<- decompose(ts_demanda)
plot(dadossazonais)



##### VERIFICAR PARA OUTRAS VARIÁVEIS ################################



dados_mes_ts<-ts(dados_mes$m_cost_sms, frequency=12, start=c(2010,1))
str(dados_mes_ts)
head(dados_mes_ts)

sazidata<- decompose(dados_mes_ts)
plot(sazidata)



###########################################################################

ts1 <- tslm(m_demand ~ season + m_PPI + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers + m_cost_radio 
            + m_cost_tv + m_cost_internet, 
            data = dados_mes_ts)

summary(ts1)

checkresiduals(ts1)


ts2 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers + m_cost_radio 
            + m_cost_tv + m_cost_internet, 
            data = dados_mes_ts)

summary(ts2)

checkresiduals(ts2)

ts3 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers
            + m_cost_tv + m_cost_internet, 
            data = dados_mes_ts)

summary(ts3)

checkresiduals(ts3)

ts4 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers
            + m_cost_tv, 
            data = dados_mes_ts)

summary(ts4)

checkresiduals(ts4)

ts5 <- tslm(m_demand ~ season  + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers
            + m_cost_tv, 
            data = dados_mes_ts)

summary(ts5)

checkresiduals(ts5)

ts6 <- tslm(m_demand ~ season  + m_CPI + m_sales + m_supply_data +m_cost_sms
            + m_cost_newspapers + m_cost_tv, 
            data = dados_mes_ts)

summary(ts6)

checkresiduals(ts6)

ts7 <- tslm(m_demand ~ season  + m_CPI + m_supply_data +m_cost_sms
            + m_cost_newspapers + m_cost_tv, 
            data = dados_mes_ts)

summary(ts7)

checkresiduals(ts7)

#### Aqui podemos ver que com o acrescimo da sazonalidade o ajuste por 
#### series temporais apresenta uma melhora comparado ao modelo por regressão multipla
#### as variaveis que explicam seguem sendo as mesmas, porém assim com no modelo 
#### anterior podemos ver que o ultimo modelo não tem o melhor ajuste



imcdiag(ts7, "VIF")

# CCF ---------------------------------------------------------------------

acf2(dados_mes_ts$m_demand)


ccf2(dados_mes$m_CPI, dados_mes$m_demand)
ccf2(dados_mes$m_supply_data, dados_mes$m_demand)
ccf2(dados_mes$m_cost_sms, dados_mes$m_demand)
ccf2(dados_mes$m_cost_newspapers, dados_mes$m_demand)
ccf2(dados_mes$m_cost_tv, dados_mes$m_demand)



###########################################################################################
#### MUDANÇA DA ESCALA DOS DADOS
### alteração da escala

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


data_ts_scale<-ts(data_new_escala, frequency=12, start=c(2010,1))
str(data_ts_scale)
head(data_ts_scale)
str(data_ts_scale)


############ GRAFICOS NA NOVA ESCALA

ggplot(data_ts_scale, aes(x = ind_mes, y = m_demand)) +
  geom_line()


ggplot(data_ts_scale, aes(x = ind_mes, y = m_CCI)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_PPI)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_CPI)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_sales)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_cost_sms)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_cost_newspapers)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_cost_internet)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_cost_radio)) +
  geom_line()

ggplot(data_ts_scale, aes(x = ind_mes, y = m_cost_tv)) +
  geom_line()

###### DECOMPOSE

ts_demanda<-ts(data_new_escala$m_demand, frequency=12, start=c(2010,1))
str(ts_demanda)
head(ts_demanda)

dadossazonais<- decompose(ts_demanda)
dadossazonais


########### TSLM()

ts1 <- tslm(m_demand ~ season + trend + m_PPI + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers + m_cost_radio 
            + m_cost_tv + m_cost_internet, 
            data = data_ts_scale)

summary(ts1)
anova(ts1)
checkresiduals(ts1)

ts2 <- tslm(m_demand ~ season + trend + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers + m_cost_radio 
            + m_cost_tv + m_cost_internet, 
            data = data_ts_scale)

summary(ts2)
anova(ts2)
checkresiduals(ts2)

ts3 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers + m_cost_radio 
            + m_cost_tv + m_cost_internet, 
            data = data_ts_scale)

summary(ts3)
anova(ts3)
checkresiduals(ts3)

ts4 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers 
            + m_cost_tv + m_cost_internet, 
            data = data_ts_scale)

summary(ts4)
anova(ts4)
checkresiduals(ts4)

ts5 <- tslm(m_demand ~ season + m_CCI + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers 
            + m_cost_tv, 
            data = data_ts_scale)

summary(ts5)
anova(ts5)
checkresiduals(ts5)

ts6 <- tslm(m_demand ~ season + m_CPI + m_sales + m_supply_data
            + m_mean_unit_price +m_cost_sms + m_cost_newspapers 
            + m_cost_tv, 
            data = data_ts_scale)

summary(ts6)
anova(ts6)
checkresiduals(ts6)



ts7 <- tslm(m_demand ~ season + m_CPI + m_sales + m_supply_data
           +m_cost_sms + m_cost_newspapers 
            + m_cost_tv, 
            data = data_ts_scale)

summary(ts7)
anova(ts7)
checkresiduals(ts7)

ts8 <- tslm(m_demand ~ season + m_CPI + m_supply_data
            +m_cost_sms + m_cost_newspapers 
            + m_cost_tv, 
            data = data_ts_scale)

summary(ts8)
anova(ts8)
checkresiduals(ts8)

acf2(data_new_escala$m_demand)



