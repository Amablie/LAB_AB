## https://towardsdatascience.com/building-a-marketing-mix-model-in-r-3a7004d21239

#### Tutorial de aplicação de series temporais para MMM

library(astsa) # para analisar séries temporais
library(tsibble)  # para trabalhar com séries temporais
library(tidyverse) ### para manipulação e tratamento dos dados

### foi decidido que a abordagem seria mensal, por conta dos dados macro economicos e do ruído dos dados
##### BASE DE DADOS MENSAL ####################################

only_data2<-data_cyn2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,20)]
data_mes<-only_data2 %>%
  group_by(anomes) %>%
  summarise(m_demand = sum(DEMAND),
            m_mean_unit_price=mean(Unit_Price),
            m_supply_data=sum(Supply_Data),
            m_sales=sum(SALES),
            m_cost_sms=sum(Cost_SMS),
            m_cost_newspapers=sum(Cost_Newspaper),
            m_cost_radio=sum(Cost_Radio),
            m_cost_tv=sum(Cost_TV),
            m_cost_internet = sum(Cost_Internet),
            m_CPI=mean(CPI),
            m_CCI=mean(CCI),
            m_PPI=mean(PPI))

##############################################################


library(lubridate) #incerindo indice para observações do mês
data_mes$ind_mes<-seq.int(nrow(data_mes))
data_mes

### variaveis resposta
ggplot(data_mes, aes(x = ind_mes, y = m_demand)) +
  geom_line()

ggplot(data_mes, aes(x = ind_mes, y = m_sales)) +
  geom_line()


hist(data_mes$m_demand)
hist(data_mes$m_cost_sms)
hist(data_mes$m_cost_internet)
