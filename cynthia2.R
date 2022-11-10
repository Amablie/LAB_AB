str(MMM_data)
data_cyn<-MMM_data
head(data_cyn)
str(data_cyn)

## separadando as datas em dia, mês e ano
library(tidyverse)
data_cyn$DATE <- as.Date(data_cyn$DATE)
str(data_cyn)
data_cyn<-data_cyn%>%transform(data_cyn$DATE, day = format(DATE, "%d"), 
                               month = format(DATE, "%m"), year = format(DATE, "%Y"))
str(data_cyn)

##atribuindo indice a cada data
data_cyn$index <- seq.int(nrow(data_cyn))


##atribuindo indice à semana
semana<-as.vector(rep(c(1:373), each = 7))
indice_semana<-semana[1:2613]
data_cyn$indice_semana<-indice_semana


##desconsiderando dados macroeconomicos e GRPs
only_data<-data_cyn[,c(1,2,3,7,8,9,10,11,12,13,14,21,22,23,24,25)]
str(only_data)
only_data 

##agregando de forma semanal, somando algumas variaveis, mas manter fixo outras

teste<-only_data %>%
  group_by(indice_semana) %>%
  summarise(weekly_demand = sum(DEMAND),
            weekly_mean_unit_price=mean(Unit_Price),
            weekly_supply_data=sum(Supply_Data),
            weekly_sales=sum(SALES),
            weekly_cost_sms=sum(Cost_SMS),
            weekly_cost_newspapers=sum(Cost_Newspaper),
            weekly_cost_radio=sum(Cost_Radio),
            weekly_cost_tv=sum(Cost_TV))
teste

ggplot(teste, aes(x = indice_semana, y = weekly_demand)) +
  geom_line()


#Agregação de forma semanal para acompanhar as séries de custos
ggplot(teste, aes(x = indice_semana, y = weekly_mean_unit_price)) +
  geom_line()

ggplot(teste, aes(x = indice_semana, y = weekly_supply_data)) +
  geom_line()

ggplot(teste, aes(x = indice_semana, y = weekly_sales)) +
  geom_line()

ggplot(teste, aes(x = indice_semana, y = weekly_cost_sms)) +
  geom_line()

ggplot(teste, aes(x = indice_semana, y = weekly_cost_newspapers)) +
  geom_line()

ggplot(teste, aes(x = indice_semana, y = weekly_cost_radio)) +
  geom_line()

ggplot(teste, aes(x = indice_semana, y = weekly_cost_tv)) +
  geom_line()

###Fazendo a agregação por mês

install.packages('zoo')
install.packages('magrittr')
library(tidyverse)
library(zoo)
library(magrittr)
data_cyn2<-MMM_data

data_cyn2$DATE <- as.Date(data_cyn2$DATE)
data_cyn2$anomes<-as.yearmon(data_cyn2$DATE, "%b-%y") %>%
  format(., "%Y-%m")


#desconsiderando apenas os dados de GRP numa nova tabela e agregando por mes
only_data2<-data_cyn2[,c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,20)]
teste2<-only_data2 %>%
  group_by(anomes) %>%
  summarise(m_demand = sum(DEMAND),
            m_mean_unit_price=mean(Unit_Price),
            m_supply_data=sum(Supply_Data),
            m_sales=sum(SALES),
            m_cost_sms=sum(Cost_SMS),
            m_cost_newspapers=sum(Cost_Newspaper),
            m_cost_radio=sum(Cost_Radio),
            m_cost_tv=sum(Cost_TV),
            m_CPI=mean(CPI),
            m_CCI=mean(CCI),
            m_PPI=mean(PPI))


library(lubridate) #inserindo indice para observações do mês
teste2$ind_mes<-seq.int(nrow(teste2))
str(teste2)

teste2$anomes<- as.POSIXct(as.numeric(as.character(teste2$anomes)), origin='2010-01')

teste2$ano<-as.yearmon(teste2$anomes, "%b-%y") %>%
  format(., "%Y")

teste2



#Séries no formato mensal para acompanhar as séries de custos
##Dados Macro Econômicos
ggplot(teste2, aes(x =ind_mes, y = m_PPI)) + geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_CCI)) +
  geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_PPI)) +
  geom_line()

##Dados de desempenho de vendas
ggplot(teste2, aes(x = ind_mes, y = m_demand)) +
  geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_mean_unit_price)) +
  geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_supply_data)) +
  geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_sales)) +
  geom_line()

##Investimento em canais
ggplot(teste2, aes(x = ind_mes, y = m_cost_sms)) +
  geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_cost_tv)) +
  geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_cost_newspapers)) +
  geom_line()

ggplot(teste2, aes(x = ind_mes, y = m_cost_radio)) +
  geom_line()



