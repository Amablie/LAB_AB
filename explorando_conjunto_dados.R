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




str(data_cyn)

##desconsiderando dados macroeconomicos e GRPs
only_data<-data_cyn[,c(1,2,3,7,8,9,10,11,12,13,14,21,22,23,24,25)]
str(only_data)

##agregando de forma semanal, somando algumas variaveis, mas manter fixo outras

x<-only_data %>%
  group_by(indice_semana) %>%
  summarise(weekly_demand = sum(DEMAND),
            weekly_mean_unit_price=mean(Unit_Price),
            weekly_supply_data=sum(Supply_Data),
            weekly_sales=sum(SALES),
            weekly_cost_sms=sum(Cost_SMS),
            weekly_cost_newspapes=sum(Cost_Newspaper),
            weekly_cost_radio=sum(Cost_Radio),
            weekly_cost_tv=sum(Cost_TV))


aggregate(no~DATE+, only_data, sum)

data_cyn %>% 
  ggplot(aes(x = DATE,
             y = Cost_SMS))+
  geom_line() +
  facet_wrap(~ year)



data_cyn %>% 
  ggplot(aes(x = DATE,
             y = Cost_SMS))+
  geom_line() +
  facet_wrap(~ year)
