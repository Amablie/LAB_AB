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

str(data_cyn)
##desconsiderando dados macroeconomicos e GRPs
only_data<-data_cyn[,c(1,2,3,7,8,9,10,11,12,13,14,21,22,23,24)]
str(only_data)

##agregando de forma semanal, somando

aggregate(no~DATE+, only_data, sum)
