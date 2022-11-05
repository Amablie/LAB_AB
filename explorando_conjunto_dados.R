str(MMM_data)
data_cyn<-MMM_data
head(data_cyn)

## separadando as datas em dia, mês e ano
library(tidyverse)
data_cyn$DATE <- as.Date(data_cyn$DATE)
data_cyn<-data_cyn%>%transform(data_cyn, day = format(DATE, "%d"), 
          month = format(DATE, "%m"), year = format(DATE, "%Y"))
data_cyn
head(data_cyn)

##atribuindo indice a cada data
data_cyn$index <- seq.int(nrow(data_cyn))
data_cyn

data_cyn
