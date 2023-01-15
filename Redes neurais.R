# https://www.learnbymarketing.com/tutorials/neural-networks-in-r-tutorial/
# https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/

##install.packages("caret")
library(neuralnet)
library(keras) # for deep learning
library(tidyverse) # general utility functions
library(caret)
library(readxl)

MMM_data <- read_excel("MMM_data_excel.xlsx", 
                       col_types = c("text", "date", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric","numeric"))


str(MMM_data)
head(MMM_data)
dim(MMM_data)


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


### Devemos manter os dados originais ou transformamos em mensal tbm?
### https://blogs.rstudio.com/ai/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/
### Se a resposta for sim, devemos transformar os dados novamente dividindo por 10000 e tirando as colunas não númericas

###############################################################################

dim(data_new_escala)
str(data_new_escala)
num_data <- data_new_escala[,c(-1,-16,-17)]
str(num_data)

indice_treino <- createDataPartition(y = num_data$m_demand,p = 0.9, list = F)
train <-  num_data[indice_treino, ]
test <- num_data[-indice_treino, ]

dim(train)
dim(test)


