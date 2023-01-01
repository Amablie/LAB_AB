# https://www.learnbymarketing.com/tutorials/neural-networks-in-r-tutorial/
# https://www.analyticsvidhya.com/blog/2017/09/creating-visualizing-neural-network-in-r/

install.packages("caret")
library(neuralnet)
library(keras) # for deep learning
library(tidyverse) # general utility functions
library(caret)


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




fit <- nnetar(data_new_escala, lambda=0)
