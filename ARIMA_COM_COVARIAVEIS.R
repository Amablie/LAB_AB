library(neuralnet)
library(keras) # for deep learning
library(tidyverse) # general utility functions
library(caret)
library(readxl)

dim(data_new_escala)
str(data_new_escala)
num_data <- data_new_escala[,c(-1,-16,-17)]
str(num_data)

indice_treino <- createDataPartition(y = num_data$m_demand,p = 0.9, list = F)
train <-  num_data[indice_treino, ]
test <- num_data[-indice_treino, ]

dim(train)
dim(test)


str(train)
# head(midiacov)


head(train)
# par(mfrow = c(3,1))
# plot(y=macrocov$m_CPI, x=data_new_escala$data, type = "l")
# plot(y=macrocov$m_PPI, x=data_new_escala$data, type = "l")
# plot(y=macrocov$m_CCI, x=data_new_escala$data, type = "l")
# dev.off()


macrocov <- train[, c(10:12)]
vendacov <- train[, c(2:4)]
midiacov <- train[, c(5:9)]

autoplot(ts(midiacov))
autoplot(ts(vendacov))
autoplot(ts(macrocov))
plot.ts(midiacov)

###### Aqui podemos ver que as varivaeis de custo são estacionárias e apresentam
##### comportamento ciclico, quanto as macroeconomicas e de vendas possue
##### tendencia e sazonalidade, logo são não estacionárias


cov <- train[,c(2:12)]
str(cov)
cov<-as.matrix(cov) ### precisa transformar em matriz antes de colocar no modelo


######## DECOMPOSIÇÃO DAS VARIAVEIS DE MIDIA ########################
ts_cost<-ts(train$m_cost_tv, frequency=12, start=c(2010,1))
str(ts_cost)
head(ts_cost)

dsazonais<- decompose(ts_cost)
plot(dsazonais)

######################################################################

arima(train$m_demand, xreg = cov, order = c(0,1,1))
sarima(train$m_demand, xreg = cov, 0,1,1)

#### fazemos a verificação com os argumentos do modelo apenas para demanda
#### Apesar de ter funcionado para a demanda não é um bom modelo para as
#### covariaveis, pois apresenta um valor abaixo do p-valor 




##### ANALISANDO MÉDIA MÓVEL ###########

autoplot(ts(data_new_escala$m_cost_radio)) +
  autolayer(ma(data_new_escala$m_cost_radio,2), series="5-MA") +
  xlab("Year") + ylab("Demand") +
  scale_colour_manual(values=c("Data"="grey50","5-MA"="red"),
                      breaks=c("Data","5-MA"))



#### A estacionaridade tem que ficar na linha do zero, com o acrescimo da média
#### móvel em midia, podemos suavizar a linha e aproximar de zero
#########################################################



fit1<- arima(train$m_demand, xreg = cov, order = c(1,0,1))

checkresiduals(fit1)

sarima(train$m_demand, xreg = cov, 1,0,1)

fit2 <- arima(train$m_demand, xreg = cov, order = c(2,0,2))

checkresiduals(fit2)

sarima(train$m_demand, xreg = cov, 2,0,2)


fit3 <- arima(train$m_demand, xreg = cov, order = c(3,0,2))

checkresiduals(fit3)

sarima(train$m_demand, xreg = cov, 3,0,2)

fit4 <- arima(train$m_demand, xreg = cov, order = c(4,0,2))

checkresiduals(fit4)
#### entender a saída
sarima(train$m_demand, xreg = cov, 4,0,2)
#### ENTENDER A SAÍDA DO SARIMA


AIC(fit3)  #### melhor modelo ajustado
AIC(fit4)


sarima(train$m_demand, xreg = cov, 3,0,2)  ####  


##### PREDIÇÃO

cov_test <- test[,c(2:12)]
str(cov_test)
cov_test<-as.matrix(cov_test)



sarima.for(train$m_demand,
           xreg = cov, 
           newxreg = cov_test, 8,
           3,0,2)

