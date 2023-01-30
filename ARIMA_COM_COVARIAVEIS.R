library(neuralnet)
library(keras) # for deep learning
library(tidyverse) # general utility functions
library(caret)
library(readxl)
library(fable)

dim(data_new_escala)
str(data_new_escala)
num_data <- data_new_escala[,c(-1,-16,-17)]
str(num_data)


# macroeconomica
acf2(dados_mes$m_PPI)
acf2(dados_mes$m_CCI)
acf2(dados_mes$m_CPI)

## diferença
acf2(diff(dados_mes$m_PPI))
acf2(diff(dados_mes$m_CCI)) # lag 2
acf2(diff(dados_mes$m_CPI)) # lag 3

# Olhando os dados, podemos ver que talvez seja necessário realizar uma diferança para tornar
# os dados estacionários

# custo
acf2(dados_mes$m_cost_sms)
acf2(dados_mes$m_cost_newspapers)
acf2(dados_mes$m_cost_radio)
acf2(dados_mes$m_cost_internet)
acf2(dados_mes$m_cost_tv)




par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(data_new_escala$m_CPI,x= data_new_escala$data, xlab="Tempo", ylab="Série do resto", pch=19, col="skyblue3", type = "l")
grid()
plot(diff(data_new_escala$m_demand,1), xlab="Tempo", ylab="Série diferenciada", pch=19, col="skyblue3", type = "l")
grid()



data_diff <- data.frame(
            m_demand = diff(num_data$m_demand,1),
            m_supply_data  = diff(num_data$m_supply_data,1),
            m_mean_unit_price =  diff(num_data$m_mean_unit_price,1),
            m_sales =  diff(num_data$m_sales,1),
            m_cost_sms =  diff(num_data$m_cost_sms,1),
            m_cost_newspapers =  diff(num_data$m_cost_newspapers,1),
            m_cost_radio =  diff(num_data$m_cost_radio,1),
            m_cost_tv  =  diff(num_data$m_cost_tv,1),
            m_cost_internet  =  diff(num_data$m_cost_internet,1),
            m_CPI = diff(num_data$m_CPI,1),
            m_CCI  =  diff(num_data$m_CCI,1),
            m_PPI  =  diff(num_data$m_PPI,1),
            data = num_data$data[-1])

str(data_diff)
str(num_data)


plot(data_diff$m_supply_data,x= data_diff$data, xlab="Tempo", ylab="Série do resto", pch=19, col="skyblue3", type = "l")
grid()

train <-  data_diff[1:77, ]
test <- data_diff[-c(1:77), ]


dim(train)
dim(test)


str(train)
# head(midiacov)
head(train)



head(test)

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

#### TODOS OS DADOS ESTÃO ESTACIONARIOS


par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(data_new_escala$m_demand-fitted(fit),x= data_new_escala$data, xlab="Tempo", ylab="Série do resto", pch=19, col="skyblue3", type = "l")
grid()
plot(diff(data_new_escala$m_demand,1), xlab="Tempo", ylab="Série diferenciada", pch=19, col="skyblue3", type = "l")
grid()


###### Aqui podemos ver que as varivaeis de custo são estacionárias e apresentam
##### comportamento ciclico, quanto as macroeconomicas e de vendas possue
##### tendencia e sazonalidade, logo são não estacionárias


cov <- train[,c(2:12)]
str(cov)
cov<-as.matrix(cov) ### precisa transformar em matriz antes de colocar no modelo


######## DECOMPOSIÇÃO DAS VARIAVEIS DE MIDIA ########################
ts_cost<-ts(train$m_cost_sms, frequency=12, start=c(2010,1))
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

fit2 <- arima(train$m_demand, xreg = cov, order = c(2,0,1))
checkresiduals(fit2)
sarima(train$m_demand, xreg = cov, 2,0,1)


fit3 <- arima(train$m_demand, xreg = cov, order = c(3,0,1))
checkresiduals(fit3)
sarima(train$m_demand, xreg = cov, 3,0,1)

fit4 <- arima(train$m_demand, xreg = cov, order = c(3,0,2))
checkresiduals(fit4)
sarima(train$m_demand, xreg = cov, 3,0,2)


fit5<- arima(train$m_demand, xreg = cov, order = c(3,0,3))
checkresiduals(fit5)
sarima(train$m_demand, xreg = cov, 3,0,3)

fit6<- arima(train$m_demand, xreg = cov, order = c(3,0,0))
checkresiduals(fit5)
sarima(train$m_demand, xreg = cov, 3,0,0)

AIC(fit3)  #### melhor modelo ajustado
AIC(fit4)
AIC(fit5)  
AIC(fit6)

BIC(fit3)  #### melhor modelo ajustado
BIC(fit4)
BIC(fit5)  #### melhor modelo ajustado
BIC(fit6)

sarima(train$m_demand, xreg = cov, 3,0,2)  ####  


##### PREDIÇÃO

cov_test <- test[,c(2:12)]
str(cov_test)
cov_test<-as.matrix(cov_test)


dev.off()
sarima.for(train$m_demand,
           xreg = cov, 
           newxreg = cov_test[c(1:2),], 2,
           3,0,2)


sarima.for(train$m_demand,
           xreg = cov, 
           newxreg = cov_test[c(1:2),], 2,
           3,0,0)

ggplot(test, aes(x = data , y = m_demand)) +
  geom_line()

auto.arima(train$m_demand, xreg = cov)



## -----------------------------------------------------------------------------------





