library(neuralnet)
library(keras) # for deep learning
library(tidyverse) # general utility functions
library(caret)
library(readxl)
library(fable)
#install.packages('Metrics')
library(Metrics)

dim(dados_mensais)
str(dados_mensais)
num_data <- dados_mensais[,c(-1,-16,-17)]
str(num_data)


par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(dados_mensais$m_demand,x= dados_mensais$data, xlab="Tempo", ylab="Série da demanda", pch=19, col="skyblue3", type = "l")
grid()
plot(diff(dados_mensais$m_demand,1), xlab="Tempo", ylab="Série diferenciada", pch=19, col="skyblue3", type = "l")
grid()


####DEIXANDO TODOS OS DADOS ESTACIONÁRIOS
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

# data_diff<-ts(data_diff, frequency=12, start=c(2010,2))
# data_diff

plot(data_diff$m_supply_data,x= data_diff$data, xlab="Tempo", ylab="Série do resto", pch=19, col="skyblue3", type = "l")
grid()

### SEPRAÇÃO DE TREINO E TESTE

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

autoplot(ts(train[,c(1:12)]))
#### TODOS OS DADOS ESTÃO ESTACIONARIOS


par(mfrow=c(2,1), mar=c(3,3,1,1), mgp=c(1.6,.6,0))
plot(dados_mensais$m_demand-fitted(fit),x= dados_mensais$data, xlab="Tempo", ylab="Série do resto", pch=19, col="skyblue3", type = "l")
grid()
plot(diff(dados_mensais$m_demand,1), xlab="Tempo", ylab="Série diferenciada", pch=19, col="skyblue3", type = "l")
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
RMSE(fit1$residuals)


fit2 <- arima(train$m_demand, xreg = cov, order = c(2,0,1))
checkresiduals(fit2)
sarima(train$m_demand, xreg = cov, 2,0,1)
RMSE(fit2$residuals)

fit3 <- arima(train$m_demand, xreg = cov, order = c(3,0,1))
checkresiduals(fit3)
sarima(train$m_demand, xreg = cov, 3,0,1)
RMSE(fit3$residuals)

fit4 <- arima(train$m_demand, xreg = cov, order = c(3,0,2))
checkresiduals(fit4)
sarima(train$m_demand, xreg = cov, 3,0,2)
RMSE(fit4$residuals)

fit5<- arima(train$m_demand, xreg = cov, order = c(3,0,3))
checkresiduals(fit5)
sarima(train$m_demand, xreg = cov, 3,0,3)
RMSE(fit5$residuals)
acf2(fit5$residuals)

fit6<- arima(train$m_demand, xreg = cov, order = c(3,0,0))
checkresiduals(fit6)
sarima(train$m_demand, xreg = cov, 3,0,0)
RMSE(fit6$residuals)

fit7<- arima(train$m_demand, xreg = cov, order = c(3,0,4))
checkresiduals(fit7)
sarima(train$m_demand, xreg = cov, 3,0,4)
RMSE(fit7$residuals)


### pela analise de residuos vemos que o modelo que mais se destaca é o fit6, um modelo arima
### (3,0,0) sendo esse nosso modelo selecionado, porém pelo critério de aic o melhor modelo foi o fit5

AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)
AIC(fit5)
AIC(fit6)
AIC(fit7)

##### -------------------- PREDIÇÃO --------------------------------------

cov_test <- test[,c(2:12)]
str(cov_test)
cov_test<-as.matrix(cov_test)


dev.off()
pred5 <-sarima.for(train$m_demand,
           xreg = cov, 
           newxreg = cov_test, 8,
           3,0,3)


pred6 <-sarima.for(train$m_demand,
           xreg = cov, 
           newxreg = cov_test[1:8,], 8,
           3,0,0)


c(test[1])
c(pred6$pred)
c(pred5$pred)

## -----------------------------------------------------------------------------------


#### PARA O FIT 6
### desfaz o calculo da diferença
diffinv(data_diff$m_demand, xi = 209071)
c(num_data$m_demand)

pred6
dif_demand<-c(train$m_demand)

dif_pred6<-c(9229.096,  31926.453, -11164.673, -14209.466,  44468.313,  -7609.844, -16859.231, -20510.920)
d<-c(dif_demand,dif_pred6)
inversa6<-diffinv(d, xi = 209071)

num_data$m_demand[c(1:77)]


num_data$m_demand[-c(1:77)]
data_inversa6 <-as_data_frame(inversa6)
c(data_inversa6[-c(1:77),])

pred_real6<-data.frame(predição = c(data_inversa6[-c(1:78),]),
           dados_reais = c(num_data$m_demand[-c(1:78)])
           )


RMSE(pred_real6$dados_reais, pred_real6$value)






### PARA O FIT 5 


#### PARA O FIT 6
### desfaz o calculo da diferença
diffinv(data_diff$m_demand, xi = 209071)
c(num_data$m_demand)

pred5
dif_demand<-c(train$m_demand)

dif_pred5<-c(3219.057,  21012.091,  -6177.925,  -7668.715,  37098.048,  -4742.341, -19223.474, -27501.158)
d<-c(data_diff$m_demand[c(1:77)], dif_pred5)
inversa5<-diffinv(d, xi = 209071)


num_data$m_demand[c(1:77)]


num_data$m_demand[-c(1:77)]
data_inversa5<-as_data_frame(inversa5)
c(data_inversa5[-c(1:77),])

pred_real5<-data.frame(predict = c(data_inversa5[-c(1:78),]),
                      dados_reais = c(num_data$m_demand[-c(1:78)])
)

pred_real5
pred_real6

RMSE(pred_real5$dados_reais, pred_real5$value)


rmse(pred_real5$dados_reais, pred_real5$value)
mae(pred_real5$dados_reais, pred_real5$value)
mape(pred_real5$dados_reais, pred_real5$value)




#install.packages('Metrics')
library(Metrics)
previsto<-inversa5
RNN <- c(171353.1, 168123.1, 167969.0, 165288.5, 161884.9, 159054.5, 152365.3, 145833.6)
prev_RNN <- c(num_data$m_demand[1:78], RNN)

comparativo<-pred_real5
comparativo
rmse(pred_real5$dados_reais, pred_real5$value)


options (scipen = 999)
######
# install.packages('ggfortify')
# install.packages('zoo')
library(ggfortify)
library(zoo)
ts1 <- inversa5
ts2 <- num_data$m_demand #dados observados
ts3 <- prev_RNN
autoplot(ts( cbind( "predição ARIMA"= ts1,"predição RNN" = ts3, observado=ts2), start = c(2010,1), frequency = 12),
         facets = FALSE, 
         ylab = "Demanda",
         xlab = "Tempo")
