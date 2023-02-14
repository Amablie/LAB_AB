#Instalando Pacotes

install.packages('tidyverse')
library(tidyverse)
install.packages('ludibridate')
library(lubridate)
install.packages('tsibble')
library(tsibble) # Tidy Temporal Data Frames and Tools
install.packages('feasts')
library(feasts) # Feature Extraction and Statistics for Time Series
install.packages('tsibbledata')
library(tsibbledata) # Diverse Datasets for 'tsibble'
install.packages('torch')
library(torch)
install.packages('readxl')
library(readxl)


#Carregando dados
dados<-read_excel("dados_mensais_imp.xlsx")
str(dados)


# Data input code modified to accommodate two predictors

n_timesteps <-1
n_forecast <- 8
batch_size <- 32


#SEPARANDO EM TREINO, VALIDAÇÃO E TESTE
train<-dados%>%
  filter(data<'2015-05-01')%>%
  select(m_demand, m_cost_sms, m_cost_newspapers,m_mean_unit_price,m_supply_data,m_sales, m_cost_radio, m_cost_tv, m_cost_internet, m_CPI, m_CCI, m_PPI )%>%
  as.matrix()

str(train)

validation<-dados%>%
  filter(data >= as.Date("2015-05-01") & data <= as.Date("2016-06-01"))%>%
  select(m_demand, m_cost_sms, m_cost_newspapers,m_mean_unit_price,m_supply_data,m_sales, m_cost_radio, m_cost_tv, m_cost_internet, m_CPI, m_CCI, m_PPI)%>%
  as.matrix()

test<-dados%>%
  filter(data>'2016-06-01')%>%
  select(m_demand, m_cost_sms, m_cost_newspapers,m_mean_unit_price,m_supply_data,m_sales, m_cost_radio, m_cost_tv, m_cost_internet, m_CPI, m_CCI, m_PPI)%>%
  as.matrix()


#Calculando Média e Desvio padrão das covariaiveis

train_mean_demand <- mean(train[,1])
train_sd_demand <- sd(train[,1])

train_mean_cost_sms <- mean(train[,2])
train_sd_cost_sms<- sd(train[,2])

train_mean_cost_newspapers <- mean(train[,3])
train_sd_cost_newspapers<- sd(train[,3])

train_mean_m_mean_unit_price<-mean(train[,4])
train_sd_m_mean_unit_price<-sd(train[,4])

train_mean_m_supply_data<-mean(train[,5])
train_sd_m_supply_data<-sd(train[,5])

train_mean_m_sale<-mean(train[,6])
train_sd_m_sale<-sd(train[,6])

train_mean_m_cost_radio<-mean(train[,7])
train_sd_m_cost_radio<-sd(train[,7])

train_mean_m_cost_tv<-mean(train[,8])
train_sd_m_cost_tv<-sd(train[,8])

train_mean_m_cost_internet<-mean(train[,8])
train_sd_m_cost_internet<-sd(train[,8])

train_mean_m_CPI<-mean(train[,9])
train_sd_m_CPI<-sd(train[,9])

train_mean_m_CCI<-mean(train[,10])
train_sd_m_CCI<-sd(train[,10])

train_mean_m_PPI<-mean(train[,11])
train_sd_m_PPI<-sd(train[,11])


dataset_dados <- dataset(
  name = "dataset_dados",
  
  initialize = function(data, n_timesteps, n_forecast, sample_frac = 1) {
    
    demanda <- (data[ , 1] - train_mean_demand)/train_sd_demand
    cost_sms <- (data[ , 2] - train_mean_cost_sms)/ train_sd_cost_sms
    cost_newspapers<-(data[ , 3] - train_mean_cost_newspapers)/ train_sd_cost_newspapers
    unit_price<-(data[ , 4] - train_mean_m_mean_unit_price)/ train_sd_m_mean_unit_price
    supply_data<-(data[ , 5] - train_mean_m_supply_data)/ train_sd_m_supply_data
    sales<-(data[ , 6] - train_mean_m_sale)/ train_sd_m_sale
    cost_radio<-(data[ , 7] - train_mean_m_cost_radio)/ train_sd_m_cost_radio
    cost_tv<-(data[ , 8] - train_mean_m_cost_tv)/ train_sd_m_cost_tv
    cost_internet<-(data[ , 9] - train_mean_m_cost_internet)/ train_sd_m_cost_internet
    CPI<-(data[ , 10] - train_mean_m_CPI)/ train_sd_m_CPI
    CCI<-(data[ , 11] - train_mean_m_CCI)/ train_sd_m_CCI
    PPI<-(data[ , 12] - train_mean_m_PPI)/ train_sd_m_PPI
    self$x <- cbind(demanda, cost_sms,cost_newspapers, unit_price,supply_data,sales, cost_radio, cost_tv, cost_internet, CPI, CCI, PPI) %>%
      torch_tensor()
    
    self$n_timesteps <- n_timesteps
    self$n_forecast <- n_forecast
    
    n <- nrow(self$x) - self$n_timesteps - self$n_forecast + 1
    self$starts <- sort(sample.int(
      n = n,
      size = n * sample_frac
    ))
    
  },
  
  .getitem = function(i) {
    
    start <- self$starts[i]
    end <- start + self$n_timesteps - 1
    pred_length <- self$n_forecast
    
    list(
      x = self$x[start:end, ],
      y = self$x[(end + 1):(end + pred_length), 1]
    )
    
  },
  
  .length = function() {
    length(self$starts)
  }
  
)


### Criando dataset e dataloaders
train_ds <- dataset_dados(train, n_timesteps, n_forecast, sample_frac =1)
train_dl <- train_ds %>% dataloader(batch_size = batch_size, shuffle = TRUE)

valid_ds <- dataset_dados(validation, n_timesteps, n_forecast, sample_frac = 1)
valid_dl <- valid_ds %>% dataloader(batch_size = batch_size)
test_ds <- dataset_dados(test, n_timesteps, n_forecast)
test_dl <- test_ds %>% dataloader(batch_size = 1)


model <- nn_module(
  
  initialize = function(type, input_size, hidden_size, linear_size, output_size,
                        num_layers = 2, dropout = 0, linear_dropout = 0) {
    
    self$type <- type
    self$num_layers <- num_layers
    self$linear_dropout <- linear_dropout
    
    self$rnn <- if (self$type == "gru") {
      nn_gru(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    } else {
      nn_lstm(
        input_size = input_size,
        hidden_size = hidden_size,
        num_layers = num_layers,
        dropout = dropout,
        batch_first = TRUE
      )
    }
    
    self$mlp <- nn_sequential(
      nn_linear(hidden_size, linear_size),
      nn_relu(),
      nn_dropout(linear_dropout),
      nn_linear(linear_size, output_size)
    )
    
  },
  
  forward = function(x) {
    
    x <- self$rnn(x)
    x[[1]][ ,-1, ..] %>% 
      self$mlp()
    
  }
  
)


net <- model("gru", input_size = 12, hidden_size =64 , linear_size = 512, output_size = n_forecast, linear_dropout = 0)
# training RNNs on the GPU currently prints a warning that may clutter 
# the console
# see https://github.com/mlverse/torch/issues/461
# alternatively, use 
# device <- "cpu"
device <- torch_device(if (cuda_is_available()) "cuda" else "cpu")
net <- net$to(device = device)


#Treinamento
optimizer <- optim_adam(net$parameters, lr = 0.001)

num_epochs <- 15

train_batch <- function(b) {
  
  optimizer$zero_grad()
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$backward()
  optimizer$step()
  
  loss$item()
}

valid_batch <- function(b) {
  
  output <- net(b$x$to(device = device))
  target <- b$y$to(device = device)
  
  loss <- nnf_mse_loss(output, target)
  loss$item()
  
}

for (epoch in 1:num_epochs) {
  
  net$train()
  train_loss <- c()
  
  coro::loop(for (b in train_dl) {
    loss <-train_batch(b)
    train_loss <- c(train_loss, loss)
  })
  
  cat(sprintf("\nEpoch %d, training: loss: %3.5f \n", epoch, mean(train_loss)))
  
  net$eval()
  valid_loss <- c()
  
  coro::loop(for (b in valid_dl) {
    loss <- valid_batch(b)
    valid_loss <- c(valid_loss, loss)
  })
  
  cat(sprintf("\nEpoch %d, validation: loss: %3.5f \n", epoch, mean(valid_loss)))
}


#AVALIAÇÃO
net$eval()
test_preds <- vector(mode = "list", length = length(test_dl))
i <- 1
coro::loop(for (b in test_dl) {
  
  input <- b$x
  output <- net(input$to(device = device))
  preds <- as.numeric(output)
  
  test_preds[[i]] <- preds
  i <<- i + 1
  
})


str(test_pred1)


#previsto<-(test_pred1 * train_sd_demand + train_mean_demand)+
 # (test_pred1*train_sd_cost_sms+train_mean_cost_sms)+
  #(test_pred1*train_sd_cost_newspapers+train_mean_cost_newspapers)+
  #(test_pred1*train_sd_m_mean_unit_price+train_mean_m_mean_unit_price)+
  #(test_pred1*train_sd_m_supply_data+train_mean_m_supply_data)+
  #(test_pred1*train_sd_m_sale+train_mean_m_sale)+
  #(test_pred1*train_sd_m_cost_radio+train_mean_m_cost_radio)+
  #(test_pred1*train_sd_m_cost_tv+train_mean_m_cost_tv)+
  #(test_pred1*train_sd_m_cost_internet+train_mean_m_cost_internet)+
  #(test_pred1*train_sd_m_CPI+train_mean_m_CPI)+
  #(test_pred1*train_sd_m_CCI+train_mean_m_CCI)+
  #(test_pred1*train_sd_m_PPI+train_mean_m_PPI)
  
  
#install.packages('Metrics')
library(Metrics)
previsto<-(test_pred1 * train_sd_demand + train_mean_demand)
comparativo<-cbind(atual, previsto)
comparativo
rmse(atual, previsto)
previsto

#GRAFICO #juntando dados+previsão em um único vetor
graf<-length(85)
graf<-c(dados$m_demand[1:78], previsto)

######
install.packages('ggfortify')
install.packages('zoo')
library(ggfortify)
library(zoo)
ts1 <- graf
ts2 <- dados$m_demand # dados observados
autoplot(ts( cbind(observed=ts2, predict=ts1)  , start = c(2010,1), frequency = 12 ),facets = FALSE)

