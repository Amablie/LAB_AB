
# PACOTES -----------------------------------------------------------------
library(tidyverse) ### para manipulação e tratamento dos dados
library(readxl) ## para ler a base de dados


### BASE DE DADOS --------------------------------------------------------------------
MMM_data <- read_excel("MMM_data_excel.xlsx", 
                       col_types = c("text", "date", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric", 
                                     "numeric", "numeric", "numeric","numeric"))


##------------------------------------------------------------------------------------

head(MMM_data)
summary(MMM_data)
str(MMM_data)


# data_new_escala <-
#   MMM_data %>% 
#   transform(DEMAND = DEMAND/1000,
#             CPI  = CPI/1000,
#             CCI = CCI/1000,
#             PPI = PPI/1000,
#             m_cost_sms = m_cost_sms/1000,
#             m_cost_newspapers = m_cost_newspapers/1000,
#             m_cost_radio = m_cost_radio/1000,
#             m_cost_tv  = m_cost_tv/1000,
#             m_cost_internet  = m_cost_internet/1000,
#             m_CPI = m_CPI/1000,
#             m_CCI  = m_CCI/1000,
#             m_PPI  = m_PPI/1000)



MMM_nonnumeric <- MMM_data[,-c(1,9)] ### apenas valores númericos
tb <- MMM_nonnumeric %>% 
  gather(key = "variable", value = "value", -DATE)

str(tb)

ggplot(tb,
       mapping = aes( y= value, x = DATE)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  geom_line() 

MMM_cost <- MMM_data[, -c(1,4,5,6,15,16,17,18,19)] # sem os dados de GRP
ggpairs(MMM_cost)


## RESPOSTA ------------------------------------------

Sales <- ggplot(MMM_data, aes(x = DATE, y = SALES)) +
  geom_line()
supply <- ggplot(MMM_data, aes(x = DATE, y = Supply_Data)) +
  geom_line()
unit <- ggplot(MMM_data, aes(x = DATE, y = Unit_Price)) +
  geom_line()
demand <- ggplot(MMM_data, aes(x = DATE, y = DEMAND)) +
  geom_line()

grid.arrange(Sales, supply, unit, demand,  nrow = 4)


## MACROECONOMICA -----------------------------------

PPI <-ggplot(MMM_data, aes(x = DATE, y = PPI)) +
  geom_line()

CCI <- ggplot(MMM_data, aes(x = DATE, y = CCI)) +
  geom_line()

CPI <- ggplot(MMM_data, aes(x = DATE, y = CPI)) +
  geom_line()

grid.arrange(PPI, CCI, CPI,  nrow = 3)


# Como é possível observar as varipaveis estão com agregações diferentes, as variáveis macroeconomicas 
# estão no formato mês e as demais estão diárias.
# 
# Optamos em transformar a base para o formato **mês** devido ao comportamente mensal das variaveis 
# macroeconomicas observadas,combinado a ideia de que a série traz muitos ruídos em suas variaveis, talvez
# a agregação pudesse ajudar a suavizar o comportamento das outras variáveis e reduzir o ruído.


dia <- ggplot(MMM_data, aes(x = DATE, y = DEMAND)) +
  geom_line()
semana <- ggplot(teste, aes(x = indice_semana, y = weekly_demand)) +
  geom_line()
mes <- ggplot(dados_mensais, aes(x = data, y = m_demand)) +
  geom_line()
grid.arrange(dia, semana, mes,  nrow = 3)


# Aqui podemos ver o efeito da agregação diante da variável resposta escolhida para a modelagem
# no caso a variável Demanda.


#  VARIAVEIS DE CUSTO -----------------------------------------------------

##Investimento em canais
mes_SMS <- ggplot(dados_mensais, aes(x = data, y = m_cost_sms)) +
  geom_line()

mes_TV <- ggplot(dados_mensais, aes(x = data, y = m_cost_tv)) +
  geom_line()

mes_NEWS <- ggplot(dados_mensais, aes(x = data, y = m_cost_newspapers)) +
  geom_line()

mes_RADIO <- ggplot(dados_mensais, aes(x = data, y = m_cost_radio)) +
  geom_line()

mes_INTERNET <- ggplot(dados_mensais, aes(x = data, y = m_cost_internet)) +
  geom_line()

grid.arrange(mes_SMS, mes_TV, mes_NEWS, mes_RADIO, mes_INTERNET,  nrow = 5)


# Analisando os dados de investimento nos canais de midia da empresa em questão, notamos um 
# comportamente semelhante ao de ruído branco na nossa base de dados, chamando nossa atenção
# para como são feitas as distribuições de custo ao longo dos anos para essa empresa.
