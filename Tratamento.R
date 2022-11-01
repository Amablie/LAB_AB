library(tidyverse)
library(readxl)
library(dygraphs) 
library(GGally)
library(tsible)
library(corrgram)
library(corrplot)
install.packages("astsa")
library(astsa)




### BASE DE DADOS
MMM_data <- read_excel("MMM_data_excel.xlsx", 
                                 col_types = c("text", "date", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric","numeric"))


##--------------------------------------------------------------------------

options (scipen = 999)
head(MMM_data)
summary(MMM_data)
str(MMM_data)
MMM_nonnumeric <- MMM_data[,-c(1,2)]
cor(MMM_nonnumeric)

pairs(MMM_nonnumeric)
ggcorr(MMM_nonnumeric)
corrplot(MMM_nonnumeric)


teste <- MMM_data %>% 
  mutate(sales_teste = Unit_Price*Supply_Data)
teste[,c("SALES", "sales_teste")]

## RESPOSTA

ggplot(MMM_data, aes(x = DATE, y = SALES)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Supply_Data)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Unit_Price)) +
  geom_line()


ggplot(MMM_data, aes(x = DATE, y = DEMAND)) +
  geom_line()

ggplot(MMM_data, aes(x = Unit))

## MACROECONOMICA

ggplot(MMM_data, aes(x = DATE, y = CPI)) +
  geom_line()






acf2(MMM_data$DEMAND)
