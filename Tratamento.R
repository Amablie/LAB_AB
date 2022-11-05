library(tidyverse)
library(readxl)
library(dygraphs) 
library(GGally)
library(tsible)
library(corrgram)
library(corrplot)
#install.packages("astsa")
library(astsa)




### BASE DE DADOS --------------------------------------------------------------------
MMM_data <- read_excel("MMM_data_excel.xlsx", 
                                 col_types = c("text", "date", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric","numeric"))


##------------------------------------------------------------------------------------

options (scipen = 999)
head(MMM_data)
summary(MMM_data)
str(MMM_data)
MMM_nonnumeric <- MMM_data[,-c(1,2)]
cor(MMM_nonnumeric)
names(MMM_nonnumeric)




pairs(MMM_nonnumeric)
ggcorr(MMM_nonnumeric)
corrplot(MMM_nonnumeric)


# teste <- MMM_data %>% 
#   mutate(sales_teste = Unit_Price*Supply_Data)
# teste[,c("SALES", "sales_teste")]

## RESPOSTA ------------------------------------------

ggplot(MMM_data, aes(x = DATE, y = SALES)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Supply_Data)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Unit_Price)) +
  geom_line()


ggplot(MMM_data, aes(x = DATE, y = DEMAND)) +
  geom_line()

ggplot(MMM_data, aes(x = Unit_Price, y  = DEMAND))+
  geom_point()

## MACROECONOMICA -----------------------------------

ggplot(MMM_data, aes(x = DATE, y = PPI)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = CCI)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = CPI)) +
  geom_line()

## MARKETING ----------------------------------------------

ggplot(MMM_data, aes(x = DATE, y = Cost_Newspaper)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Cost_SMS)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Cost_Radio)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Cost_TV)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Cost_Internet)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = GRP_NewPaper)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = GRP_SMS)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = GRP_Radio)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = GRP_Internet)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = GRP_TV)) +
  geom_line()


mod<-lm(DEMAND ~ ., data = MMM_nonnumeric)

summary(mod)


mod1<-lm(DEMAND ~ CPI+CCI+PPI +Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Radio+
        Cost_TV+Cost_Internet+GRP_NewPaper+GRP_SMS+GRP_Radio+GRP_Internet+GRP_TV,
        data = MMM_nonnumeric)

summary(mod1)

mod2<-lm(DEMAND ~ CPI+CCI+Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Radio+
           Cost_TV+Cost_Internet+GRP_NewPaper+GRP_SMS+GRP_Radio+GRP_Internet+GRP_TV,
         data = MMM_nonnumeric)

summary(mod2)

mod3<-lm(DEMAND ~ CPI+CCI+Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Radio+
           Cost_TV+Cost_Internet+GRP_NewPaper+GRP_SMS+GRP_Internet+GRP_TV,
         data = MMM_nonnumeric)

summary(mod3)

mod3<-lm(DEMAND ~ CPI+CCI+Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Radio+
           Cost_TV+Cost_Internet+GRP_NewPaper+GRP_Internet+GRP_TV,
         data = MMM_nonnumeric)

summary(mod3)

mod4<-lm(DEMAND ~ CPI+CCI+Unit_Price +Supply_Data+SALES+Cost_SMS+
           Cost_TV+Cost_Internet+GRP_NewPaper+GRP_Internet+GRP_TV,
         data = MMM_nonnumeric)

summary(mod4)


plot(log(MMM_data$Cost_SMS) ~ MMM_data$DEMAND)

acf2(MMM_data$Cost_SMS)
