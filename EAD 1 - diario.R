
library(tidyverse) ### para manipulação e tratamento dos dados
library(readxl) ## para ler a base de dados
#library(dygraphs) ##
library(GGally) ## para a análise exploratória
#install.packages("tsibble")
library(tsibble) # para trabalhar com séries temporais
library(corrplot) # para analisar a correlação entre as variaveis
#install.packages("astsa")
library(astsa) # para analisar séries temporais
library(MASS) # para trabalhar com transformação BoxCox
library(anytime)



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



MMM_nonnumeric <- MMM_data[,-c(1,2)] ### apenas valores númericos
matrixcor <-cor(MMM_nonnumeric)

MMM_midia <- MMM_data[, -c(1,4,5,6)] ### sem as informações macroeconomicas
head(MMM_midia)

MMM_cost <- MMM_data[, -c(1,4,5,6,15,16,17,18,19)] # sem os dados de GRP
head(MMM_cost)


### analisando correlação e dispersão das variáveis
pairs(MMM_nonnumeric) 
ggcorr(MMM_nonnumeric)
corrplot(MMM_nonnumeric)
ggpairs(MMM_cost)

tb <- MMM_nonnumeric %>% 
  gather(key = "variable", value = "value", -DEMAND)

ggplot(tb,
       mapping = aes( x= value, y = DEMAND)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  scale_y_sqrt()+
  geom_point() +
  geom_smooth(method = "lm")

ggplot(tb,
       mapping = aes( x= value, y = DEMAND)) +
  facet_wrap(facets = ~variable, scale = "free_x") +
  scale_y_log10()+
  geom_point() +
  geom_smooth(method = "lm")

#Graficos de dispersão em relação a demanda



## RESPOSTA ------------------------------------------

ggplot(MMM_data, aes(x = DATE, y = SALES)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Supply_Data)) +
  geom_line()

ggplot(MMM_data, aes(x = DATE, y = Unit_Price)) +
  geom_line()


ggplot(MMM_data, aes(x = DATE, y = DEMAND)) +
  geom_line()+
  geom_smooth(method = "loess")

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
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = Cost_SMS)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = Cost_Radio)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = Cost_TV)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = Cost_Internet)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = GRP_NewPaper)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = GRP_SMS)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = GRP_Radio)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = GRP_Internet)) +
  geom_line()+
  geom_smooth(method = "loess")

ggplot(MMM_data, aes(x = DATE, y = GRP_TV)) +
  geom_line()+
  geom_smooth(method = "loess")



### Análise de correlação -------------------------------------

#### PRECISA DE TRANSFORMAÇÃO?????
plot(log(MMM_data$Cost_SMS) ~ MMM_data$DEMAND)




# -------------------------------------------------------------------------

### Todas as variáveis

######## SEM TRANSFORMAÇÃO

mod<-lm(DEMAND ~ ., data = MMM_nonnumeric)

summary(mod)


mod1<-lm(DEMAND ~ CPI+CCI+PPI +Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Newspaper+Cost_Radio+
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

### variaveis midia

##### SEM  TRANSFORMAÇÃO
mod_midia<-lm(DEMAND ~ Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Newspaper+Cost_Radio+
           Cost_TV+Cost_Internet,
         data = MMM_nonnumeric)

summary(mod_midia)

mod1_midia<-lm(DEMAND ~ Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Radio+
                Cost_TV+Cost_Internet,
              data = MMM_nonnumeric)

summary(mod1_midia)

mod2_midia<-lm(DEMAND ~ Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_TV+Cost_Internet,
               data = MMM_nonnumeric)

summary(mod2_midia)

### Os investimentos que mais influenciam na demanda são os de SMS, TV e Internet. Radio e Jornal foram retirados no ajuste


### variveis midia + macro economicas
mod_MACRO<-lm(DEMAND ~ CCI + CPI + PPI + Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Newspaper+Cost_Radio+
                Cost_TV+Cost_Internet,
              data = MMM_nonnumeric)
summary(mod_MACRO)



mod_MACRO1<-lm(DEMAND ~ CCI + CPI + PPI + Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Radio+
                Cost_TV+Cost_Internet,
              data = MMM_nonnumeric)
summary(mod_MACRO1)


mod_MACRO2<-lm(DEMAND ~ CCI + CPI + Unit_Price +Supply_Data+SALES+Cost_SMS+Cost_Radio+
                 Cost_TV+Cost_Internet,
               data = MMM_nonnumeric)
summary(mod_MACRO2)



mod_MACRO3<-lm(DEMAND ~ CCI + CPI + Unit_Price +Supply_Data+SALES+Cost_SMS+
                 Cost_TV+Cost_Internet,
               data = MMM_nonnumeric)
summary(mod_MACRO3)


## 

# AUTOCORRELAÇÃO ----------------------------------------------------------

## investimento
acf2(MMM_cost$Cost_SMS)## 
acf2(MMM_cost$Cost_Newspaper) 
acf2(MMM_cost$Cost_Radio)
acf2(MMM_cost$Cost_Internet)
acf2(MMM_cost$Cost_TV)



acf2(MMM_cost$DEMAND)


# acf2(MMM_cost$CPI)
# acf2(MMM_data$CCI)
# acf2(MMM_data$PPI)
# acf2(MMM_data$Unit_Price)
# acf2(MMM_data$Supply_Data)
# acf2(MMM_data$SALES)


?acf2

