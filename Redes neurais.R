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

### Devemos manter os dados originais ou transformamos em mensal tbm?
### https://blogs.rstudio.com/ai/posts/2017-12-20-time-series-forecasting-with-recurrent-neural-networks/
### Se a resposta for sim, devemos transformar os dados novamente dividindo por 10000 e tirando as colunas não númericas



