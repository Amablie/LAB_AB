### ANALISE DESCRITIVA


## INSTALANDO SIMULADOR DE RFM
install.packages("devtools")
devtools::install_github("DoktorMike/dammmdatagen")

library(dammmdatagen)


## BASE EXEMPLO
mydf <- generateCovariatesData()
head(mydf)
