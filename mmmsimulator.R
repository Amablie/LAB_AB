### ANALISE DESCRITIVA


## INSTALANDO SIMULADOR DE RFM
install.packages("devtools")
devtools::install_github("DoktorMike/dammmdatagen")

library(dammmdatagen)


## BASE EXEMPLO
mydf <- generateCovariatesData()
head(mydf)

library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
ret <- generateRetailData()
#> Joining, by = "date"
#> Joining, by = "date"
#> Joining, by = "date"
#> New names:
#> * product_a -> product_a...7
#> * product_b -> product_b...8
#> * product_c -> product_c...9
#> * product_a -> product_a...10
#> * product_b -> product_b...11
#> * ...
dates <- ret[["covariates"]][["Macro"]][["date"]]
qplot(dates, ret[["response"]]) + geom_line() + ylim(0, NA)
