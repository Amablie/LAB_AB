### ANALISE DESCRITIVA


## INSTALANDO SIMULADOR DE RFM
#install.packages("devtools")
#devtools::install_github("DoktorMike/dammmdatagen")

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



entrytocolname <- function(x) tibble::tibble(rowSums(ret[["effects"]][[x]])) %>% setNames(x)
Reduce(dplyr::bind_cols, lapply(names(ret[["effects"]]), entrytocolname)) %>%
  dplyr::mutate(date = dates) %>%
  tidyr::pivot_longer(-date, names_to = "variable", values_to = "value") %>%
  ggplot2::ggplot(ggplot2::aes(x = date, y = value, fill = variable)) +
  ggplot2::geom_bar(stat = "identity") + ggplot2::theme_minimal() +
  ggplot2::ylab("Units sold") +
  ggplot2::xlab("")



generateCompetitorData(fromDate = Sys.Date() - 30, toDate = Sys.Date()) %>%
  gather("competitor", "spend", -"date") %>%
  ggplot(aes(y = spend, x = date, fill = competitor)) +
  geom_bar(stat = "identity", position = position_stack()) +
  theme_minimal() +
  scale_y_continuous(labels = dollar_format(prefix = "kr. "))


generateMacroData(fromDate = Sys.Date() - 30, toDate = Sys.Date()) %>%
  gather("indicator", "value", -"date") %>%
  ggplot(aes(y = value, x = date, color = indicator)) +
  geom_line(size = 1.5) +
  theme_minimal()


generateEventData(Sys.Date() - 265, Sys.Date()) %>%
  gather(type, value, -date) %>%
  ggplot(aes(y = value, x = date, fill = type)) +
  geom_bar(stat = "identity") +
  theme_minimal()

generateEventData(Sys.Date() - 265, Sys.Date(), freq = 0.1) %>%
  gather(type, value, -date) %>%
  ggplot(aes(y = value, x = date, fill = type)) +
  geom_bar(stat = "identity") +
  theme_minimal()

mydflist <- generateOnlineData(Sys.Date() - 30, Sys.Date())
mydflist[["impression"]] %>%
  gather(type, impression, -date) %>%
  ggplot(aes(y = impression, x = date, fill = type)) +
  geom_bar(stat = "identity") +
  theme_minimal()


x<-12



y <- 11
