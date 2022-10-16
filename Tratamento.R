library(tidyverse)



MMM_data <- read_excel("MMM_data_excel.xlsx", 
                                 col_types = c("text", "date", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric", 
                                                    "numeric", "numeric", "numeric","numeric"))

View(MMM_data)

head(MMM_data)
