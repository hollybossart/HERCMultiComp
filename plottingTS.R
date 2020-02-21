
library(TTR)
library(tidyverse)


data = read.csv("Data/OHLCprices.csv")
company_names = data.frame(unique(data$TICKER))
names(company_names)[1] <- "Company"


## now that we have all of the data, let's calculate volatility

