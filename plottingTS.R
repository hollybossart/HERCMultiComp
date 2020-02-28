## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(knitr)
library(TTR)
library(tidyverse)

source("R/volatilityfunctions.R")

data = read.csv("Data/OHLCprices2.csv")
ticker = data.frame(unique(data$TICKER))
names(ticker)[1] <- "Company"



## ---- echo=FALSE---------------------------------------------------------
data$COMNAM[data$COMNAM == 'WAL MART STORES INC'] <- 'WALMART INC'
comnames <- as.data.frame(unique(data$COMNAM))


## ---- echo=FALSE---------------------------------------------------------
oracle <- filter(data, data$COMNAM == 'ORACLE CORP')
microsoft <- filter(data, data$COMNAM == 'MICROSOFT CORP')
exxon <- filter(data, data$COMNAM == 'EXXON MOBIL CORP')
gm <- filter(data, data$COMNAM == 'GENERAL MOTORS CO')
ibm <- filter(data, data$COMNAM == 'INTERNATIONAL BUSINESS MACHS COR')
facebook <- filter(data, data$COMNAM == 'FACEBOOK INC')
chevron <- filter(data, data$COMNAM == 'CHEVRON CORP NEW')
apple <- filter(data, data$COMNAM == 'APPLE INC')
alibaba <- filter(data, data$COMNAM == 'ALIBABA GROUP HOLDING LTD')
pg <- filter(data, data$COMNAM == 'PROCTER & GAMBLE CO')
pfizer <- filter(data, data$COMNAM == 'PFIZER INC')
johnson <- filter(data, data$COMNAM == 'JOHNSON & JOHNSON')
disney <- filter(data, data$COMNAM == 'DISNEY WALT CO')
wellsfargo <- filter(data, data$COMNAM == 'WELLS FARGO & CO NEW')
jpmorgan <- filter(data, data$COMNAM == 'JPMORGAN CHASE & CO')
walmart <- filter(data, data$COMNAM == 'WALMART INC')
intel <- filter(data, data$COMNAM == 'INTEL CORP')
bankofa <- filter(data, data$COMNAM == 'BANK OF AMERICA CORP')
verizon <- filter(data, data$COMNAM == 'VERIZON COMMUNICATIONS INC')
att <- filter(data, data$COMNAM == 'A T & T INC')
homedep <- filter(data, data$COMNAM == 'HOME DEPOT INC')
citi <- filter(data, data$COMNAM == 'CITIGROUP INC')
amazon <- filter(data, data$COMNAM == 'AMAZON COM INC')
chinamob <- filter(data, data$COMNAM == 'CHINA MOBILE LTD')
taiwan <- filter(data, data$COMNAM == 'TAIWAN SEMICONDUCTOR MFG CO LTD')
novartis <- filter(data, data$COMNAM == 'NOVARTIS A G')
visa <- filter(data, data$COMNAM == 'VISA INC')
unhealth <- filter(data, data$COMNAM == 'UNITEDHEALTH GROUP INC')
busch <- filter(data, data$COMNAM == 'ANHEUSER BUSCH INBEV SA NV')
netflix <- filter(data, data$COMNAM == 'NETFLIX INC')



## ---- echo=FALSE---------------------------------------------------------
# making the data into time series plots
oracle.p.t <- ts(oracle$PRC, start = c(2015,1,1), frequency = 251.6)
summary(oracle.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(oracle.p.t, ylim=c(30,75),xlab="2015-2019",main="Oracle Closing Prices 01/01/2015-12/31/2019")



## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(oracle$OPENPRC, oracle$ASKHI, oracle$BIDLO, oracle$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(oracle$OPENPRC, oracle$ASKHI, oracle$BIDLO, oracle$PRC), ylab="GK volatility", main = "Oracle Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
oracle.v <- garmanklassTA(oracle$OPENPRC, oracle$ASKHI, oracle$BIDLO, oracle$PRC)
oracle.v <- ts(oracle.v, start = c(2015,1,2), frequency = 251.4)

temp <- oracle[-1,]
vol <- as.data.frame(oracle.v) 
temp <- cbind(temp$date, vol)
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/oracle.csv", row.names = FALSE)
rm(temp)
rm(vol)

## ---- echo=FALSE---------------------------------------------------------
microsoft.p.t <- ts(microsoft$PRC, start = c(2015,1,1), frequency = 251.6)
summary(microsoft.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(microsoft.p.t, ylim=c(30,175),xlab="2015-2019",main="microsoft Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(microsoft$OPENPRC, microsoft$ASKHI, microsoft$BIDLO, microsoft$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(microsoft$OPENPRC, microsoft$ASKHI, microsoft$BIDLO, microsoft$PRC), ylab="GK volatility", main = "microsoft Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
microsoft.v <- garmanklassTA(microsoft$OPENPRC, microsoft$ASKHI, microsoft$BIDLO, microsoft$PRC)
microsoft.v <- ts(microsoft.v, start = c(2015,1,2), frequency = 251.4)
temp <- microsoft[-1,]
temp <- cbind(temp$date, as.data.frame(microsoft.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/microsoft.csv", row.names = FALSE)
rm(temp)



## ---- echo=FALSE---------------------------------------------------------
exxon.p.t <- ts(exxon$PRC, start = c(2015,1,1), frequency = 251.6)
summary(exxon.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(exxon.p.t, ylim=c(60,100),xlab="2015-2019",main="exxon Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(exxon$OPENPRC, exxon$ASKHI, exxon$BIDLO, exxon$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(exxon$OPENPRC, exxon$ASKHI, exxon$BIDLO, exxon$PRC), ylab="GK volatility", main = "exxon Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
exxon.v <- garmanklassTA(exxon$OPENPRC, exxon$ASKHI, exxon$BIDLO, exxon$PRC)
exxon.v <- ts(exxon.v, start = c(2015,1,2), frequency = 251.4)
temp <- cbind(exxon[-1,]$date, as.data.frame(exxon.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/exxon.csv", row.names = FALSE)
rm(temp)



## ---- echo=FALSE---------------------------------------------------------
gm.p.t <- ts(gm$PRC, start = c(2015,1,1), frequency = 251.6)
summary(gm.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(gm.p.t, ylim=c(20,55),xlab="2015-2019",main="gm Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(gm$OPENPRC, gm$ASKHI, gm$BIDLO, gm$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(gm$OPENPRC, gm$ASKHI, gm$BIDLO, gm$PRC), ylab="GK volatility", main = "gm Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
gm.v <- garmanklassTA(gm$OPENPRC, gm$ASKHI, gm$BIDLO, gm$PRC)
gm.v <- ts(gm.v, start = c(2015,1,2), frequency = 251.4)

temp <- cbind(gm[-1,]$date, as.data.frame(gm.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/gm.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
ibm.p.t <- ts(ibm$PRC, start = c(2015,1,1), frequency = 251.6)
summary(ibm.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(ibm.p.t, ylim=c(100,190),xlab="2015-2019",main="ibm Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(ibm$OPENPRC, ibm$ASKHI, ibm$BIDLO, ibm$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(ibm$OPENPRC, ibm$ASKHI, ibm$BIDLO, ibm$PRC), ylab="GK volatility", main = "ibm Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
ibm.v <- garmanklassTA(ibm$OPENPRC, ibm$ASKHI, ibm$BIDLO, ibm$PRC)
ibm.v <- ts(ibm.v, start = c(2015,1,2), frequency = 251.4)

temp <- cbind(ibm[-1,]$date, as.data.frame(ibm.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/ibm.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
facebook.p.t <- ts(facebook$PRC, start = c(2015,1,1), frequency = 251.6)
summary(facebook.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(facebook.p.t, ylim=c(70,230),xlab="2015-2019",main="facebook Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(facebook$OPENPRC, facebook$ASKHI, facebook$BIDLO, facebook$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(facebook$OPENPRC, facebook$ASKHI, facebook$BIDLO, facebook$PRC), ylab="GK volatility", main = "facebook Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
facebook.v <- garmanklassTA(facebook$OPENPRC, facebook$ASKHI, facebook$BIDLO, facebook$PRC)
facebook.v <- ts(facebook.v, start = c(2015,1,2), frequency = 251.4)

temp <- cbind(facebook[-1,]$date, as.data.frame(facebook.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/facebook.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
chevron.p.t <- ts(chevron$PRC, start = c(2015,1,1), frequency = 251.6)
summary(chevron.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(chevron.p.t, ylim=c(60,145),xlab="2015-2019",main="chevron Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(chevron$OPENPRC, chevron$ASKHI, chevron$BIDLO, chevron$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(chevron$OPENPRC, chevron$ASKHI, chevron$BIDLO, chevron$PRC), ylab="GK volatility", main = "chevron Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
chevron.v <- garmanklassTA(chevron$OPENPRC, chevron$ASKHI, chevron$BIDLO, chevron$PRC)
chevron.v <- ts(chevron.v, start = c(2015,1,2), frequency = 251.4)

temp <- cbind(chevron[-1,]$date, as.data.frame(chevron.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/chevron.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
apple.p.t <- ts(apple$PRC, start = c(2015,1,1), frequency = 251.6)
summary(apple.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(apple.p.t, ylim=c(80,300),xlab="2015-2019",main="apple Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(apple$OPENPRC, apple$ASKHI, apple$BIDLO, apple$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(apple$OPENPRC, apple$ASKHI, apple$BIDLO, apple$PRC), ylab="GK volatility", main = "apple Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
apple.v <- garmanklassTA(apple$OPENPRC, apple$ASKHI, apple$BIDLO, apple$PRC)
apple.v <- ts(apple.v, start = c(2015,1,2), frequency = 251.4)
temp <- cbind(apple[-1,]$date, as.data.frame(apple.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/apple.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
alibaba.p.t <- ts(alibaba$PRC, start = c(2015,1,1), frequency = 251.6)
summary(alibaba.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(alibaba.p.t, ylim=c(50,220),xlab="2015-2019",main="alibaba Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(alibaba.v <- garmanklassTA(alibaba$OPENPRC, alibaba$ASKHI, alibaba$BIDLO, alibaba$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(alibaba$OPENPRC, alibaba$ASKHI, alibaba$BIDLO, alibaba$PRC), ylab="GK volatility", main = "alibaba Volatility 01/02/2015-12/31/2019")

temp <- cbind(alibaba[-1,]$date, as.data.frame(alibaba.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/alibaba.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
pg.p.t <- ts(pg$PRC, start = c(2015,1,1), frequency = 251.6)
summary(pg.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(pg.p.t, ylim=c(60,130),xlab="2015-2019",main="pg Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(pg.v <- garmanklassTA(pg$OPENPRC, pg$ASKHI, pg$BIDLO, pg$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(pg$OPENPRC, pg$ASKHI, pg$BIDLO, pg$PRC), ylab="GK volatility", main = "pg Volatility 01/02/2015-12/31/2019")

temp <- cbind(pg[-1,]$date, as.data.frame(pg.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/pg.csv", row.names = FALSE)
rm(temp)

## ---- echo=FALSE---------------------------------------------------------
pfizer.p.t <- ts(pfizer$PRC, start = c(2015,1,1), frequency = 251.6)
summary(pfizer.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(pfizer.p.t, ylim=c(25,50),xlab="2015-2019",main="pfizer Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(pfizer.v <- garmanklassTA(pfizer$OPENPRC, pfizer$ASKHI, pfizer$BIDLO, pfizer$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(pfizer$OPENPRC, pfizer$ASKHI, pfizer$BIDLO, pfizer$PRC), ylab="GK volatility", main = "pfizer Volatility 01/02/2015-12/31/2019")

temp <- cbind(pfizer[-1,]$date, as.data.frame(pfizer.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/pfizer.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
johnson.p.t <- ts(johnson$PRC, start = c(2015,1,1), frequency = 251.6)
summary(johnson.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(johnson.p.t, ylim=c(85,150),xlab="2015-2019",main="johnson&johnson Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(johnson.v <- garmanklassTA(johnson$OPENPRC, johnson$ASKHI, johnson$BIDLO, johnson$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(johnson$OPENPRC, johnson$ASKHI, johnson$BIDLO, johnson$PRC), ylab="GK volatility", main = "johnson Volatility 01/02/2015-12/31/2019")

temp <- cbind(johnson[-1,]$date, as.data.frame(johnson.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/johnson.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
disney.p.t <- ts(disney$PRC, start = c(2015,1,1), frequency = 251.6)
summary(disney.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(disney.p.t, ylim=c(80,160),xlab="2015-2019",main="disney Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(disney.v <- garmanklassTA(disney$OPENPRC, disney$ASKHI, disney$BIDLO, disney$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(disney$OPENPRC, disney$ASKHI, disney$BIDLO, disney$PRC), ylab="GK volatility", main = "disney Volatility 01/02/2015-12/31/2019")

temp <- cbind(disney[-1,]$date, as.data.frame(disney.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/disney.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
wellsfargo.p.t <- ts(wellsfargo$PRC, start = c(2015,1,1), frequency = 251.6)
summary(wellsfargo.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(wellsfargo.p.t, ylim=c(40,70),xlab="2015-2019",main="wellsfargo Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(wellsfargo.v <- garmanklassTA(wellsfargo$OPENPRC, wellsfargo$ASKHI, wellsfargo$BIDLO, wellsfargo$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(wellsfargo$OPENPRC, wellsfargo$ASKHI, wellsfargo$BIDLO, wellsfargo$PRC), ylab="GK volatility", main = "wellsfargo Volatility 01/02/2015-12/31/2019")

temp <- cbind(wellsfargo[-1,]$date, as.data.frame(wellsfargo.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/wellsfargo.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
jpmorgan.p.t <- ts(jpmorgan$PRC, start = c(2015,1,1), frequency = 251.6)
summary(jpmorgan.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(jpmorgan.p.t, ylim=c(50,150),xlab="2015-2019",main="jpmorgan Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(jpmorgan.v <- garmanklassTA(jpmorgan$OPENPRC, jpmorgan$ASKHI, jpmorgan$BIDLO, jpmorgan$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(jpmorgan$OPENPRC, jpmorgan$ASKHI, jpmorgan$BIDLO, jpmorgan$PRC), ylab="GK volatility", main = "jpmorgan Volatility 01/02/2015-12/31/2019")

temp <- cbind(jpmorgan[-1,]$date, as.data.frame(jpmorgan.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/jpmorgan.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
walmart.p.t <- ts(walmart$PRC, start = c(2015,1,1), frequency = 251.6)
summary(walmart.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(walmart.p.t, ylim=c(50,130),xlab="2015-2019",main="walmart Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(walmart.v <- garmanklassTA(walmart$OPENPRC, walmart$ASKHI, walmart$BIDLO, walmart$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(walmart$OPENPRC, walmart$ASKHI, walmart$BIDLO, walmart$PRC), ylab="GK volatility", main = "walmart Volatility 01/02/2015-12/31/2019")

temp <- cbind(walmart[-1,]$date, as.data.frame(walmart.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/walmart.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
intel.p.t <- ts(intel$PRC, start = c(2015,1,1), frequency = 251.6)
summary(intel.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(intel.p.t, ylim=c(20,65),xlab="2015-2019",main="intel Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(intel.v <- garmanklassTA(intel$OPENPRC, intel$ASKHI, intel$BIDLO, intel$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(intel$OPENPRC, intel$ASKHI, intel$BIDLO, intel$PRC), ylab="GK volatility", main = "intel Volatility 01/02/2015-12/31/2019")

temp <- cbind(intel[-1,]$date, as.data.frame(intel.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/intel.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
bankofa.p.t <- ts(bankofa$PRC, start = c(2015,1,1), frequency = 251.6)
summary(bankofa.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(bankofa.p.t, ylim=c(10,40),xlab="2015-2019",main="bankofa Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(bankofa.v <- garmanklassTA(bankofa$OPENPRC, bankofa$ASKHI, bankofa$BIDLO, bankofa$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(bankofa$OPENPRC, bankofa$ASKHI, bankofa$BIDLO, bankofa$PRC), ylab="GK volatility", main = "bankofa Volatility 01/02/2015-12/31/2019")

temp <- cbind(bankofa[-1,]$date, as.data.frame(bankofa.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/bankofa.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
verizon.p.t <- ts(verizon$PRC, start = c(2015,1,1), frequency = 251.6)
summary(verizon.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(verizon.p.t, ylim=c(40,65),xlab="2015-2019",main="verizon Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(verizon.v <- garmanklassTA(verizon$OPENPRC, verizon$ASKHI, verizon$BIDLO, verizon$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(verizon$OPENPRC, verizon$ASKHI, verizon$BIDLO, verizon$PRC), ylab="GK volatility", main = "verizon Volatility 01/02/2015-12/31/2019")

temp <- cbind(verizon[-1,]$date, as.data.frame(verizon.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/verizon.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
att.p.t <- ts(att$PRC, start = c(2015,1,1), frequency = 251.6)
summary(att.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(att.p.t, ylim=c(25,45),xlab="2015-2019",main="att Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(att.v <- garmanklassTA(att$OPENPRC, att$ASKHI, att$BIDLO, att$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(att$OPENPRC, att$ASKHI, att$BIDLO, att$PRC), ylab="GK volatility", main = "att Volatility 01/02/2015-12/31/2019")

temp <- cbind(att[-1,]$date, as.data.frame(att.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/att.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
homedep.p.t <- ts(homedep$PRC, start = c(2015,1,1), frequency = 251.6)
summary(homedep.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(homedep.p.t, ylim=c(100,240),xlab="2015-2019",main="homedep Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(homedep.v <- garmanklassTA(homedep$OPENPRC, homedep$ASKHI, homedep$BIDLO, homedep$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(homedep$OPENPRC, homedep$ASKHI, homedep$BIDLO, homedep$PRC), ylab="GK volatility", main = "homedep Volatility 01/02/2015-12/31/2019")

temp <- cbind(homedep[-1,]$date, as.data.frame(homedep.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/homedep.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
citi.p.t <- ts(citi$PRC, start = c(2015,1,1), frequency = 251.6)
summary(citi.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(citi.p.t, ylim=c(30,85),xlab="2015-2019",main="citi Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(citi.v <- garmanklassTA(citi$OPENPRC, citi$ASKHI, citi$BIDLO, citi$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(citi$OPENPRC, citi$ASKHI, citi$BIDLO, citi$PRC), ylab="GK volatility", main = "citi Volatility 01/02/2015-12/31/2019")
temp <- cbind(citi[-1,]$date, as.data.frame(citi.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/ibm.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
amazon.p.t <- ts(amazon$PRC, start = c(2015,1,1), frequency = 251.6)
summary(amazon.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(amazon.p.t, ylim=c(280,2040),xlab="2015-2019",main="amazon Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(amazon.v <- garmanklassTA(amazon$OPENPRC, amazon$ASKHI, amazon$BIDLO, amazon$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(amazon$OPENPRC, amazon$ASKHI, amazon$BIDLO, amazon$PRC), ylab="GK volatility", main = "amazon Volatility 01/02/2015-12/31/2019")

temp <- cbind(amazon[-1,]$date, as.data.frame(amazon.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/amazon.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
chinamob.p.t <- ts(chinamob$PRC, start = c(2015,1,1), frequency = 251.6)
summary(chinamob.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(chinamob.p.t, ylim=c(35,76),xlab="2015-2019",main="China Mobile Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(chinamob.v <- garmanklassTA(chinamob$OPENPRC, chinamob$ASKHI, chinamob$BIDLO, chinamob$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(chinamob$OPENPRC, chinamob$ASKHI, chinamob$BIDLO, chinamob$PRC), ylab="GK volatility", main = "chinamob Volatility 01/02/2015-12/31/2019")

temp <- cbind(chinamob[-1,]$date, as.data.frame(chinamob.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/chinamob.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
taiwan.p.t <- ts(taiwan$PRC, start = c(2015,1,1), frequency = 251.6)
summary(taiwan.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(taiwan.p.t, ylim=c(18,60),xlab="2015-2019",main="Taiwan Semiconductors Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(taiwan.v <- garmanklassTA(taiwan$OPENPRC, taiwan$ASKHI, taiwan$BIDLO, taiwan$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(taiwan$OPENPRC, taiwan$ASKHI, taiwan$BIDLO, taiwan$PRC), ylab="GK volatility", main = "taiwan Volatility 01/02/2015-12/31/2019")

temp <- cbind(taiwan[-1,]$date, as.data.frame(taiwan.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/taiwan.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
novartis.p.t <- ts(novartis$PRC, start = c(2015,1,1), frequency = 251.6)
summary(novartis.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(novartis.p.t, ylim=c(65,110),xlab="2015-2019",main="novartis Closing Prices 01/01/2015-12/31/2019")




## ---- echo=FALSE---------------------------------------------------------
summary(novartis.v <- garmanklassTA(novartis$OPENPRC, novartis$ASKHI, novartis$BIDLO, novartis$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(novartis$OPENPRC, novartis$ASKHI, novartis$BIDLO, novartis$PRC), ylab="GK volatility", main = "novartis Volatility 01/02/2015-12/31/2019")

temp <- cbind(novartis[-1,]$date, as.data.frame(novartis.v))
names(temp) <- c('date', 'volatility')
write.csv(temp, "Data/novartis.csv", row.names = FALSE)
rm(temp)


## ---- echo=FALSE---------------------------------------------------------
visa.p.t <- ts(visa$PRC, start = c(2015,1,1), frequency = 251.6)
summary(visa.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(visa.p.t, ylim=c(63,280),xlab="2015-2019",main="visa Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(visa$OPENPRC, visa$ASKHI, visa$BIDLO, visa$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(visa$OPENPRC, visa$ASKHI, visa$BIDLO, visa$PRC), ylab="GK volatility", main = "visa Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
unhealth.p.t <- ts(unhealth$PRC, start = c(2015,1,1), frequency = 251.6)
summary(unhealth.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(unhealth.p.t, ylim=c(90,300),xlab="2015-2019",main="United Health Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(unhealth$OPENPRC, unhealth$ASKHI, unhealth$BIDLO, unhealth$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(unhealth$OPENPRC, unhealth$ASKHI, unhealth$BIDLO, unhealth$PRC), ylab="GK volatility", main = "unhealth Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
busch.p.t <- ts(busch$PRC, start = c(2015,1,1), frequency = 251.6)
summary(busch.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(busch.p.t, ylim=c(60,135),xlab="2015-2019",main="Busch Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(busch$OPENPRC, busch$ASKHI, busch$BIDLO, busch$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(busch$OPENPRC, busch$ASKHI, busch$BIDLO, busch$PRC), ylab="GK volatility", main = "busch Volatility 01/02/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
netflix.p.t <- ts(netflix$PRC, start = c(2015,1,1), frequency = 251.6)
summary(netflix.p.t)


## ---- echo=FALSE---------------------------------------------------------
par(mfrow=c(1,1), mex=0.75)
plot.ts(netflix.p.t, ylim=c(80,710),xlab="2015-2019",main="netflix Closing Prices 01/01/2015-12/31/2019")


## ---- echo=FALSE---------------------------------------------------------
summary(garmanklassTA(netflix$OPENPRC, netflix$ASKHI, netflix$BIDLO, netflix$PRC))
par(mfrow=c(1,1), mex=0.75)
plot.ts(garmanklassTA(netflix$OPENPRC, netflix$ASKHI, netflix$BIDLO, netflix$PRC), ylab="GK volatility", main = "netflix Volatility 01/02/2015-12/31/2019")


