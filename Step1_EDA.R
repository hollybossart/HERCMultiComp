#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objectives : This program computes volatilityes and their changepoints.                       ]*#
#*[ Last update: 02/28/2020                                                                       ]*#
#*[ Author     : Holly Bossart & Jaechoul Lee                                                     ]*#
#*[-----------------------------------------------------------------------------------------------]*#

### Setup data input and output directories
WD.lib <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Codes/")
WD.inp <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Data/")
WD.out <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Work/")

### Load the proposed GA code and Poisson likelihood funtions
source(file=paste(WD.lib,"lib_volatilityfunctions.R",sep=""))

### Required packages
library(TTR)
library(tidyverse)
library(changepoint)                                    # PELT method (Killick, Fearnhead, & Eckley, JASA 2012)
library(ecp)                                            # e.divisive (Matteson & James, JASA 2014) &
                                                        # e.cp3o (James & Matteson, arXiv 2015)
### Data import
data.OHLC <- read.csv(file=paste(WD.inp,"OHLCprices.csv",sep=""))
data.OHLC[1:10,]

comnames <- as.data.frame(unique(data.OHLC$COMNAM))
colnames(comnames) <- "Company"
comnames                                                # 30 companies

### Indivisual company data
data.oracle     <- filter(data.OHLC, COMNAM == 'ORACLE CORP')
data.microsoft  <- filter(data.OHLC, COMNAM == 'MICROSOFT CORP')
data.exxon      <- filter(data.OHLC, COMNAM == 'EXXON MOBIL CORP')
data.gm         <- filter(data.OHLC, COMNAM == 'GENERAL MOTORS CO')
data.ibm        <- filter(data.OHLC, COMNAM == 'INTERNATIONAL BUSINESS MACHS COR')
data.facebook   <- filter(data.OHLC, COMNAM == 'FACEBOOK INC')
data.chevron    <- filter(data.OHLC, COMNAM == 'CHEVRON CORP NEW')
data.apple      <- filter(data.OHLC, COMNAM == 'APPLE INC')
data.alibaba    <- filter(data.OHLC, COMNAM == 'ALIBABA GROUP HOLDING LTD')
data.pg         <- filter(data.OHLC, COMNAM == 'PROCTER & GAMBLE CO')
data.pfizer     <- filter(data.OHLC, COMNAM == 'PFIZER INC')
data.johnson    <- filter(data.OHLC, COMNAM == 'JOHNSON & JOHNSON')
data.disney     <- filter(data.OHLC, COMNAM == 'DISNEY WALT CO')
data.wellsfargo <- filter(data.OHLC, COMNAM == 'WELLS FARGO & CO NEW')
data.jpmorgan   <- filter(data.OHLC, COMNAM == 'JPMORGAN CHASE & CO')
data.walmart    <- filter(data.OHLC, COMNAM == 'WALMART INC')
data.intel      <- filter(data.OHLC, COMNAM == 'INTEL CORP')
data.bankofa    <- filter(data.OHLC, COMNAM == 'BANK OF AMERICA CORP')
data.verizon    <- filter(data.OHLC, COMNAM == 'VERIZON COMMUNICATIONS INC')
data.att        <- filter(data.OHLC, COMNAM == 'A T & T INC')
data.homedep    <- filter(data.OHLC, COMNAM == 'HOME DEPOT INC')
data.citi       <- filter(data.OHLC, COMNAM == 'CITIGROUP INC')
data.amazon     <- filter(data.OHLC, COMNAM == 'AMAZON COM INC')
data.chinamob   <- filter(data.OHLC, COMNAM == 'CHINA MOBILE LTD')
data.taiwan     <- filter(data.OHLC, COMNAM == 'TAIWAN SEMICONDUCTOR MFG CO LTD')
data.novartis   <- filter(data.OHLC, COMNAM == 'NOVARTIS A G')
data.visa       <- filter(data.OHLC, COMNAM == 'VISA INC')
data.unhealth   <- filter(data.OHLC, COMNAM == 'UNITEDHEALTH GROUP INC')
data.busch      <- filter(data.OHLC, COMNAM == 'ANHEUSER BUSCH INBEV SA NV')
data.netflix    <- filter(data.OHLC, COMNAM == 'NETFLIX INC')

### Numbers for data
n.obs <- nrow(data.oracle)                              # no of observations
n.yrs <- 5                                              # no of years
n.day <- n.obs/n.yrs                                    # no of days per year
n.day                                                   # 251.6

### Closing price time series
p.t_oracle     <- ts(data.oracle$PRC,     start=c(2015,1,1), frequency=n.day)
p.t_microsoft  <- ts(data.microsoft$PRC,  start=c(2015,1,1), frequency=n.day)
p.t_exxon      <- ts(data.exxon$PRC,      start=c(2015,1,1), frequency=n.day)


p.t_homedep    <- ts(data.homedep$PRC,    start=c(2015,1,1), frequency=n.day)











### Time plot of closing prices over 1/01/2015-12/31/2019
summary(p.t_oracle)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_oracle,ylim=c(20,80),xlab="Year",main="Oracle Closing Prices 1/01/2015-12/31/2019")

summary(p.t_microsoft)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_microsoft,ylim=c(20,180),xlab="Year",main="Microsoft Closing Prices 1/01/2015-12/31/2019")

summary(p.t_exxon)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_exxon,ylim=c(40,120),xlab="Year",main="Exxon Closing Prices 1/01/2015-12/31/2019")



summary(p.t_homedep)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_homedep,ylim=c(100,250),xlab="Year",main="Home Depot Closing Prices 1/01/2015-12/31/2019")




### Garman & Klass volatility series using garmanklassTA: v.t
v.t_oracle     <- garmanklassTA(open =data.oracle$OPENPRC,
                                high =data.oracle$ASKHI,
                                low  =data.oracle$BIDLO,
                                close=data.oracle$PRC)

v.t_microsoft  <- garmanklassTA(open =data.microsoft$OPENPRC,
                                high =data.microsoft$ASKHI,
                                low  =data.microsoft$BIDLO,
                                close=data.microsoft$PRC)



v.t_homedep    <- garmanklassTA(open =data.homedep$OPENPRC,
                                high =data.homedep$ASKHI,
                                low  =data.homedep$BIDLO,
                                close=data.homedep$PRC)




length(p.t_oracle)                               # 1258
length(v.t_oracle)                               # 1257 [CAUTION] We might need to keep an eye on this for changepoints

summary(v.t_oracle)
summary(v.t_microsoft)

summary(v.t_homedep)


### Garman & Klass volatility series using TTR volatility: w.t
w.t_oracle     <- ts(volatility(OHLC=data.oracle[,c(6,4,3,5)],n=1,calc="garman.klass",N=260),
                     start=c(2015,1,1),frequency=n.day)

w.t_microsoft  <- ts(volatility(OHLC=data.microsoft[,c(6,4,3,5)],n=1,calc="garman.klass",N=260),
                     start=c(2015,1,1),frequency=n.day)



w.t_homedep    <- ts(volatility(OHLC=data.homedep[,c(6,4,3,5)],n=1,calc="garman.klass",N=260),
                     start=c(2015,1,1),frequency=n.day)



length(w.t_oracle)                               # 1258 [CAUTION] This does not produce missing when n=1




### Volitility time plots (v.t and w.t)          # [Q] which one should we use?
summary(w.t_oracle)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(v.t_oracle,ylim=c(0,2),
        xlab="Year",ylab="GK volatility",main="Oracle Volatility 1/02/2015-12/31/2019")
lines(w.t_oracle,col="blue")

summary(w.t_microsoft)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(v.t_microsoft,ylim=c(0,5),
        xlab="Year",ylab="GK volatility",main="Microsoft Volatility 1/02/2015-12/31/2019")
lines(w.t_microsoft,col="blue")



summary(w.t_homedep)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(v.t_homedep,ylim=c(0,18),
        xlab="Year",ylab="GK volatility",main="Home Depot Volatility 1/02/2015-12/31/2019")
lines(w.t_homedep,col="blue")




### Sample ACF
dev.new()
par(mfrow=c(2,1),mex=0.75)
acf(v.t_oracle,lag.max=100,na.action=na.pass)
acf(w.t_oracle,lag.max=100,na.action=na.pass)



### PELT method (change in mean)
cpt.PELT_v.oracle <- cpt.mean(v.t_oracle,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.oracle)                                # no changepoints detected

cpt.PELT_w.oracle <- cpt.mean(w.t_oracle,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_w.oracle)                                # no changepoints detected

cpt.PELT_v.microsoft <- cpt.mean(v.t_microsoft,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.microsoft)

t.PELT_v.microsoft <- cpts(cpt.PELT_v.microsoft)+1        # add 1 to keep changepoint time meaning same
t.PELT_v.microsoft                                        # 775

data.microsoft[t.PELT_v.microsoft,]                       # date 20180130

cpt.PELT_w.microsoft <- cpt.mean(w.t_microsoft,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_w.microsoft)                             # no changepoints detected




cpt.PELT_v.homedep <- cpt.mean(v.t_homedep,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.homedep)

t.PELT_w.homedep <- cpts(cpt.PELT_v.homedep)+1            # add 1 to keep changepoint time meaning same
t.PELT_w.homedep                                          # 161  162  774  783  946 1005

data.microsoft[t.PELT_w.homedep,]

cpt.PELT_w.homedep <- cpt.mean(w.t_homedep,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_w.homedep)                               # no changepoints detected



dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(v.t_microsoft,ylim=c(0,5),
        xlab="Year",ylab="GK volatility",main="Microsoft Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_microsoft)[t.PELT_v.microsoft],col="red",lty=2) 

dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(v.t_homedep,ylim=c(0,18),
        xlab="Year",ylab="GK volatility",main="Home Depot Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_homedep)[t.PELT_w.homedep],col="red",lty=2)



 

