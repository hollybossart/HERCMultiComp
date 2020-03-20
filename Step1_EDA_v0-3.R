#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objectives : This program computes volatilities and their changepoints.                       ]*#
#*[ Last update: 03/20/2020                                                                       ]*#
#*[ Author     : Holly Bossart & Jaechoul Lee                                                     ]*#
#*[-----------------------------------------------------------------------------------------------]*#

### Setup data input and output directories (Lee)
# WD.lib <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Codes/")
# WD.inp <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Data/")
# WD.out <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Work/")

### Setup data input and output directories (Bossart)
WD.lib <- c("C:/Users/12088/Dropbox/Research/HERCMultiComp/R/")
WD.inp <- c("C:/Users/12088/Dropbox/Research/HERCMultiComp/Data/")
WD.out <- c("C:/Users/12088/Dropbox/Research/HERCMultiComp/Output/")

### Load the proposed GA code and Poisson likelihood funtions
source(file=paste(WD.lib,"lib_volatilityfunctions.R",sep=""))

### Required packages
library(TTR)                                            # Alternate volatility function volatility()
library(tidyverse)                                      # for filter() 
library(changepoint)                                    # PELT method (Killick, Fearnhead, & Eckley, JASA 2012)
library(ecp)                                            # e.divisive (Matteson & James, JASA 2014) &
                                                        # e.cp3o (James & Matteson, arXiv 2015)
### Data import
data.OHLC <- read.csv(file=paste(WD.inp,"OHLCprices.csv",sep=""))
data.OHLC[1:10,]

comnames <- as.data.frame(unique(data.OHLC$COMNAM))
colnames(comnames) <- "Company"
comnames                                                # 30 companies

### Individual company data
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


### Closing price time series for all 30 companies
p.t_oracle     <- ts(data.oracle$PRC,     start=c(2015,1,1), frequency=n.day)
p.t_microsoft  <- ts(data.microsoft$PRC,  start=c(2015,1,1), frequency=n.day)
p.t_exxon      <- ts(data.exxon$PRC,      start=c(2015,1,1), frequency=n.day)
p.t_gm         <- ts(data.gm$PRC,         start=c(2015,1,1), frequency=n.day)
p.t_ibm        <- ts(data.ibm$PRC,        start=c(2015,1,1), frequency=n.day)
p.t_facebook   <- ts(data.facebook$PRC,   start=c(2015,1,1), frequency=n.day)
p.t_chevron    <- ts(data.chevron$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_apple      <- ts(data.apple$PRC,      start=c(2015,1,1), frequency=n.day)
p.t_alibaba    <- ts(data.alibaba$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_pg         <- ts(data.pg$PRC,         start=c(2015,1,1), frequency=n.day)
p.t_pfizer     <- ts(data.pfizer$PRC,     start=c(2015,1,1), frequency=n.day)
p.t_johnson    <- ts(data.johnson$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_disney     <- ts(data.disney$PRC,     start=c(2015,1,1), frequency=n.day)
p.t_wellsfargo <- ts(data.wellsfargo$PRC, start=c(2015,1,1), frequency=n.day)
p.t_jpmorgan   <- ts(data.jpmorgan$PRC,   start=c(2015,1,1), frequency=n.day)
p.t_walmart    <- ts(data.walmart$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_intel      <- ts(data.intel$PRC,      start=c(2015,1,1), frequency=n.day)
p.t_bankofa    <- ts(data.bankofa$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_verizon    <- ts(data.verizon$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_att        <- ts(data.att$PRC,        start=c(2015,1,1), frequency=n.day)
p.t_homedep    <- ts(data.homedep$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_citi       <- ts(data.citi$PRC,       start=c(2015,1,1), frequency=n.day)
p.t_amazon     <- ts(data.amazon$PRC,     start=c(2015,1,1), frequency=n.day)
p.t_chinamob   <- ts(data.chinamob$PRC,   start=c(2015,1,1), frequency=n.day)
p.t_taiwan     <- ts(data.taiwan$PRC,     start=c(2015,1,1), frequency=n.day)
p.t_novartis   <- ts(data.novartis$PRC,   start=c(2015,1,1), frequency=n.day)
p.t_netflix    <- ts(data.netflix$PRC,    start=c(2015,1,1), frequency=n.day)
p.t_visa       <- ts(data.visa$PRC,       start=c(2015,1,1), frequency=n.day)
p.t_unhealth   <- ts(data.unhealth$PRC,   start=c(2015,1,1), frequency=n.day)
p.t_busch      <- ts(data.busch$PRC,      start=c(2015,1,1), frequency=n.day)



### Time plot of closing prices for all 30 companies over 1/01/2015-12/31/2019
### NOTE: when comparing two TS plots, notice that scale is dependent on mins/maxs of each TS 
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

summary(p.t_gm)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_gm,ylim=c(25,50),xlab="Year",main="General Motors Closing Prices 1/01/2015-12/31/2019")

summary(p.t_ibm)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_ibm,ylim=c(100,190),xlab="Year",main="IBM Closing Prices 1/01/2015-12/31/2019")

summary(p.t_facebook)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_facebook,ylim=c(75,220),xlab="Year",main="Facebook Closing Prices 1/01/2015-12/31/2019")

summary(p.t_chevron)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_chevron,ylim=c(65,135),xlab="Year",main="Chevron Closing Prices 1/01/2015-12/31/2019")

summary(p.t_apple)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_apple,ylim=c(85,300),xlab="Year",main="Apple Closing Prices 1/01/2015-12/31/2019")

summary(p.t_alibaba)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_alibaba,ylim=c(55,220),xlab="Year",main="Alibaba Closing Prices 1/01/2015-12/31/2019")

summary(p.t_pg)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_pg,ylim=c(65,130),xlab="Year",main="Procter & Gamble Closing Prices 1/01/2015-12/31/2019")

summary(p.t_pfizer)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_pfizer,ylim=c(25,50),xlab="Year",main="Pfizer Closing Prices 1/01/2015-12/31/2019")

summary(p.t_johnson)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_johnson,ylim=c(85,150),xlab="Year",main="Johnson & Johnson Closing Prices 1/01/2015-12/31/2019")

summary(p.t_disney)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_disney,ylim=c(85,155),xlab="Year",main="Disney Closing Prices 1/01/2015-12/31/2019")

summary(p.t_wellsfargo)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_wellsfargo,ylim=c(40,70),xlab="Year",main="Wells Fargo Closing Prices 1/01/2015-12/31/2019")

summary(p.t_jpmorgan)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_jpmorgan,ylim=c(50,140),xlab="Year",main="JP Morgan Closing Prices 1/01/2015-12/31/2019")

summary(p.t_walmart)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_walmart,ylim=c(55,125),xlab="Year",main="Walmart Closing Prices 1/01/2015-12/31/2019")

summary(p.t_intel)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_intel,ylim=c(20,65),xlab="Year",main="Intel Closing Prices 1/01/2015-12/31/2019")

summary(p.t_bankofa)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_bankofa,ylim=c(10,40),xlab="Year",main="Bank of America Closing Prices 1/01/2015-12/31/2019")

summary(p.t_verizon)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_verizon,ylim=c(40,65),xlab="Year",main="Verizon Closing Prices 1/01/2015-12/31/2019")

summary(p.t_att)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_att,ylim=c(25,45),xlab="Year",main="AT&T Closing Prices 1/01/2015-12/31/2019")

summary(p.t_homedep)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_homedep,ylim=c(100,240),xlab="Year",main="Home Depot Closing Prices 1/01/2015-12/31/2019")

summary(p.t_citi)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_citi,ylim=c(30,85),xlab="Year",main="CITI Closing Prices 1/01/2015-12/31/2019")

summary(p.t_amazon)                                                   # extremely large spread min: 286.9 and max: 2039.5
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_amazon,ylim=c(280,2040),xlab="Year",main="Amazon Closing Prices 1/01/2015-12/31/2019")

summary(p.t_chinamob)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_chinamob,ylim=c(35,80),xlab="Year",main="China Mobile Closing Prices 1/01/2015-12/31/2019")

summary(p.t_taiwan)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_taiwan,ylim=c(15,60),xlab="Year",main="Taiwan Semiconductor Closing Prices 1/01/2015-12/31/2019")

summary(p.t_novartis)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_novartis,ylim=c(65,110),xlab="Year",main="Novartis Mobile Closing Prices 1/01/2015-12/31/2019")

summary(p.t_netflix)                                               # large spread min: 82.79 and max: 707.61
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_netflix,ylim=c(80,710),xlab="Year",main="Netflix Closing Prices 1/01/2015-12/31/2019")

summary(p.t_visa)                                                  # sharp drop at beg. of 2015 due to stocks splitting
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_visa,ylim=c(60,280),xlab="Year",main="Visa Mobile Closing Prices 1/01/2015-12/31/2019")

summary(p.t_unhealth)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_unhealth,ylim=c(95,300),xlab="Year",main="United Health Closing Prices 1/01/2015-12/31/2019")

summary(p.t_busch)
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)
plot.ts(p.t_busch,ylim=c(60,135),xlab="Year",main="Busch Closing Prices 1/01/2015-12/31/2019")


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

v.t_netflix   <- garmanklassTA(open =data.netflix$OPENPRC,
                                high =data.netflix$ASKHI,
                                low  =data.netflix$BIDLO,
                                close=data.netflix$PRC)

v.t_unhealth  <- garmanklassTA(open =data.unhealth$OPENPRC,
                                 high =data.unhealth$ASKHI,
                                 low  =data.unhealth$BIDLO,
                                 close=data.unhealth$PRC)

v.t_visa      <- garmanklassTA(open =data.visa$OPENPRC,
                                 high =data.visa$ASKHI,
                                 low  =data.visa$BIDLO,
                                 close=data.visa$PRC)

v.t_taiwan    <- garmanklassTA(open =data.taiwan$OPENPRC,
                             high =data.taiwan$ASKHI,
                             low  =data.taiwan$BIDLO,
                             close=data.taiwan$PRC)

v.t_amazon    <- garmanklassTA(open =data.amazon$OPENPRC,
                               high =data.amazon$ASKHI,
                               low  =data.amazon$BIDLO,
                               close=data.amazon$PRC)

v.t_bankofa    <- garmanklassTA(open =data.bankofa$OPENPRC,
                               high =data.bankofa$ASKHI,
                               low  =data.bankofa$BIDLO,
                               close=data.bankofa$PRC)

v.t_intel    <- garmanklassTA(open =data.intel$OPENPRC,
                               high =data.intel$ASKHI,
                               low  =data.intel$BIDLO,
                               close=data.intel$PRC)

v.t_alibaba    <- garmanklassTA(open =data.alibaba$OPENPRC,
                               high =data.alibaba$ASKHI,
                               low  =data.alibaba$BIDLO,
                               close=data.alibaba$PRC)

v.t_exxon    <- garmanklassTA(open =data.exxon$OPENPRC,
                                high =data.exxon$ASKHI,
                                low  =data.exxon$BIDLO,
                                close=data.exxon$PRC)

                                                           


length(p.t_oracle)                               # 1258
length(v.t_oracle)                               # 1257 [CAUTION] We might need to keep an eye on this for changepoints

summary(v.t_oracle)
summary(v.t_microsoft)
summary(v.t_homedep)



### Volitility time plots (v.t)



### Sample ACF
dev.new()
par(mfrow=c(2,1),mex=0.75)
acf(v.t_oracle,lag.max=100,na.action=na.pass)
acf(w.t_oracle,lag.max=100,na.action=na.pass)



### PELT method (change in mean)
cpt.PELT_v.oracle <- cpt.mean(v.t_oracle,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.oracle)                                # no changepoints detected

cpt.PELT_v.microsoft <- cpt.mean(v.t_microsoft,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.microsoft)

t.PELT_v.microsoft <- cpts(cpt.PELT_v.microsoft)+1        # add 1 to keep changepoint time meaning same
t.PELT_v.microsoft                                        # 775

data.microsoft[t.PELT_v.microsoft,]                       # date 20180130

cpt.PELT_v.homedep <- cpt.mean(v.t_homedep,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.homedep)

t.PELT_v.homedep <- cpts(cpt.PELT_v.homedep)+1            # add 1 to keep changepoint time meaning same
t.PELT_v.homedep                                          # 161  162  774  783  946 1005

data.microsoft[t.PELT_v.homedep,]

cpt.PELT_v.netflix <- cpt.mean(v.t_netflix,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=260)
summary(cpt.PELT_v.netflix)                               # 44 changepoints detected, seems high
t.PELT_v.netflix <- cpts(cpt.PELT_v.netflix)+1 

cpt.PELT_v.unhealth <- cpt.mean(v.t_unhealth,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.unhealth)                              # 160 161 685 773 777 778 782 833 949 987 1003 1074 1077 1079 1226 1227  
t.PELT_v.unhealth <- cpts(cpt.PELT_v.unhealth)+1 

cpt.PELT_v.visa <- cpt.mean(v.t_visa,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.visa)                                 # 51 160 161 777 782 946 1009
t.PELT_v.visa <- cpts(cpt.PELT_v.visa)+1 

cpt.PELT_v.taiwan <- cpt.mean(v.t_taiwan,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.taiwan)                                # no changepoints detected

cpt.PELT_v.amazon <- cpt.mean(v.t_amazon,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.amazon)                               # 331 change points detected    
t.PELT_v.amazon <- cpts(cpt.PELT_v.amazon)+1 


cpt.PELT_v.bankofa <- cpt.mean(v.t_bankofa,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.bankofa)                               # no change points detected  


cpt.PELT_v.intel <- cpt.mean(v.t_intel,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.intel)                               # no change points detected  


cpt.PELT_v.alibaba <- cpt.mean(v.t_alibaba,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.alibaba)                               # 611 614 775 782 1026  
t.PELT_v.alibaba <- cpts(cpt.PELT_v.alibaba)+1


cpt.PELT_v.exxon <- cpt.mean(v.t_exxon,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.exxon)                               # no changepoints detected 


cpt.PELT_v.microsoft <- cpt.mean(v.t_microsoft,penalty="MBIC",method="PELT",test.stat="Normal",minseglen=1)
summary(cpt.PELT_v.microsoft)                          # CAUTION 774 changepoints detected




# Plotting volatility with change points
dev.new(width=12,height=6)                                # microsoft plot with changepoints
par(mfrow=c(1,1),mex=0.75)
plot.ts(v.t_microsoft,ylim=c(0,5),
        xlab="Year",ylab="GK volatility",main="Microsoft Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_microsoft)[t.PELT_v.microsoft],col="red",lty=2) 

dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)                               # visa plot with changepoints
plot.ts(v.t_visa,ylim=c(0,18),
        xlab="Year",ylab="GK volatility",main="Visa Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_visa)[t.PELT_v.visa],col="red",lty=2) 


dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)                               # netflix plot with changepoints
plot.ts(v.t_netflix,ylim=c(0,18),
        xlab="Year",ylab="GK volatility",main="Netflix Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_netflix)[t.PELT_v.netflix],col="red",lty=2)
 
dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)                               # united health plot with changepoints
plot.ts(v.t_unhealth,ylim=c(0,18),
        xlab="Year",ylab="GK volatility",main="United Health Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_unhealth)[t.PELT_v.unhealth],col="red",lty=2)


dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)                               # Amazon plot with changepoints
plot.ts(v.t_amazon,ylim=c(0,18),
        xlab="Year",ylab="GK volatility",main="Amazon Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_amazon)[t.PELT_v.amazon],col="red",lty=2)


dev.new(width=12,height=6)
par(mfrow=c(1,1),mex=0.75)                               # alibaba plot with changepoints
plot.ts(v.t_alibaba,ylim=c(0,18),
        xlab="Year",ylab="GK volatility",main="Alibaba Volatility 1/02/2015-12/31/2019")
abline(v=time(v.t_alibaba)[t.PELT_v.alibaba],col="red",lty=2)
