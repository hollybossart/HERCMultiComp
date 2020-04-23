#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objectives : This program splits the volatility data in two time periods and fits ARFIMA      ]*#
#*[              models to each of two parts.                                                     ]*#
#*[ Last update: 04/22/2020                                                                       ]*#
#*[ Authors    : Holly Bossart & Jaechoul Lee                                                     ]*#
#*[-----------------------------------------------------------------------------------------------]*#

### Setup data input and output directories (Lee)
WD.lib <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Codes/")
WD.inp <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Data/")
WD.out <- c("L:/Home/JaechoulLee/!1Research/Paper/ESS/P04_MC/S3_HERC/Work/")

### Setup data input and output directories (Bossart)
WD.lib <- c("C:/Users/12088/Dropbox/Research/HERCMultiComp/R/")
WD.inp <- c("C:/Users/12088/Dropbox/Research/HERCMultiComp/Data/")
WD.out <- c("C:/Users/12088/Dropbox/Research/HERCMultiComp/Output/")

### Load the proposed GA code and Poisson likelihood funtions
source(file=paste(WD.lib,"lib_volatilityfunctions.R",sep=""))

### Required packages
library(tidyverse)                                      # for filter() 
library(fracdiff)                                       # for fracdiff()

### Data import
data.OHLC <- read.csv(file=paste(WD.inp,"OHLCprices.csv",sep=""))
data.OHLC[1:10,]

comnames <- as.data.frame(unique(data.OHLC$COMNAM))
colnames(comnames) <- "Company"
comnames                                                # 30 companies

### Individual company data split into two parts
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
n.day <- n.obs/n.yrs                                    # no of days per year (251.6)

### Garman & Klass volatility series using garmanklassTA: v.t
v.t_oracle     <- garmanklassTA(open =data.oracle$OPENPRC,
                                high =data.oracle$ASKHI,
                                low  =data.oracle$BIDLO,
                                close=data.oracle$PRC)
v.t_microsoft  <- garmanklassTA(open =data.microsoft$OPENPRC,
                                high =data.microsoft$ASKHI,
                                low  =data.microsoft$BIDLO,
                                close=data.microsoft$PRC)
v.t_exxon      <- garmanklassTA(open =data.exxon$OPENPRC,
                                high =data.exxon$ASKHI,
                                low  =data.exxon$BIDLO,
                                close=data.exxon$PRC)
v.t_gm         <- garmanklassTA(open =data.gm$OPENPRC,
                                high =data.gm$ASKHI,
                                low  =data.gm$BIDLO,
                                close=data.gm$PRC)
v.t_ibm        <- garmanklassTA(open =data.ibm$OPENPRC,
                                high =data.ibm$ASKHI,
                                low  =data.ibm$BIDLO,
                                close=data.ibm$PRC)
v.t_facebook   <- garmanklassTA(open =data.facebook$OPENPRC,
                                high =data.facebook$ASKHI,
                                low  =data.facebook$BIDLO,
                                close=data.facebook$PRC)
v.t_chevron    <- garmanklassTA(open =data.chevron$OPENPRC,
                                high =data.chevron$ASKHI,
                                low  =data.chevron$BIDLO,
                                close=data.chevron$PRC)
v.t_apple      <- garmanklassTA(open =data.apple$OPENPRC,
                                high =data.apple$ASKHI,
                                low  =data.apple$BIDLO,
                                close=data.apple$PRC)
v.t_alibaba    <- garmanklassTA(open =data.alibaba$OPENPRC,
                                high =data.alibaba$ASKHI,
                                low  =data.alibaba$BIDLO,
                                close=data.alibaba$PRC)
v.t_pg         <- garmanklassTA(open =data.pg$OPENPRC,
                                high =data.pg$ASKHI,
                                low  =data.pg$BIDLO,
                                close=data.pg$PRC)
v.t_pfizer     <- garmanklassTA(open =data.pfizer$OPENPRC,
                                high =data.pfizer$ASKHI,
                                low  =data.pfizer$BIDLO,
                                close=data.pfizer$PRC)
v.t_johnson    <- garmanklassTA(open =data.johnson$OPENPRC,
                                high =data.johnson$ASKHI,
                                low  =data.johnson$BIDLO,
                                close=data.johnson$PRC)
v.t_disney     <- garmanklassTA(open =data.disney$OPENPRC,
                                high =data.disney$ASKHI,
                                low  =data.disney$BIDLO,
                                close=data.disney$PRC)
v.t_wellsfargo <- garmanklassTA(open =data.wellsfargo$OPENPRC,
                                high =data.wellsfargo$ASKHI,
                                low  =data.wellsfargo$BIDLO,
                                close=data.wellsfargo$PRC)
v.t_jpmorgan   <- garmanklassTA(open =data.jpmorgan$OPENPRC,
                                high =data.jpmorgan$ASKHI,
                                low  =data.jpmorgan$BIDLO,
                                close=data.jpmorgan$PRC)
v.t_walmart    <- garmanklassTA(open =data.walmart$OPENPRC,
                                high =data.walmart$ASKHI,
                                low  =data.walmart$BIDLO,
                                close=data.walmart$PRC)
v.t_intel      <- garmanklassTA(open =data.intel$OPENPRC,
                                high =data.intel$ASKHI,
                                low  =data.intel$BIDLO,
                                close=data.intel$PRC)
v.t_bankofa    <- garmanklassTA(open =data.bankofa$OPENPRC,
                                high =data.bankofa$ASKHI,
                                low  =data.bankofa$BIDLO,
                                close=data.bankofa$PRC)
v.t_verizon    <- garmanklassTA(open =data.verizon$OPENPRC,
                                high =data.verizon$ASKHI,
                                low  =data.verizon$BIDLO,
                                close=data.verizon$PRC)
v.t_att        <- garmanklassTA(open =data.att$OPENPRC,
                                high =data.att$ASKHI,
                                low  =data.att$BIDLO,
                                close=data.att$PRC)
v.t_homedep    <- garmanklassTA(open =data.homedep$OPENPRC,
                                high =data.homedep$ASKHI,
                                low  =data.homedep$BIDLO,
                                close=data.homedep$PRC)
v.t_citi       <- garmanklassTA(open =data.citi$OPENPRC,
                                high =data.citi$ASKHI,
                                low  =data.citi$BIDLO,
                                close=data.citi$PRC)
v.t_amazon     <- garmanklassTA(open =data.amazon$OPENPRC,
                                high =data.amazon$ASKHI,
                                low  =data.amazon$BIDLO,
                                close=data.amazon$PRC)
v.t_chinamob   <- garmanklassTA(open =data.chinamob$OPENPRC,
                                high =data.chinamob$ASKHI,
                                low  =data.chinamob$BIDLO,
                                close=data.chinamob$PRC)
v.t_taiwan     <- garmanklassTA(open =data.taiwan$OPENPRC,
                                high =data.taiwan$ASKHI,
                                low  =data.taiwan$BIDLO,
                                close=data.taiwan$PRC)
v.t_novartis   <- garmanklassTA(open =data.novartis$OPENPRC,
                                high =data.novartis$ASKHI,
                                low  =data.novartis$BIDLO,
                                close=data.novartis$PRC)
v.t_netflix    <- garmanklassTA(open =data.netflix$OPENPRC,
                                high =data.netflix$ASKHI,
                                low  =data.netflix$BIDLO,
                                close=data.netflix$PRC)
v.t_visa       <- garmanklassTA(open =data.visa$OPENPRC,
                                high =data.visa$ASKHI,
                                low  =data.visa$BIDLO,
                                close=data.visa$PRC)
v.t_unhealth   <- garmanklassTA(open =data.unhealth$OPENPRC,
                                high =data.unhealth$ASKHI,
                                low  =data.unhealth$BIDLO,
                                close=data.unhealth$PRC)
v.t_busch      <- garmanklassTA(open =data.busch$OPENPRC,
                                high =data.busch$ASKHI,
                                low  =data.busch$BIDLO,
                                close=data.busch$PRC)

### Two parts based on the changepoint results
#   part one: 2016/01/01 to 2017/12/31
#   part two: 2018/02/01 to 2019/12/31

### Calculating the start date for part 2 data sets to use window() function
start_1 <- 2016
end_1   <- 2018

start_2 <- end_1 + 21*deltat(v.t_oracle)        # there were 21 trading days in january 2018
                                                # doing this calculation gives the start time for feb 1 2018
end_2   <- tail(time(v.t_oracle),1)             # retrieves the end time index

c(start_1,end_1)                                # 2016 2018
c(start_2,end_2)                                # 2018.084 2019.996

### Splitting each volatility data set into two parts
v.t_oracle_1     <- window(v.t_oracle,                      
                           start=start_1,       # the beginning value will be on 2016/01/01
                           end=end_1)           # the end value will be on 2017/12/31
tsp(v.t_oracle_1)                               # verifies start = 2016.002
                                                # end = 2017.99
                                                # frequency = 251.4
v.t_oracle_2     <- window(v.t_oracle,                      
                           start=start_2,
                           end=end_2)
tsp(v.t_oracle_2)                               # verifies start = 2018.087
                                                # end = 2019.996 
                                                # frequency = 251.4

v.t_microsoft_1  <- window(v.t_microsoft, start=start_1,end=end_1)
v.t_microsoft_2  <- window(v.t_microsoft, start=start_2,end=end_2)

v.t_exxon_1      <- window(v.t_exxon,     start=start_1,end=end_1)
v.t_exxon_2      <- window(v.t_exxon,     start=start_2,end=end_2)

v.t_gm_1         <- window(v.t_gm,        start=start_1,end=end_1) 
v.t_gm_2         <- window(v.t_gm,        start=start_2,end=end_2)

v.t_ibm_1        <- window(v.t_ibm,       start=start_1,end=end_1)
v.t_ibm_2        <- window(v.t_ibm,       start=start_2,end=end_2)

v.t_facebook_1   <- window(v.t_facebook,  start=start_1,end=end_1)
v.t_facebook_2   <- window(v.t_facebook,  start=start_2,end=end_2)

v.t_chevron_1    <- window(v.t_chevron,   start=start_1,end=end_1)
v.t_chevron_2    <- window(v.t_chevron,   start=start_2,end=end_2)

v.t_apple_1      <- window(v.t_apple,     start=start_1,end=end_1)
v.t_apple_2      <- window(v.t_apple,     start=start_2,end=end_2)

v.t_alibaba_1    <- window(v.t_alibaba,   start=start_1,end=end_1)
v.t_alibaba_2    <- window(v.t_alibaba,   start=start_2,end=end_2)

v.t_pg_1         <- window(v.t_pg,        start=start_1,end=end_1)
v.t_pg_2         <- window(v.t_pg,        start=start_2,end=end_2)

v.t_pfizer_1     <- window(v.t_pfizer,    start=start_1,end=end_1)
v.t_pfizer_2     <- window(v.t_pfizer,    start=start_2,end=end_2)

v.t_johnson_1    <- window(v.t_johnson,   start=start_1,end=end_1)
v.t_johnson_2    <- window(v.t_johnson,   start=start_2,end=end_2)

v.t_disney_1     <- window(v.t_disney,    start=start_1,end=end_1)
v.t_disney_2     <- window(v.t_disney,    start=start_2,end=end_2)

v.t_wellsfargo_1 <- window(v.t_wellsfargo,start=start_1,end=end_1)
v.t_wellsfargo_2 <- window(v.t_wellsfargo,start=start_2,end=end_2)

v.t_jpmorgan_1   <- window(v.t_jpmorgan,  start=start_1,end=end_1)
v.t_jpmorgan_2   <- window(v.t_jpmorgan,  start=start_2,end=end_2)

v.t_walmart_1    <- window(v.t_walmart,   start=start_1,end=end_1)
v.t_walmart_2    <- window(v.t_walmart,   start=start_2,end=end_2)
                      
v.t_intel_1      <- window(v.t_intel,     start=start_1,end=end_1)
v.t_intel_2      <- window(v.t_intel,     start=start_2,end=end_2)
                     
v.t_bankofa_1    <- window(v.t_bankofa,   start=start_1,end=end_1)
v.t_bankofa_2    <- window(v.t_bankofa,   start=start_2,end=end_2)

v.t_verizon_1    <- window(v.t_verizon,   start=start_1,end=end_1)
v.t_verizon_2    <- window(v.t_verizon,   start=start_2,end=end_2)

v.t_att_1        <- window(v.t_att,       start=start_1,end=end_1)
v.t_att_2        <- window(v.t_att,       start=start_2,end=end_2)

v.t_homedep_1    <- window(v.t_homedep,   start=start_1,end=end_1)
v.t_homedep_2    <- window(v.t_homedep,   start=start_2,end=end_2)

v.t_citi_1       <- window(v.t_citi,      start=start_1,end=end_1)
v.t_citi_2       <- window(v.t_citi,      start=start_2,end=end_2)

v.t_amazon_1     <- window(v.t_amazon,    start=start_1,end=end_1)
v.t_amazon_2     <- window(v.t_amazon,    start=start_2,end=end_2)

v.t_chinamob_1   <- window(v.t_chinamob,  start=start_1,end=end_1)
v.t_chinamob_2   <- window(v.t_chinamob,  start=start_2,end=end_2)

v.t_taiwan_1     <- window(v.t_taiwan,    start=start_1,end=end_1)
v.t_taiwan_2     <- window(v.t_taiwan,    start=start_2,end=end_2)

v.t_novartis_1   <- window(v.t_novartis,  start=start_1,end=end_1)
v.t_novartis_2   <- window(v.t_novartis,  start=start_2,end=end_2)

v.t_netflix_1    <- window(v.t_netflix,   start=start_1,end=end_1)
v.t_netflix_2    <- window(v.t_netflix,   start=start_2,end=end_2)

v.t_visa_1       <- window(v.t_visa,      start=start_1,end=end_1)
v.t_visa_2       <- window(v.t_visa,      start=start_2,end=end_2)

v.t_unhealth_1   <- window(v.t_unhealth,  start=start_1,end=end_1)
v.t_unhealth_2   <- window(v.t_unhealth,  start=start_2,end=end_2)

v.t_busch_1      <- window(v.t_busch,     start=start_1,end=end_1)
v.t_busch_2      <- window(v.t_busch,     start=start_2,end=end_2)



### ARFIMA modelling functions
fracdiff.AICC <- function(fit) {               # This function computes AICC of a fitted ARFIMA model
  n.par <- length(c(fit$d,fit$ar,fit$ma))
  n <- fit$n
  aicc <- -2*fit$log.likelihood+2*(n.par+1)*n/(n-n.par-2)
  return(aicc)
}

fracdiff.AIC <- function(fit) {                # This function computes AIC of a fitted ARFIMA model
  return(AIC(fit))
}

fracdiff.BIC <- function(fit) {                # This function computes BIC of a fitted ARFIMA model
  return(BIC(fit))
}



### MODEL FITTING FOR PART 1 DATA--------------------------------------

bst.models <- data.frame("Company" = character(),  # this data frame will hold all of the model specifications for the best model
                         "Part" = numeric(),
                         "p" = numeric(), 
                         "d" = numeric(),
                         "q" = numeric(),
                         stringsAsFactors = FALSE)
colnames(bst.models) <- c("Company", "Part", "p", "d", "q")



### microsoft_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_microsoft_1,ylim=c(0,3),
        xlab="Year",ylab="GK volatility",main="Microsoft Volatility 1/01/2016-12/31/2017")
acf(v.t_microsoft_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(v.t_microsoft_1,lag.max=100,ylim=c(-0.2,1),main="")

fit.microsoft_1.0d0 <- fracdiff(v.t_microsoft_1-mean(v.t_microsoft_1),nar=0,nma=0,M=50)
summary(fit.microsoft_1.0d0)

fit.microsoft_1.1d0 <- fracdiff(v.t_microsoft_1-mean(v.t_microsoft_1),nar=1,nma=0,M=50)
summary(fit.microsoft_1.1d0)

fit.microsoft_1.2d0 <- fracdiff(v.t_microsoft_1-mean(v.t_microsoft_1),nar=2,nma=0,M=50)
summary(fit.microsoft_1.2d0)

fit.microsoft_1.0d1 <- fracdiff(v.t_microsoft_1-mean(v.t_microsoft_1),nar=0,nma=1,M=50)
summary(fit.microsoft_1.0d1)

fit.microsoft_1.1d1 <- fracdiff(v.t_microsoft_1-mean(v.t_microsoft_1),nar=1,nma=1,M=30)           # unable to compute
summary(fit.microsoft_1.1d1)

fit.microsoft_1.0d0_b <- fracdiff(v.t_microsoft_1-mean(v.t_microsoft_1),nar=0,nma=0,M=30)         # changed M value to 30, results are the same as above
summary(fit.microsoft_1.0d0)


c(fracdiff.AICC(fit.microsoft_1.0d0),fracdiff.AIC(fit.microsoft_1.0d0),fracdiff.BIC(fit.microsoft_1.0d0))  
c(fracdiff.AICC(fit.microsoft_1.1d0),fracdiff.AIC(fit.microsoft_1.1d0),fracdiff.BIC(fit.microsoft_1.1d0))
c(fracdiff.AICC(fit.microsoft_1.0d1),fracdiff.AIC(fit.microsoft_1.0d1),fracdiff.BIC(fit.microsoft_1.0d1))


### microsoft_1 model diagnostics: autocorrelation in residuals
fit.microsoft_1.bst <- fit.microsoft_1.0d0                                                       # this minimizes AICC, AIC, BIC

r.t_microsoft_1 <- fit.microsoft_1.bst$residuals
summary(r.t_microsoft_1)                                                                         # min -0.48 max 1.76 mean 0


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_microsoft_1,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="Microsoft Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_microsoft_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_microsoft_1,lag.max=100,ylim=c(-0.2,1),main="")

### microsoft_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_microsoft_1,                                                                           # histogram of residuals
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,2.5),
     main="Residual Histogram")                                                                 
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_microsoft_1),sd=sd(r.t_microsoft_1)),lty=1,col="red")             # add theoretical normal density
qqnorm(r.t_microsoft_1)                                                                         # normal Q-Q plot
qqline(r.t_microsoft_1)

shapiro.test(r.t_microsoft_1)                                                                   # Shapiro-Wilk normality test
ks.test(r.t_microsoft_1,"pnorm",mean=mean(r.t_microsoft_1),sd=sd(r.t_microsoft_1))

### add model to table
bst.models[nrow(bst.models)+1,] <- c("Microsoft", 1, 0, fit.microsoft_1.bst$d, 0)



### oracle_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_oracle_1,ylim=c(0,3),
        xlab="Year",ylab="GK volatility",main="Oracle Volatility 1/01/2016-12/31/2017")
acf(v.t_oracle_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(v.t_oracle_1,lag.max=100,ylim=c(-0.2,1),main="")

fit.oracle_1.0d0 <- fracdiff(v.t_oracle_1-mean(v.t_oracle_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.oracle_1.0d0)

fit.oracle_1.1d0 <- fracdiff(v.t_oracle_1-mean(v.t_oracle_1),nar=1,nma=0,M=50)                  # ar, d term significant
summary(fit.oracle_1.1d0)

fit.oracle_1.2d0 <- fracdiff(v.t_oracle_1-mean(v.t_oracle_1),nar=2,nma=0,M=50)                  # only d, ar1 term significant
summary(fit.oracle_1.2d0)

fit.oracle_1.0d1 <- fracdiff(v.t_oracle_1-mean(v.t_oracle_1),nar=0,nma=1,M=50)                  # ma term significant
summary(fit.oracle_1.0d1)

fit.oracle_1.0d2 <- fracdiff(v.t_oracle_1-mean(v.t_oracle_1),nar=0,nma=2,M=50)                  # ma1 term significant
summary(fit.oracle_1.0d2)

fit.oracle_1.1d1 <- fracdiff(v.t_oracle_1-mean(v.t_oracle_1),nar=1,nma=1,M=50)                  # only d term significant          
summary(fit.oracle_1.1d1)

fit.oracle_1.0d0_b <- fracdiff(v.t_oracle_1-mean(v.t_oracle_1),nar=0,nma=0,M=30)                # changed M value to 30, results are the same as above
summary(fit.oracle_1.0d0_b)


c(fracdiff.AICC(fit.oracle_1.0d0),fracdiff.AIC(fit.oracle_1.0d0),fracdiff.BIC(fit.oracle_1.0d0))  
c(fracdiff.AICC(fit.oracle_1.1d0),fracdiff.AIC(fit.oracle_1.1d0),fracdiff.BIC(fit.oracle_1.1d0))
c(fracdiff.AICC(fit.oracle_1.0d1),fracdiff.AIC(fit.oracle_1.0d1),fracdiff.BIC(fit.oracle_1.0d1))




### oracle_1 model diagnostics: autocorrelation in residuals
fit.oracle_1.bst <- fit.oracle_1.0d0                                                          # this minimizes BIC but AICC and AIC are very close to min val

r.t_oracle_1 <- fit.oracle_1.bst$residuals
summary(r.t_oracle_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_oracle_1,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="Oracle Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_oracle_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_oracle_1,lag.max=100,ylim=c(-0.2,1),main="")

### oracle_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_oracle_1,                                                                            # histogram of residuals
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_oracle_1),sd=sd(r.t_oracle_1)),lty=1,col="red")                # add theoretical normal density
qqnorm(r.t_oracle_1)                                                                         # [Q] Does this QQ plot support normality? Hard to tell at the ends
qqline(r.t_oracle_1)
shapiro.test(r.t_oracle_1)                                                                   # Shapiro-Wilk normality test supports normality
ks.test(r.t_oracle_1,"pnorm",mean=mean(r.t_oracle_1),sd=sd(r.t_oracle_1))                    # KS test supports normality


bst.models[nrow(bst.models)+1,] <- c("Oracle", 1, 0, fit.oracle_1.bst$d, 0)



### exxon_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_exxon_1,ylim=c(0,3),                                                             # might have a slight downward linear trend?
        xlab="Year",ylab="GK volatility",main="Exxon Volatility 1/01/2016-12/31/2017")
acf(v.t_exxon_1,lag.max=100,ylim=c(-0.2,1),main="")                                          # looks like slow decrease of acf as lag increases
pacf(v.t_exxon_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.exxon_1.0d0 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.exxon_1.0d0)

fit.exxon_1.1d0 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=1,nma=0,M=50)                  # ar, d term significant
summary(fit.exxon_1.1d0)

fit.exxon_1.2d0 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=2,nma=0,M=50)                  # all terms significant
summary(fit.exxon_1.2d0)

fit.exxon_1.0d1 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=0,nma=1,M=50)                  # ma term significant
summary(fit.exxon_1.0d1)

fit.exxon_1.0d2 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=0,nma=2,M=50)                  # ma1 and ma2 term significant
summary(fit.exxon_1.0d2)

fit.exxon_1.1d1 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=1,nma=1,M=50)                  # all terms significant         
summary(fit.exxon_1.1d1)

fit.exxon_1.1d2 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=1,nma=2,M=50)                  # warning when computing correlation        
summary(fit.exxon_1.1d2)

fit.exxon_1.2d1 <- fracdiff(v.t_exxon_1-mean(v.t_exxon_1),nar=2,nma=1,M=50)                  # ar2 term not significant            
summary(fit.exxon_1.2d1)



c(fracdiff.AICC(fit.exxon_1.0d0),fracdiff.AIC(fit.exxon_1.0d0),fracdiff.BIC(fit.exxon_1.0d0))  
c(fracdiff.AICC(fit.exxon_1.1d0),fracdiff.AIC(fit.exxon_1.1d0),fracdiff.BIC(fit.exxon_1.1d0))
c(fracdiff.AICC(fit.exxon_1.0d1),fracdiff.AIC(fit.exxon_1.0d1),fracdiff.BIC(fit.exxon_1.0d1))
c(fracdiff.AICC(fit.exxon_1.1d1),fracdiff.AIC(fit.exxon_1.1d1),fracdiff.BIC(fit.exxon_1.1d1))
c(fracdiff.AICC(fit.exxon_1.2d0),fracdiff.AIC(fit.exxon_1.2d0),fracdiff.BIC(fit.exxon_1.2d0))
c(fracdiff.AICC(fit.exxon_1.0d2),fracdiff.AIC(fit.exxon_1.0d2),fracdiff.BIC(fit.exxon_1.0d2))


### exxon_1 model diagnostics: autocorrelation in residuals
fit.exxon_1.bst <- fit.exxon_1.0d0                                                          # this minimizes BIC but AICC and AIC are close to min val

r.t_exxon_1 <- fit.exxon_1.bst$residuals
summary(r.t_exxon_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_exxon_1,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="Exxon Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_exxon_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_exxon_1,lag.max=100,ylim=c(-0.2,1),main="")

### exxon_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_exxon_1,                                                                            # histogram of residuals
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_exxon_1),sd=sd(r.t_exxon_1)),lty=1,col="red")                 # add theoretical normal density
qqnorm(r.t_exxon_1)                                                                         
qqline(r.t_exxon_1)

shapiro.test(r.t_exxon_1)                                                                   # Shapiro-Wilk normality test supports normality
ks.test(r.t_exxon_1,"pnorm",mean=mean(r.t_exxon_1),sd=sd(r.t_exxon_1))                      # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Exxon", 1, 0, fit.exxon_1.bst$d, 0)                   # adding to the table



### gm_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_gm_1,ylim=c(0,2),                                                             
        xlab="Year",ylab="GK volatility",main="GM Volatility 1/01/2016-12/31/2017")
acf(v.t_gm_1,lag.max=100,ylim=c(-0.2,1),main="")                                         
pacf(v.t_gm_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.gm_1.0d0 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.gm_1.0d0)

fit.gm_1.1d0 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=1,nma=0,M=50)                  # ar, d term significant
summary(fit.gm_1.1d0)

fit.gm_1.2d0 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=2,nma=0,M=50)                  # ar2 not sig
summary(fit.gm_1.2d0)

fit.gm_1.0d1 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=0,nma=1,M=50)                  # d, ma term significant
summary(fit.gm_1.0d1)

fit.gm_1.0d2 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=0,nma=2,M=50)                  # ma2 not sig
summary(fit.gm_1.0d2)

fit.gm_1.1d1 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=1,nma=1,M=50)                  # warning when computing corr      
summary(fit.gm_1.1d1)

fit.gm_1.1d2 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=1,nma=2,M=50)                  # warning when computing correlation        
summary(fit.gm_1.1d2)

fit.gm_1.2d1 <- fracdiff(v.t_gm_1-mean(v.t_gm_1),nar=2,nma=1,M=50)                  # ar2 term not significant            
summary(fit.gm_1.2d1)



c(fracdiff.AICC(fit.gm_1.0d0),fracdiff.AIC(fit.gm_1.0d0),fracdiff.BIC(fit.gm_1.0d0))  
c(fracdiff.AICC(fit.gm_1.1d0),fracdiff.AIC(fit.gm_1.1d0),fracdiff.BIC(fit.gm_1.1d0))
c(fracdiff.AICC(fit.gm_1.0d1),fracdiff.AIC(fit.gm_1.0d1),fracdiff.BIC(fit.gm_1.0d1))




### gm_1 model diagnostics: autocorrelation in residuals
fit.gm_1.bst <- fit.gm_1.0d0                                                          # this minimizes BIC but AICC and AIC are close to min val

r.t_gm_1 <- fit.gm_1.bst$residuals
summary(r.t_gm_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_gm_1,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="General Motors Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_gm_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_gm_1,lag.max=100,ylim=c(-0.2,1),main="")

### gm_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_gm_1,                                                                           # histogram of residuals
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_gm_1),sd=sd(r.t_gm_1)),lty=1,col="red")                    # add theoretical normal density
qqnorm(r.t_gm_1)                                                                         
qqline(r.t_gm_1)

shapiro.test(r.t_gm_1)                                                                   # Shapiro-Wilk normality test supports normality
ks.test(r.t_gm_1,"pnorm",mean=mean(r.t_gm_1),sd=sd(r.t_gm_1))                            # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("General Motors", 1, 0, fit.gm_1.bst$d, 0)          # adding to the table



### ibm_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_ibm_1,ylim=c(0,4),                                                             
        xlab="Year",ylab="GK volatility",main="IBM Volatility 1/01/2016-12/31/2017")
acf(v.t_ibm_1,lag.max=100,ylim=c(-0.2,1),main="")                                         
pacf(v.t_ibm_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.ibm_1.0d0 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.ibm_1.0d0)

fit.ibm_1.1d0 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=1,nma=0,M=50)                  # ar significant
summary(fit.ibm_1.1d0)

fit.ibm_1.2d0 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=2,nma=0,M=50)                  # ar1/2 significant
summary(fit.ibm_1.2d0)

fit.ibm_1.0d1 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=0,nma=1,M=50)                  # ma not sig
summary(fit.ibm_1.0d1)

fit.ibm_1.0d2 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=0,nma=2,M=50)                  # all significant
summary(fit.ibm_1.0d2)

fit.ibm_1.1d1 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=1,nma=1,M=50)                  # warning when computing corr      
summary(fit.ibm_1.1d1)

fit.ibm_1.1d2 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=1,nma=2,M=50)                  # warning when computing correlation        
summary(fit.ibm_1.1d2)

fit.ibm_1.2d1 <- fracdiff(v.t_ibm_1-mean(v.t_ibm_1),nar=2,nma=1,M=50)                  # warning when computing corr         
summary(fit.ibm_1.2d1)



c(fracdiff.AICC(fit.ibm_1.0d0),fracdiff.AIC(fit.ibm_1.0d0),fracdiff.BIC(fit.ibm_1.0d0))  
c(fracdiff.AICC(fit.ibm_1.1d0),fracdiff.AIC(fit.ibm_1.1d0),fracdiff.BIC(fit.ibm_1.1d0))
c(fracdiff.AICC(fit.ibm_1.2d0),fracdiff.AIC(fit.ibm_1.2d0),fracdiff.BIC(fit.ibm_1.2d0))
c(fracdiff.AICC(fit.ibm_1.0d2),fracdiff.AIC(fit.ibm_1.0d2),fracdiff.BIC(fit.ibm_1.0d2))




### ibm_1 model diagnostics: autocorrelation in residuals
fit.ibm_1.bst <- fit.ibm_1.0d0                                                          # this minimizes BIC but AICC and AIC are close to min val

r.t_ibm_1 <- fit.ibm_1.bst$residuals
summary(r.t_ibm_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_ibm_1,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="IBM Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_ibm_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_ibm_1,lag.max=100,ylim=c(-0.2,1),main="")

### ibm_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_ibm_1,                                                                           # one outlier
     breaks=seq(-2,5,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_ibm_1),sd=sd(r.t_ibm_1)),lty=1,col="red")                   # add theoretical normal density
qqnorm(r.t_ibm_1)                                                                         
qqline(r.t_ibm_1)

shapiro.test(r.t_ibm_1)                                                                   # Shapiro-Wilk normality test supports normality
ks.test(r.t_ibm_1,"pnorm",mean=mean(r.t_ibm_1),sd=sd(r.t_ibm_1))                          # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("IBM", 1, 0, fit.ibm_1.bst$d, 0)                     # adding to the table




### facebook_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_facebook_1,ylim=c(0,5),                                                             
        xlab="Year",ylab="GK volatility",main="Facebook Volatility 1/01/2016-12/31/2017")
acf(v.t_facebook_1,lag.max=100,ylim=c(-0.2,1),main="")                                         
pacf(v.t_facebook_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.facebook_1.0d0 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.facebook_1.0d0)

fit.facebook_1.1d0 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=1,nma=0,M=50)                  # ar significant
summary(fit.facebook_1.1d0)

fit.facebook_1.2d0 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=2,nma=0,M=50)                  # ar2 not sig
summary(fit.facebook_1.2d0)

fit.facebook_1.0d1 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=0,nma=1,M=50)                  # ma sig
summary(fit.facebook_1.0d1)

fit.facebook_1.0d2 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=0,nma=2,M=50)                  # ma1 sig
summary(fit.facebook_1.0d2)

fit.facebook_1.1d1 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=1,nma=1,M=50)                  # all sig     
summary(fit.facebook_1.1d1)

fit.facebook_1.1d2 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=1,nma=2,M=50)                  # warning when computing correlation        
summary(fit.facebook_1.1d2)

fit.facebook_1.2d1 <- fracdiff(v.t_facebook_1-mean(v.t_facebook_1),nar=2,nma=1,M=50)                  # warning when computing corr         
summary(fit.facebook_1.2d1)



c(fracdiff.AICC(fit.facebook_1.0d0),fracdiff.AIC(fit.facebook_1.0d0),fracdiff.BIC(fit.facebook_1.0d0))  
c(fracdiff.AICC(fit.facebook_1.1d0),fracdiff.AIC(fit.facebook_1.1d0),fracdiff.BIC(fit.facebook_1.1d0))
c(fracdiff.AICC(fit.facebook_1.2d0),fracdiff.AIC(fit.facebook_1.2d0),fracdiff.BIC(fit.facebook_1.2d0))
c(fracdiff.AICC(fit.facebook_1.0d2),fracdiff.AIC(fit.facebook_1.0d2),fracdiff.BIC(fit.facebook_1.0d2))
c(fracdiff.AICC(fit.facebook_1.1d1),fracdiff.AIC(fit.facebook_1.1d1),fracdiff.BIC(fit.facebook_1.1d1))
c(fracdiff.AICC(fit.facebook_1.0d1),fracdiff.AIC(fit.facebook_1.0d1),fracdiff.BIC(fit.facebook_1.0d1))


### facebook_1 model diagnostics: autocorrelation in residuals
fit.facebook_1.bst <- fit.facebook_1.0d0                                                         

r.t_facebook_1 <- fit.facebook_1.bst$residuals
summary(r.t_facebook_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_facebook_1,ylim=c(-2,5),
        xlab="Year",ylab="GK volatility",main="Facebook Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_facebook_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_facebook_1,lag.max=100,ylim=c(-0.2,1),main="")

### facebook_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_facebook_1,                                                                                # [Q] some outliers?
     breaks=seq(-2,5,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_facebook_1),sd=sd(r.t_facebook_1)),lty=1,col="red")               
qqnorm(r.t_facebook_1)                                                                         
qqline(r.t_facebook_1)

shapiro.test(r.t_facebook_1)                                                                        # Shapiro-Wilk normality test supports normality
ks.test(r.t_facebook_1,"pnorm",mean=mean(r.t_facebook_1),sd=sd(r.t_facebook_1))                     # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Facebook", 1, 0, fit.facebook_1.bst$d, 0)                     # adding to the table



### chevron_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_chevron_1,ylim=c(0,5),                                                             
        xlab="Year",ylab="GK volatility",main="Chevron Volatility 1/01/2016-12/31/2017")
acf(v.t_chevron_1,lag.max=100,ylim=c(-0.2,1),main="")                                              # definitely appears to be long-memory
pacf(v.t_chevron_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.chevron_1.0d0 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.chevron_1.0d0)

fit.chevron_1.1d0 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=1,nma=0,M=50)                  # ar significant
summary(fit.chevron_1.1d0)

fit.chevron_1.2d0 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=2,nma=0,M=50)                  # all terms significant
summary(fit.chevron_1.2d0)

fit.chevron_1.0d1 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=0,nma=1,M=50)                  # ma sig
summary(fit.chevron_1.0d1)

fit.chevron_1.0d2 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=0,nma=2,M=50)                  # warning when computing correlation
summary(fit.chevron_1.0d2)

fit.chevron_1.1d1 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=1,nma=1,M=50)                  # warning   
summary(fit.chevron_1.1d1)

fit.chevron_1.1d2 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=1,nma=2,M=50)                  # warning when computing correlation        
summary(fit.chevron_1.1d2)

fit.chevron_1.2d1 <- fracdiff(v.t_chevron_1-mean(v.t_chevron_1),nar=2,nma=1,M=50)                  # warning when computing corr         
summary(fit.chevron_1.2d1)



c(fracdiff.AICC(fit.chevron_1.0d0),fracdiff.AIC(fit.chevron_1.0d0),fracdiff.BIC(fit.chevron_1.0d0))  
c(fracdiff.AICC(fit.chevron_1.1d0),fracdiff.AIC(fit.chevron_1.1d0),fracdiff.BIC(fit.chevron_1.1d0))
c(fracdiff.AICC(fit.chevron_1.2d0),fracdiff.AIC(fit.chevron_1.2d0),fracdiff.BIC(fit.chevron_1.2d0))
c(fracdiff.AICC(fit.chevron_1.0d1),fracdiff.AIC(fit.chevron_1.0d1),fracdiff.BIC(fit.chevron_1.0d1))


### chevron_1 model diagnostics: autocorrelation in residuals
fit.chevron_1.bst <- fit.chevron_1.0d0                                                         

r.t_chevron_1 <- fit.chevron_1.bst$residuals
summary(r.t_chevron_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_chevron_1,ylim=c(-2,5),
        xlab="Year",ylab="GK volatility",main="Chevron Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_chevron_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_chevron_1,lag.max=100,ylim=c(-0.2,1),main="")

### chevron_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_chevron_1,                                                                                # [Q] some outliers?
     breaks=seq(-2,5,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_chevron_1),sd=sd(r.t_chevron_1)),lty=1,col="red")               
qqnorm(r.t_chevron_1)                                                                         
qqline(r.t_chevron_1)

shapiro.test(r.t_chevron_1)                                                                        # Shapiro-Wilk normality test supports normality
ks.test(r.t_chevron_1,"pnorm",mean=mean(r.t_chevron_1),sd=sd(r.t_chevron_1))                     # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Chevron", 1, 0, fit.chevron_1.bst$d, 0)                     # adding to the table




### apple_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_apple_1,ylim=c(0,6),                                                             
        xlab="Year",ylab="GK volatility",main="Apple Volatility 1/01/2016-12/31/2017")
acf(v.t_apple_1,lag.max=100,ylim=c(-0.2,1),main="")                                          # definitely appears to be long-memory
pacf(v.t_apple_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.apple_1.0d0 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.apple_1.0d0)

fit.apple_1.1d0 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=1,nma=0,M=50)                  # ar significant
summary(fit.apple_1.1d0)

fit.apple_1.2d0 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=2,nma=0,M=50)                  # all terms significant
summary(fit.apple_1.2d0)

fit.apple_1.0d1 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=0,nma=1,M=50)                  # ma sig
summary(fit.apple_1.0d1)

fit.apple_1.0d2 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=0,nma=2,M=50)                  # ma2 not significant
summary(fit.apple_1.0d2)

fit.apple_1.1d1 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=1,nma=1,M=50)                  # all terms sig  
summary(fit.apple_1.1d1)

fit.apple_1.1d2 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=1,nma=2,M=50)                  # ma 2 not sig     
summary(fit.apple_1.1d2)

fit.apple_1.2d1 <- fracdiff(v.t_apple_1-mean(v.t_apple_1),nar=2,nma=1,M=50)                  # warning when computing corr         
summary(fit.apple_1.2d1)



c(fracdiff.AICC(fit.apple_1.0d0),fracdiff.AIC(fit.apple_1.0d0),fracdiff.BIC(fit.apple_1.0d0))  
c(fracdiff.AICC(fit.apple_1.1d0),fracdiff.AIC(fit.apple_1.1d0),fracdiff.BIC(fit.apple_1.1d0))
c(fracdiff.AICC(fit.apple_1.2d0),fracdiff.AIC(fit.apple_1.2d0),fracdiff.BIC(fit.apple_1.2d0))
c(fracdiff.AICC(fit.apple_1.0d1),fracdiff.AIC(fit.apple_1.0d1),fracdiff.BIC(fit.apple_1.0d1))
c(fracdiff.AICC(fit.apple_1.1d1),fracdiff.AIC(fit.apple_1.1d1),fracdiff.BIC(fit.apple_1.1d1))

### apple_1 model diagnostics: autocorrelation in residuals
fit.apple_1.bst <- fit.apple_1.0d0                                                         

r.t_apple_1 <- fit.apple_1.bst$residuals
summary(r.t_apple_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_apple_1,ylim=c(-2,5),
        xlab="Year",ylab="GK volatility",main="Apple Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_apple_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_apple_1,lag.max=100,ylim=c(-0.2,1),main="")

### apple_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_apple_1,                                                                                # [Q] some outliers?
     breaks=seq(-2,5,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_apple_1),sd=sd(r.t_apple_1)),lty=1,col="red")               
qqnorm(r.t_apple_1)                                                                         
qqline(r.t_apple_1)

shapiro.test(r.t_apple_1)                                                                        # Shapiro-Wilk normality test supports normality
ks.test(r.t_apple_1,"pnorm",mean=mean(r.t_apple_1),sd=sd(r.t_apple_1))                           # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Apple", 1, 0, fit.apple_1.bst$d, 0)                        # adding to the table



### alibaba_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_alibaba_1,ylim=c(0,7),                                                             
        xlab="Year",ylab="GK volatility",main="Alibaba Volatility 1/01/2016-12/31/2017")
acf(v.t_alibaba_1,lag.max=100,ylim=c(-0.2,1),main="")                                              # definitely appears to be long-memory
pacf(v.t_alibaba_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.alibaba_1.0d0 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=0,nma=0,M=50)                  # d term significant  
summary(fit.alibaba_1.0d0)

fit.alibaba_1.1d0 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=1,nma=0,M=50)                  # ar term not significant
summary(fit.alibaba_1.1d0)

fit.alibaba_1.2d0 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=2,nma=0,M=50)                  # ar terms not sig
summary(fit.alibaba_1.2d0)

fit.alibaba_1.0d1 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=0,nma=1,M=50)                  # ma not sig
summary(fit.alibaba_1.0d1)

fit.alibaba_1.0d2 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=0,nma=2,M=50)                  # ma terms not sig
summary(fit.alibaba_1.0d2)

fit.alibaba_1.1d1 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=1,nma=1,M=50)                  # can't compute correlation 
summary(fit.alibaba_1.1d1)

fit.alibaba_1.1d2 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=1,nma=2,M=50)                  # can't compute correlation
summary(fit.alibaba_1.1d2)

fit.alibaba_1.2d1 <- fracdiff(v.t_alibaba_1-mean(v.t_alibaba_1),nar=2,nma=1,M=50)                  # warning when computing corr         
summary(fit.alibaba_1.2d1)



c(fracdiff.AICC(fit.alibaba_1.0d0),fracdiff.AIC(fit.alibaba_1.0d0),fracdiff.BIC(fit.alibaba_1.0d0))    # only model with all terms significant  


### alibaba_1 model diagnostics: autocorrelation in residuals
fit.alibaba_1.bst <- fit.alibaba_1.0d0                                                         

r.t_alibaba_1 <- fit.alibaba_1.bst$residuals
summary(r.t_alibaba_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_alibaba_1,ylim=c(-2,5),
        xlab="Year",ylab="GK volatility",main="Alibaba Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_alibaba_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_alibaba_1,lag.max=100,ylim=c(-0.2,1),main="")

### alibaba_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_alibaba_1,                                                                                    # [Q] some outliers? right skewed
     breaks=seq(-2,6.5,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_alibaba_1),sd=sd(r.t_alibaba_1)),lty=1,col="red")               
qqnorm(r.t_alibaba_1)                                                                         
qqline(r.t_alibaba_1)

shapiro.test(r.t_alibaba_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_alibaba_1,"pnorm",mean=mean(r.t_alibaba_1),sd=sd(r.t_alibaba_1))                           # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Alibaba", 1, 0, fit.alibaba_1.bst$d, 0)                          # adding to the table



### pg_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_pg_1,ylim=c(0,2),                                                             
        xlab="Year",ylab="GK volatility",main="PG Volatility 1/01/2016-12/31/2017")
acf(v.t_pg_1,lag.max=100,ylim=c(-0.2,1),main="")                                                       # definitely appears to be long-memory
pacf(v.t_pg_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.pg_1.0d0 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=0,nma=0,M=50)                                     # d term significant  
summary(fit.pg_1.0d0)

fit.pg_1.1d0 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=1,nma=0,M=50)                                     # ar term not significant
summary(fit.pg_1.1d0)

fit.pg_1.2d0 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=2,nma=0,M=50)                                     # ar terms sig
summary(fit.pg_1.2d0)

fit.pg_1.0d1 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=0,nma=1,M=50)                                     # ma not sig
summary(fit.pg_1.0d1)

fit.pg_1.0d2 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=0,nma=2,M=50)                                     # ma terms sig
summary(fit.pg_1.0d2)

fit.pg_1.1d1 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=1,nma=1,M=50)                                     # can't compute correlation 
summary(fit.pg_1.1d1)

fit.pg_1.1d2 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=1,nma=2,M=50)                                     # ma not sig
summary(fit.pg_1.1d2)

fit.pg_1.2d1 <- fracdiff(v.t_pg_1-mean(v.t_pg_1),nar=2,nma=1,M=50)                                     # all terms significant       
summary(fit.pg_1.2d1)



c(fracdiff.AICC(fit.pg_1.0d0),fracdiff.AIC(fit.pg_1.0d0),fracdiff.BIC(fit.pg_1.0d0))                  
c(fracdiff.AICC(fit.pg_1.2d1),fracdiff.AIC(fit.pg_1.2d1),fracdiff.BIC(fit.pg_1.2d1))                  


### pg_1 model diagnostics: autocorrelation in residuals
fit.pg_1.bst <- fit.pg_1.0d0                                                         

r.t_pg_1 <- fit.pg_1.bst$residuals
summary(r.t_pg_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_pg_1,ylim=c(-2,5),
        xlab="Year",ylab="GK volatility",main="PG Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_pg_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_pg_1,lag.max=100,ylim=c(-0.2,1),main="")

### pg_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_pg_1,                                                                                   
     breaks=seq(-2,3,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_pg_1),sd=sd(r.t_pg_1)),lty=1,col="red")               
qqnorm(r.t_pg_1)                                                                         
qqline(r.t_pg_1)

shapiro.test(r.t_pg_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_pg_1,"pnorm",mean=mean(r.t_pg_1),sd=sd(r.t_pg_1))                                     # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("PG", 1, 0, fit.pg_1.bst$d, 0)                               # adding to the table


### pfizer_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_pfizer_1,ylim=c(0,1.5),                                                             
        xlab="Year",ylab="GK volatility",main="Pfizer Volatility 1/01/2016-12/31/2017")
acf(v.t_pfizer_1,lag.max=100,ylim=c(-0.2,1),main="")                                                # definitely appears to be long-memory
pacf(v.t_pfizer_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.pfizer_1.0d0 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=0,nma=0,M=50)                      # d term significant  
summary(fit.pfizer_1.0d0)

fit.pfizer_1.1d0 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=1,nma=0,M=50)                      # ar term not significant
summary(fit.pfizer_1.1d0)

fit.pfizer_1.2d0 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=2,nma=0,M=50)                      # ar terms not sig
summary(fit.pfizer_1.2d0)

fit.pfizer_1.0d1 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=0,nma=1,M=50)                      # ma not sig
summary(fit.pfizer_1.0d1)

fit.pfizer_1.0d2 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=0,nma=2,M=50)                      # ma terms not sig
summary(fit.pfizer_1.0d2)

fit.pfizer_1.1d1 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=1,nma=1,M=50)                      # ma almost significant
summary(fit.pfizer_1.1d1)

fit.pfizer_1.1d2 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=1,nma=2,M=50)                      # ma2 not sig
summary(fit.pfizer_1.1d2)

fit.pfizer_1.2d1 <- fracdiff(v.t_pfizer_1-mean(v.t_pfizer_1),nar=2,nma=1,M=50)                      # cannot compute correlation      
summary(fit.pfizer_1.2d1)



c(fracdiff.AICC(fit.pfizer_1.0d0),fracdiff.AIC(fit.pfizer_1.0d0),fracdiff.BIC(fit.pfizer_1.0d0))                  
c(fracdiff.AICC(fit.pfizer_1.1d2),fracdiff.AIC(fit.pfizer_1.1d2),fracdiff.BIC(fit.pfizer_1.1d2))                  


### pfizer_1 model diagnostics: autocorrelation in residuals
fit.pfizer_1.bst <- fit.pfizer_1.0d0                                                         

r.t_pfizer_1 <- fit.pfizer_1.bst$residuals
summary(r.t_pfizer_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_pfizer_1,ylim=c(-1,1),
        xlab="Year",ylab="GK volatility",main="Pfizer Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_pfizer_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_pfizer_1,lag.max=100,ylim=c(-0.2,1),main="")

### pfizer_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_pfizer_1,                                                                                   
     breaks=seq(-1,1,0.125),
     freq=FALSE,
     col="grey85",ylim=c(0,5),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_pfizer_1),sd=sd(r.t_pfizer_1)),lty=1,col="red")               
qqnorm(r.t_pfizer_1)                                                                         
qqline(r.t_pfizer_1)

shapiro.test(r.t_pfizer_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_pfizer_1,"pnorm",mean=mean(r.t_pfizer_1),sd=sd(r.t_pfizer_1))                             # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Pfizer", 1, 0, fit.pfizer_1.bst$d, 0)                           # adding to the table


### johnson_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_johnson_1,ylim=c(0,4),                                                             
        xlab="Year",ylab="GK volatility",main="Johnson Volatility 1/01/2016-12/31/2017")
acf(v.t_johnson_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  # definitely appears to be long-memory
pacf(v.t_johnson_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.johnson_1.0d0 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=0,nma=0,M=50)                      # d term significant  
summary(fit.johnson_1.0d0)

fit.johnson_1.1d0 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=1,nma=0,M=50)                      # ar term not significant
summary(fit.johnson_1.1d0)

fit.johnson_1.2d0 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=2,nma=0,M=50)                      # ar terms sig
summary(fit.johnson_1.2d0)

fit.johnson_1.0d1 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=0,nma=1,M=50)                      # ma not sig
summary(fit.johnson_1.0d1)

fit.johnson_1.0d2 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=0,nma=2,M=50)                      # ma terms sig
summary(fit.johnson_1.0d2)

fit.johnson_1.1d1 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=1,nma=1,M=50)                      # only d sig
summary(fit.johnson_1.1d1)

fit.johnson_1.1d2 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=1,nma=2,M=50)                      # ma2 only sig
summary(fit.johnson_1.1d2)

fit.johnson_1.2d1 <- fracdiff(v.t_johnson_1-mean(v.t_johnson_1),nar=2,nma=1,M=50)                           
summary(fit.johnson_1.2d1)



c(fracdiff.AICC(fit.johnson_1.0d0),fracdiff.AIC(fit.johnson_1.0d0),fracdiff.BIC(fit.johnson_1.0d0))
c(fracdiff.AICC(fit.johnson_1.2d0),fracdiff.AIC(fit.johnson_1.2d0),fracdiff.BIC(fit.johnson_1.2d0)) 
c(fracdiff.AICC(fit.johnson_1.0d2),fracdiff.AIC(fit.johnson_1.0d2),fracdiff.BIC(fit.johnson_1.0d2))
                  


### johnson_1 model diagnostics: autocorrelation in residuals
fit.johnson_1.bst <- fit.johnson_1.0d0                                                         

r.t_johnson_1 <- fit.johnson_1.bst$residuals
summary(r.t_johnson_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_johnson_1,ylim=c(-1,3),
        xlab="Year",ylab="GK volatility",main="Johnson Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_johnson_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_johnson_1,lag.max=100,ylim=c(-0.2,1),main="")

### johnson_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_johnson_1,                                                                                   
     breaks=seq(-3,3,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_johnson_1),sd=sd(r.t_johnson_1)),lty=1,col="red")               
qqnorm(r.t_johnson_1)                                                                         
qqline(r.t_johnson_1)

shapiro.test(r.t_johnson_1)                                                                             # Shapiro-Wilk normality test supports normality
ks.test(r.t_johnson_1,"pnorm",mean=mean(r.t_johnson_1),sd=sd(r.t_johnson_1))                            # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Johnson", 1, 0, fit.johnson_1.bst$d, 0)                           # adding to the table



### disney_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_disney_1,ylim=c(0,4),                                                             
        xlab="Year",ylab="GK volatility",main="Disney Volatility 1/01/2016-12/31/2017")
acf(v.t_disney_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  # definitely appears to be long-memory
pacf(v.t_disney_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.disney_1.0d0 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=0,nma=0,M=50)                       # d term significant  
summary(fit.disney_1.0d0)

fit.disney_1.1d0 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=1,nma=0,M=50)                       # ar term not significant
summary(fit.disney_1.1d0)

fit.disney_1.2d0 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=2,nma=0,M=50)                       # aonly d sig
summary(fit.disney_1.2d0)

fit.disney_1.0d1 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=0,nma=1,M=50)                       # ma not sig
summary(fit.disney_1.0d1)

fit.disney_1.0d2 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=0,nma=2,M=50)                       # only d sig
summary(fit.disney_1.0d2)

fit.disney_1.1d1 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=1,nma=1,M=50)                       # warning
summary(fit.disney_1.1d1)

fit.disney_1.1d2 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=1,nma=2,M=50)                       # warning
summary(fit.disney_1.1d2)

fit.disney_1.2d1 <- fracdiff(v.t_disney_1-mean(v.t_disney_1),nar=2,nma=1,M=50)                           
summary(fit.disney_1.2d1)



c(fracdiff.AICC(fit.disney_1.0d0),fracdiff.AIC(fit.disney_1.0d0),fracdiff.BIC(fit.disney_1.0d0))
c(fracdiff.AICC(fit.disney_1.2d0),fracdiff.AIC(fit.disney_1.2d0),fracdiff.BIC(fit.disney_1.2d0)) 
c(fracdiff.AICC(fit.disney_1.0d2),fracdiff.AIC(fit.disney_1.0d2),fracdiff.BIC(fit.disney_1.0d2))



### disney_1 model diagnostics: autocorrelation in residuals
fit.disney_1.bst <- fit.disney_1.0d0                                                         

r.t_disney_1 <- fit.disney_1.bst$residuals
summary(r.t_disney_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_disney_1,ylim=c(-1,3),
        xlab="Year",ylab="GK volatility",main="Disney Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_disney_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_disney_1,lag.max=100,ylim=c(-0.2,1),main="")

### disney_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_disney_1,                                                                                   
     breaks=seq(-3,3,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,2),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_disney_1),sd=sd(r.t_disney_1)),lty=1,col="red")               
qqnorm(r.t_disney_1)                                                                         
qqline(r.t_disney_1)

shapiro.test(r.t_disney_1)                                                                             # Shapiro-Wilk normality test supports normality
ks.test(r.t_disney_1,"pnorm",mean=mean(r.t_disney_1),sd=sd(r.t_disney_1))                              # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Disney", 1, 0, fit.disney_1.bst$d, 0)                            # adding to the table



### wellsfargo_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_wellsfargo_1,ylim=c(0,2),                                                             
        xlab="Year",ylab="GK volatility",main="Wells Fargo Volatility 1/01/2016-12/31/2017")
acf(v.t_wellsfargo_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  # definitely appears to be long-memory
pacf(v.t_wellsfargo_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.wellsfargo_1.0d0 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=0,nma=0,M=50)                # d term significant  
summary(fit.wellsfargo_1.0d0)

fit.wellsfargo_1.1d0 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=1,nma=0,M=50)                # ar term not significant
summary(fit.wellsfargo_1.1d0)

fit.wellsfargo_1.2d0 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=2,nma=0,M=50)                # ar2 not sig
summary(fit.wellsfargo_1.2d0)

fit.wellsfargo_1.0d1 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=0,nma=1,M=50)                # ma sig
summary(fit.wellsfargo_1.0d1)

fit.wellsfargo_1.0d2 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=0,nma=2,M=50)                # ma2 not sig
summary(fit.wellsfargo_1.0d2)

fit.wellsfargo_1.1d1 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=1,nma=1,M=50)                # warning
summary(fit.wellsfargo_1.1d1)

fit.wellsfargo_1.1d2 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=1,nma=2,M=50)                 # warning
summary(fit.wellsfargo_1.1d2)

fit.wellsfargo_1.2d1 <- fracdiff(v.t_wellsfargo_1-mean(v.t_wellsfargo_1),nar=2,nma=1,M=50)                # ar 2 not sig        
summary(fit.wellsfargo_1.2d1)



c(fracdiff.AICC(fit.wellsfargo_1.0d0),fracdiff.AIC(fit.wellsfargo_1.0d0),fracdiff.BIC(fit.wellsfargo_1.0d0))
c(fracdiff.AICC(fit.wellsfargo_1.1d0),fracdiff.AIC(fit.wellsfargo_1.1d0),fracdiff.BIC(fit.wellsfargo_1.1d0)) 
c(fracdiff.AICC(fit.wellsfargo_1.0d1),fracdiff.AIC(fit.wellsfargo_1.0d1),fracdiff.BIC(fit.wellsfargo_1.0d1))



### wellsfargo_1 model diagnostics: autocorrelation in residuals
fit.wellsfargo_1.bst <- fit.wellsfargo_1.0d0                                                         

r.t_wellsfargo_1 <- fit.wellsfargo_1.bst$residuals
summary(r.t_wellsfargo_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_wellsfargo_1,ylim=c(-1,1.5),
        xlab="Year",ylab="GK volatility",main="Wells Fargo Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_wellsfargo_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_wellsfargo_1,lag.max=100,ylim=c(-0.2,1),main="")

### wellsfargo_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_wellsfargo_1,                                                                                   
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_wellsfargo_1),sd=sd(r.t_wellsfargo_1)),lty=1,col="red")               
qqnorm(r.t_wellsfargo_1)                                                                         
qqline(r.t_wellsfargo_1)

shapiro.test(r.t_wellsfargo_1)                                                                                # Shapiro-Wilk normality test supports normality
ks.test(r.t_wellsfargo_1,"pnorm",mean=mean(r.t_wellsfargo_1),sd=sd(r.t_wellsfargo_1))                         # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Wells Fargo", 1, 0, fit.wellsfargo_1.bst$d, 0)     


### jpmorgan_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_jpmorgan_1,ylim=c(0,3),                                                             
        xlab="Year",ylab="GK volatility",main="JP Morgan Volatility 1/01/2016-12/31/2017")
acf(v.t_jpmorgan_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_jpmorgan_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.jpmorgan_1.0d0 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=0,nma=0,M=50)                        # d term significant  
summary(fit.jpmorgan_1.0d0)

fit.jpmorgan_1.1d0 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=1,nma=0,M=50)                        # ar term significant
summary(fit.jpmorgan_1.1d0)

fit.jpmorgan_1.2d0 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=2,nma=0,M=50)                        # ar2 not sig
summary(fit.jpmorgan_1.2d0)

fit.jpmorgan_1.0d1 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=0,nma=1,M=50)                        # ma sig
summary(fit.jpmorgan_1.0d1)

fit.jpmorgan_1.0d2 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=0,nma=2,M=50)                       # all sig
summary(fit.jpmorgan_1.0d2)

fit.jpmorgan_1.1d1 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=1,nma=1,M=50)                       # only d sig
summary(fit.jpmorgan_1.1d1)

fit.jpmorgan_1.1d2 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=1,nma=2,M=50)                       # d, ma2 sig
summary(fit.jpmorgan_1.1d2)

fit.jpmorgan_1.2d1 <- fracdiff(v.t_jpmorgan_1-mean(v.t_jpmorgan_1),nar=2,nma=1,M=50)                       # d, ar1 sig      
summary(fit.jpmorgan_1.2d1)



c(fracdiff.AICC(fit.jpmorgan_1.0d0),fracdiff.AIC(fit.jpmorgan_1.0d0),fracdiff.BIC(fit.jpmorgan_1.0d0))
c(fracdiff.AICC(fit.jpmorgan_1.1d0),fracdiff.AIC(fit.jpmorgan_1.1d0),fracdiff.BIC(fit.jpmorgan_1.1d0)) 
c(fracdiff.AICC(fit.jpmorgan_1.0d1),fracdiff.AIC(fit.jpmorgan_1.0d1),fracdiff.BIC(fit.jpmorgan_1.0d1))
c(fracdiff.AICC(fit.jpmorgan_1.0d2),fracdiff.AIC(fit.jpmorgan_1.0d2),fracdiff.BIC(fit.jpmorgan_1.0d2))


### jpmorgan_1 model diagnostics: autocorrelation in residuals
fit.jpmorgan_1.bst <- fit.jpmorgan_1.0d0                                                         

r.t_jpmorgan_1 <- fit.jpmorgan_1.bst$residuals
summary(r.t_jpmorgan_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_jpmorgan_1,ylim=c(-1,1.5),
        xlab="Year",ylab="GK volatility",main="JP Morgan Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_jpmorgan_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_jpmorgan_1,lag.max=100,ylim=c(-0.2,1),main="")

### jpmorgan_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_jpmorgan_1,                                                                                   
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_jpmorgan_1),sd=sd(r.t_jpmorgan_1)),lty=1,col="red")               
qqnorm(r.t_jpmorgan_1)                                                                         
qqline(r.t_jpmorgan_1)

shapiro.test(r.t_jpmorgan_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_jpmorgan_1,"pnorm",mean=mean(r.t_jpmorgan_1),sd=sd(r.t_jpmorgan_1))                         # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("JP Morgan", 1, 0, fit.jpmorgan_1.bst$d, 0)     



### walmart_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_walmart_1,ylim=c(0,3),                                                             
        xlab="Year",ylab="GK volatility",main="Walmart Volatility 1/01/2016-12/31/2017")
acf(v.t_walmart_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_walmart_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.walmart_1.0d0 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=0,nma=0,M=50)                        # d term significant  
summary(fit.walmart_1.0d0)

fit.walmart_1.1d0 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=1,nma=0,M=50)                        # ar term not significant
summary(fit.walmart_1.1d0)

fit.walmart_1.2d0 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=2,nma=0,M=50)                        # ar terms not sig
summary(fit.walmart_1.2d0)

fit.walmart_1.0d1 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=0,nma=1,M=50)                        # ma not sig
summary(fit.walmart_1.0d1)

fit.walmart_1.0d2 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=0,nma=2,M=50)                       # only d sig
summary(fit.walmart_1.0d2)

fit.walmart_1.1d1 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=1,nma=1,M=50)                       # all sig
summary(fit.walmart_1.1d1)

fit.walmart_1.1d2 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=1,nma=2,M=50)                       # warning
summary(fit.walmart_1.1d2)

fit.walmart_1.2d1 <- fracdiff(v.t_walmart_1-mean(v.t_walmart_1),nar=2,nma=1,M=50)                       # all sig  
summary(fit.walmart_1.2d1)



c(fracdiff.AICC(fit.walmart_1.0d0),fracdiff.AIC(fit.walmart_1.0d0),fracdiff.BIC(fit.walmart_1.0d0))
c(fracdiff.AICC(fit.walmart_1.1d1),fracdiff.AIC(fit.walmart_1.1d1),fracdiff.BIC(fit.walmart_1.1d1)) 
c(fracdiff.AICC(fit.walmart_1.2d1),fracdiff.AIC(fit.walmart_1.2d1),fracdiff.BIC(fit.walmart_1.2d1))



### walmart_1 model diagnostics: autocorrelation in residuals
fit.walmart_1.bst <- fit.walmart_1.0d0                                                         

r.t_walmart_1 <- fit.walmart_1.bst$residuals
summary(r.t_walmart_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_walmart_1,ylim=c(-1,1.5),
        xlab="Year",ylab="GK volatility",main="Walmart Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_walmart_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_walmart_1,lag.max=100,ylim=c(-0.2,1),main="")

### walmart_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_walmart_1,                                                                                   
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_walmart_1),sd=sd(r.t_walmart_1)),lty=1,col="red")               
qqnorm(r.t_walmart_1)                                                                         
qqline(r.t_walmart_1)

shapiro.test(r.t_walmart_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_walmart_1,"pnorm",mean=mean(r.t_walmart_1),sd=sd(r.t_walmart_1))                           # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Walmart", 1, 0, fit.walmart_1.bst$d, 0)     



### intel_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_intel_1,ylim=c(0,1.5),                                                             
        xlab="Year",ylab="GK volatility",main="Intel Volatility 1/01/2016-12/31/2017")
acf(v.t_intel_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_intel_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.intel_1.0d0 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=0,nma=0,M=50)                        # d term significant  
summary(fit.intel_1.0d0)

fit.intel_1.1d0 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=1,nma=0,M=50)                        # ar term not significant
summary(fit.intel_1.1d0)

fit.intel_1.2d0 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=2,nma=0,M=50)                        # ar terms not sig
summary(fit.intel_1.2d0)

fit.intel_1.0d1 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=0,nma=1,M=50)                        # ma not sig
summary(fit.intel_1.0d1)

fit.intel_1.0d2 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=0,nma=2,M=50)                        # only d sig
summary(fit.intel_1.0d2)

fit.intel_1.1d1 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=1,nma=1,M=50)                        # all sig
summary(fit.intel_1.1d1)

fit.intel_1.1d2 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=1,nma=2,M=50)                        # ma2 not sig
summary(fit.intel_1.1d2)

fit.intel_1.2d1 <- fracdiff(v.t_intel_1-mean(v.t_intel_1),nar=2,nma=1,M=50)                        # warning   
summary(fit.intel_1.2d1)



c(fracdiff.AICC(fit.intel_1.0d0),fracdiff.AIC(fit.intel_1.0d0),fracdiff.BIC(fit.intel_1.0d0))
c(fracdiff.AICC(fit.intel_1.1d1),fracdiff.AIC(fit.intel_1.1d1),fracdiff.BIC(fit.intel_1.1d1)) 



### intel_1 model diagnostics: autocorrelation in residuals
fit.intel_1.bst <- fit.intel_1.0d0                                                         

r.t_intel_1 <- fit.intel_1.bst$residuals
summary(r.t_intel_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_intel_1,ylim=c(-1,1),
        xlab="Year",ylab="GK volatility",main="Intel Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_intel_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_intel_1,lag.max=100,ylim=c(-0.2,1),main="")

### intel_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_intel_1,                                                                                   
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_intel_1),sd=sd(r.t_intel_1)),lty=1,col="red")               
qqnorm(r.t_intel_1)                                                                         
qqline(r.t_intel_1)

shapiro.test(r.t_intel_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_intel_1,"pnorm",mean=mean(r.t_intel_1),sd=sd(r.t_intel_1))                               # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Intel", 1, 0, fit.intel_1.bst$d, 0)     


### bankofa_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_bankofa_1,ylim=c(0,1),                                                             
        xlab="Year",ylab="GK volatility",main="Bank of America Volatility 1/01/2016-12/31/2017")
acf(v.t_bankofa_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_bankofa_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.bankofa_1.0d0 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=0,nma=0,M=50)                        # d term significant  
summary(fit.bankofa_1.0d0)

fit.bankofa_1.1d0 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=1,nma=0,M=50)                        # ar term not sig
summary(fit.bankofa_1.1d0)

fit.bankofa_1.2d0 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=2,nma=0,M=50)                        # ar terms not sig
summary(fit.bankofa_1.2d0)

fit.bankofa_1.0d1 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=0,nma=1,M=50)                        # ma not sig
summary(fit.bankofa_1.0d1)

fit.bankofa_1.0d2 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=0,nma=2,M=50)                        # ma terms not sig
summary(fit.bankofa_1.0d2)

fit.bankofa_1.1d1 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=1,nma=1,M=50)                        # all sig
summary(fit.bankofa_1.1d1)

fit.bankofa_1.1d2 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=1,nma=2,M=50)                        # ma 2 not sig
summary(fit.bankofa_1.1d2)

fit.bankofa_1.2d1 <- fracdiff(v.t_bankofa_1-mean(v.t_bankofa_1),nar=2,nma=1,M=50)                        # ar2 not sig   
summary(fit.bankofa_1.2d1)



c(fracdiff.AICC(fit.bankofa_1.0d0),fracdiff.AIC(fit.bankofa_1.0d0),fracdiff.BIC(fit.bankofa_1.0d0))
c(fracdiff.AICC(fit.bankofa_1.1d1),fracdiff.AIC(fit.bankofa_1.1d1),fracdiff.BIC(fit.bankofa_1.1d1)) 



### bankofa_1 model diagnostics: autocorrelation in residuals
fit.bankofa_1.bst <- fit.bankofa_1.0d0                                                         

r.t_bankofa_1 <- fit.bankofa_1.bst$residuals
summary(r.t_bankofa_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_bankofa_1,ylim=c(-0.5,0.5),
        xlab="Year",ylab="GK volatility",main="Bank of America Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_bankofa_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_bankofa_1,lag.max=100,ylim=c(-0.2,1),main="")

### bankofa_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_bankofa_1,                                                                                   
     breaks=seq(-1,1,0.125),
     freq=FALSE,
     col="grey85",ylim=c(0,5),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_bankofa_1),sd=sd(r.t_bankofa_1)),lty=1,col="red")               
qqnorm(r.t_bankofa_1)                                                                         
qqline(r.t_bankofa_1)

shapiro.test(r.t_bankofa_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_bankofa_1,"pnorm",mean=mean(r.t_bankofa_1),sd=sd(r.t_bankofa_1))                           # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Bank of America", 1, 0, fit.bankofa_1.bst$d, 0)     



### verizon_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_verizon_1,ylim=c(0,2),                                                             
        xlab="Year",ylab="GK volatility",main="Verizon Volatility 1/01/2016-12/31/2017")
acf(v.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.verizon_1.0d0 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=0,nma=0,M=50)                       # d term significant  
summary(fit.verizon_1.0d0)

fit.verizon_1.1d0 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=1,nma=0,M=50)                       # ar term not significant
summary(fit.verizon_1.1d0)

fit.verizon_1.2d0 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=2,nma=0,M=50)                       # ar terms sig
summary(fit.verizon_1.2d0)

fit.verizon_1.0d1 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=0,nma=1,M=50)                       # ma not sig
summary(fit.verizon_1.0d1)

fit.verizon_1.0d2 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=0,nma=2,M=50)                       # all sig
summary(fit.verizon_1.0d2)

fit.verizon_1.1d1 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=1,nma=1,M=50)                       # warning
summary(fit.verizon_1.1d1)

fit.verizon_1.1d2 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=1,nma=2,M=50)                       # all sig
summary(fit.verizon_1.1d2)

fit.verizon_1.2d1 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=2,nma=1,M=50)                       # ar2 not sig    
summary(fit.verizon_1.2d1)



c(fracdiff.AICC(fit.verizon_1.0d0),fracdiff.AIC(fit.verizon_1.0d0),fracdiff.BIC(fit.verizon_1.0d0))
c(fracdiff.AICC(fit.verizon_1.2d0),fracdiff.AIC(fit.verizon_1.2d0),fracdiff.BIC(fit.verizon_1.2d0)) 
c(fracdiff.AICC(fit.verizon_1.1d2),fracdiff.AIC(fit.verizon_1.1d2),fracdiff.BIC(fit.verizon_1.1d2))
c(fracdiff.AICC(fit.verizon_1.0d2),fracdiff.AIC(fit.verizon_1.0d2),fracdiff.BIC(fit.verizon_1.0d2))


### verizon_1 model diagnostics: autocorrelation in residuals
fit.verizon_1.bst <- fit.verizon_1.0d0                                                         

r.t_verizon_1 <- fit.verizon_1.bst$residuals
summary(r.t_verizon_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_verizon_1,ylim=c(-1,1.5),
        xlab="Year",ylab="GK volatility",main="Verizon Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")

### verizon_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_verizon_1,                                                                                   
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_verizon_1),sd=sd(r.t_verizon_1)),lty=1,col="red")               
qqnorm(r.t_verizon_1)                                                                         
qqline(r.t_verizon_1)

shapiro.test(r.t_verizon_1)                                                                           # Shapiro-Wilk normality test supports normality
ks.test(r.t_verizon_1,"pnorm",mean=mean(r.t_verizon_1),sd=sd(r.t_verizon_1))                         # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Verizon", 1, 0, fit.verizon_1.bst$d, 0)     

### verizon_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_verizon_1,ylim=c(0,1.5),                                                             
        xlab="Year",ylab="GK volatility",main="Verizon Volatility 1/01/2016-12/31/2017")
acf(v.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.verizon_1.0d0 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=0,nma=0,M=50)                        # d term significant  
summary(fit.verizon_1.0d0)

fit.verizon_1.1d0 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=1,nma=0,M=50)                        # ar term not sig
summary(fit.verizon_1.1d0)

fit.verizon_1.2d0 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=2,nma=0,M=50)                        # all sig
summary(fit.verizon_1.2d0)

fit.verizon_1.0d1 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=0,nma=1,M=50)                        # ma not sig
summary(fit.verizon_1.0d1)

fit.verizon_1.0d2 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=0,nma=2,M=50)                        # all sig
summary(fit.verizon_1.0d2)

fit.verizon_1.1d1 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=1,nma=1,M=50)                       # warning
summary(fit.verizon_1.1d1)

fit.verizon_1.1d2 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=1,nma=2,M=50)                       # all sig
summary(fit.verizon_1.1d2)

fit.verizon_1.2d1 <- fracdiff(v.t_verizon_1-mean(v.t_verizon_1),nar=2,nma=1,M=50)                       # ar2 not sig      
summary(fit.verizon_1.2d1)



c(fracdiff.AICC(fit.verizon_1.0d0),fracdiff.AIC(fit.verizon_1.0d0),fracdiff.BIC(fit.verizon_1.0d0))
c(fracdiff.AICC(fit.verizon_1.2d0),fracdiff.AIC(fit.verizon_1.2d0),fracdiff.BIC(fit.verizon_1.2d0)) 
c(fracdiff.AICC(fit.verizon_1.1d2),fracdiff.AIC(fit.verizon_1.1d2),fracdiff.BIC(fit.verizon_1.1d2))
c(fracdiff.AICC(fit.verizon_1.0d2),fracdiff.AIC(fit.verizon_1.0d2),fracdiff.BIC(fit.verizon_1.0d2))


### verizon_1 model diagnostics: autocorrelation in residuals
fit.verizon_1.bst <- fit.verizon_1.0d0                                                         

r.t_verizon_1 <- fit.verizon_1.bst$residuals
summary(r.t_verizon_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_verizon_1,ylim=c(-1,1),
        xlab="Year",ylab="GK volatility",main="Verizon Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_verizon_1,lag.max=100,ylim=c(-0.2,1),main="")

### verizon_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_verizon_1,                                                                                   
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_verizon_1),sd=sd(r.t_verizon_1)),lty=1,col="red")               
qqnorm(r.t_verizon_1)                                                                         
qqline(r.t_verizon_1)

shapiro.test(r.t_verizon_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_verizon_1,"pnorm",mean=mean(r.t_verizon_1),sd=sd(r.t_verizon_1))                           # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Verizon", 1, 0, fit.verizon_1.bst$d, 0)     



### att_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_att_1,ylim=c(0,1.5),                                                             
        xlab="Year",ylab="GK volatility",main="ATT Volatility 1/01/2016-12/31/2017")
acf(v.t_att_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_att_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.att_1.0d0 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=0,nma=0,M=50)                        # d term significant  
summary(fit.att_1.0d0)

fit.att_1.1d0 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=1,nma=0,M=50)                        # ar term significant
summary(fit.att_1.1d0)

fit.att_1.2d0 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=2,nma=0,M=50)                        # ar1 ar2 not sig
summary(fit.att_1.2d0)

fit.att_1.0d1 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=0,nma=1,M=50)                        # ma sig
summary(fit.att_1.0d1)

fit.att_1.0d2 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=0,nma=2,M=50)                        # ma1 not sig
summary(fit.att_1.0d2)

fit.att_1.1d1 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=1,nma=1,M=50)                        # all sig
summary(fit.att_1.1d1)

fit.att_1.1d2 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=1,nma=2,M=50)                        # ma2 not sig
summary(fit.att_1.1d2)

fit.att_1.2d1 <- fracdiff(v.t_att_1-mean(v.t_att_1),nar=2,nma=1,M=50)                        # ar2 not sig     
summary(fit.att_1.2d1)



c(fracdiff.AICC(fit.att_1.0d0),fracdiff.AIC(fit.att_1.0d0),fracdiff.BIC(fit.att_1.0d0))
c(fracdiff.AICC(fit.att_1.1d0),fracdiff.AIC(fit.att_1.1d0),fracdiff.BIC(fit.att_1.1d0)) 
c(fracdiff.AICC(fit.att_1.0d1),fracdiff.AIC(fit.att_1.0d1),fracdiff.BIC(fit.att_1.0d1))
c(fracdiff.AICC(fit.att_1.1d1),fracdiff.AIC(fit.att_1.1d1),fracdiff.BIC(fit.att_1.1d1))


### att_1 model diagnostics: autocorrelation in residuals
fit.att_1.bst <- fit.att_1.0d0                                                         

r.t_att_1 <- fit.att_1.bst$residuals
summary(r.t_att_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_att_1,ylim=c(-1,1),
        xlab="Year",ylab="GK volatility",main="ATT Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_att_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_att_1,lag.max=100,ylim=c(-0.2,1),main="")

### att_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_att_1,                                                                                   
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_att_1),sd=sd(r.t_att_1)),lty=1,col="red")               
qqnorm(r.t_att_1)                                                                         
qqline(r.t_att_1)

shapiro.test(r.t_att_1)                                                                            # Shapiro-Wilk normality test supports normality
ks.test(r.t_att_1,"pnorm",mean=mean(r.t_att_1),sd=sd(r.t_att_1))                                   # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("ATT", 1, 0, fit.att_1.bst$d, 0)     


### homedep_1 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_homedep_1,ylim=c(0,4),                                                             
        xlab="Year",ylab="GK volatility",main="Home Depot Volatility 1/01/2016-12/31/2017")
acf(v.t_homedep_1,lag.max=100,ylim=c(-0.2,1),main="")                                                  
pacf(v.t_homedep_1,lag.max=100,ylim=c(-0.2,1),main="")                                        

fit.homedep_1.0d0 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=0,nma=0,M=50)                        # d term significant  
summary(fit.homedep_1.0d0)

fit.homedep_1.1d0 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=1,nma=0,M=50)                        # ar term not sig
summary(fit.homedep_1.1d0)

fit.homedep_1.2d0 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=2,nma=0,M=50)                        # ar terms not sig
summary(fit.homedep_1.2d0)

fit.homedep_1.0d1 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=0,nma=1,M=50)                        # ma not sig
summary(fit.homedep_1.0d1)

fit.homedep_1.0d2 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=0,nma=2,M=50)                        # ma terms not sig
summary(fit.homedep_1.0d2)

fit.homedep_1.1d1 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=1,nma=1,M=50)                        # all terms sig
summary(fit.homedep_1.1d1)

fit.homedep_1.1d2 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=1,nma=2,M=50)                        # warning
summary(fit.homedep_1.1d2)

fit.homedep_1.2d1 <- fracdiff(v.t_homedep_1-mean(v.t_homedep_1),nar=2,nma=1,M=50)                        # ar2 not sig  
summary(fit.homedep_1.2d1)



c(fracdiff.AICC(fit.homedep_1.0d0),fracdiff.AIC(fit.homedep_1.0d0),fracdiff.BIC(fit.homedep_1.0d0))
c(fracdiff.AICC(fit.homedep_1.1d1),fracdiff.AIC(fit.homedep_1.1d1),fracdiff.BIC(fit.homedep_1.1d1)) 


### homedep_1 model diagnostics: autocorrelation in residuals
fit.homedep_1.bst <- fit.homedep_1.0d0                                                         

r.t_homedep_1 <- fit.homedep_1.bst$residuals
summary(r.t_homedep_1)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_homedep_1,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="Home Depot Volatility Residuals 1/01/2016-12/31/2017")
abline(h=0,col="blue",lty=2)
acf(r.t_homedep_1,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_homedep_1,lag.max=100,ylim=c(-0.2,1),main="")

### homedep_1 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_homedep_1,                                                                                   
     breaks=seq(-3,3,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,1.5),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_homedep_1),sd=sd(r.t_homedep_1)),lty=1,col="red")               
qqnorm(r.t_homedep_1)                                                                         
qqline(r.t_homedep_1)

shapiro.test(r.t_homedep_1)                                                                      # Shapiro-Wilk normality test supports normality
ks.test(r.t_homedep_1,"pnorm",mean=mean(r.t_homedep_1),sd=sd(r.t_homedep_1))                     # KS test supports normality

bst.models[nrow(bst.models)+1,] <- c("Home Depot", 1, 0, fit.homedep_1.bst$d, 0)     


### MODEL FITTING PART TWO DATA ---------------------------------------------------------------------

### microsoft_2 ARFIMA model
dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(v.t_microsoft_2,ylim=c(0,5),
        xlab="Year",ylab="GK volatility",main="Microsoft Volatility 2/01/2018-12/31/2019")      # acf appears to show long-memory
acf(v.t_microsoft_2,lag.max=100,ylim=c(-0.2,1),main="")
pacf(v.t_microsoft_2,lag.max=100,ylim=c(-0.2,1),main="")

fit.microsoft_2.0d0 <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=0,nma=0,M=50)
summary(fit.microsoft_2.0d0)

fit.microsoft_2.1d0 <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=1,nma=0,M=50)         # p value on AR param 0.965
summary(fit.microsoft_2.1d0)

fit.microsoft_2.2d0 <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=2,nma=0,M=50)         # AR terms not significant
summary(fit.microsoft_2.2d0)

fit.microsoft_2.0d1 <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=0,nma=1,M=50)         # MA term close to zero and not significant
summary(fit.microsoft_2.0d1)

fit.microsoft_2.1d1 <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=1,nma=1,M=50)         # all terms significant
summary(fit.microsoft_2.1d1)

fit.microsoft_2.0d0_b <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=0,nma=0,M=30)       # changed M value to 30, results are the same as above
summary(fit.microsoft_2.0d0_b)

fit.microsoft_2.2d1 <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=2,nma=1,M=50)         # received warning when computing correlation matrix
summary(fit.microsoft_2.2d1)

fit.microsoft_2.1d2 <- fracdiff(v.t_microsoft_2-mean(v.t_microsoft_2),nar=1,nma=2,M=50)         # received warning when computing correlation matrix
summary(fit.microsoft_2.1d2)


### microsoft_2 model checking the AICC, AIC, BIC
c(fracdiff.AICC(fit.microsoft_2.0d0),fracdiff.AIC(fit.microsoft_2.0d0),fracdiff.BIC(fit.microsoft_2.0d0))  
c(fracdiff.AICC(fit.microsoft_2.1d0),fracdiff.AIC(fit.microsoft_2.1d0),fracdiff.BIC(fit.microsoft_2.1d0))
c(fracdiff.AICC(fit.microsoft_2.0d1),fracdiff.AIC(fit.microsoft_2.0d1),fracdiff.BIC(fit.microsoft_2.0d1))
c(fracdiff.AICC(fit.microsoft_2.1d1),fracdiff.AIC(fit.microsoft_2.1d1),fracdiff.BIC(fit.microsoft_2.1d1))

### microsoft_2 model diagnostics: autocorrelation in residuals
fit.microsoft_2.bst <- fit.microsoft_2.0d0                                                       # this minimizes AICC, AIC, BIC

r.t_microsoft_2 <- fit.microsoft_2.bst$residuals
summary(r.t_microsoft_2)                                                                        


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_microsoft_1,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="Microsoft Volatility Residuals 2/01/2018-12/31/2019")
abline(h=0,col="blue",lty=2)
acf(r.t_microsoft_2,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_microsoft_2,lag.max=100,ylim=c(-0.2,1),main="")

### microsoft_2 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_microsoft_2,                                                                           # histogram of residuals
     breaks=seq(-4,4,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,1.5),
     main="Residual Histogram")                                                                 # looks close to normal
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_microsoft_2),sd=sd(r.t_microsoft_2)),lty=1,col="red")             # add theoretical normal density
qqnorm(r.t_microsoft_2)                                                                         # normal Q-Q plot
qqline(r.t_microsoft_2)

shapiro.test(r.t_microsoft_2)                                                                   # Shapiro-Wilk test indicates normality
ks.test(r.t_microsoft_2,"pnorm",mean=mean(r.t_microsoft_2),sd=sd(r.t_microsoft_2))              # Kolmogorov-Smirnov indicates normality



### oracle_2 model diagnostics: autocorrelation in residuals
fit.oracle_2.bst <- fit.oracle_2.0d0                                                          # this minimizes BIC but AICC and AIC are very close to min val

r.t_oracle_2 <- fit.oracle_2.bst$residuals
summary(r.t_oracle_2)                                                                         


dev.new(width=12,height=6)
par(mfrow=c(3,1),mex=0.75)
plot.ts(r.t_oracle_2,ylim=c(-2,2),
        xlab="Year",ylab="GK volatility",main="Oracle Volatility Residuals 2/01/2018-12/31/2019")
abline(h=0,col="blue",lty=2)
acf(r.t_oracle_2,lag.max=100,ylim=c(-0.2,1),main="")
pacf(r.t_oracle_2,lag.max=100,ylim=c(-0.2,1),main="")

### oracle_2 residual normality check
dev.new(height=6,width=12)
par(mfrow=c(1,2),mex=0.75)
hist(r.t_oracle_2,                                                                           # histogram of residuals
     breaks=seq(-2,2,0.25),
     freq=FALSE,
     col="grey85",ylim=c(0,3),
     main="Residual Histogram")                                                              
z <- seq(-60,60,length=1000)                                      
lines(z,dnorm(z,mean=mean(r.t_oracle_2),sd=sd(r.t_oracle_2)),lty=1,col="red")                # add theoretical normal density
qqnorm(r.t_oracle_2)                                                                        
qqline(r.t_oracle_2)

shapiro.test(r.t_oracle_2)                                                                   # Shapiro-Wilk normality test supports normality
ks.test(r.t_oracle_2,"pnorm",mean=mean(r.t_oracle_2),sd=sd(r.t_oracle_2))                    # KS test supports normality



