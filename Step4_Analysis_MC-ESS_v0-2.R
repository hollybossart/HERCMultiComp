#*[-----------------------------------------------------------------------------------------------]*#
#*[ Objectives : This program performas ESS-calibrated MC tests for long memory time series       ]*#
#*[ Last update: 06/12/2020                                                                       ]*#
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


### R function to compute ACF of ARFIMA(0,d,0)
acf.ARFIMA0d0 <- function(n,d,sig) {
  h <- 1:(n-1)
  cov.0 <- sig^2*exp(lgamma(1-2*d)-2*lgamma(1-d))

  if (d>0) {
    cor.h <- exp(lgamma(h+d)+lgamma(1-d)-lgamma(h-d+1)-lgamma(d))
  }
  if (d==0) {
    cor.h <- 0*h
  } 
  if (d<0) {
    cor.h <- exp(lgamma(h+d)+lgamma(1-d)-lgamma(h-d+1))/gamma(d)
  }

  return(list(gamma.0=cov.0,rho.h=cor.h))
}


### Fisher's LSD method
ess.LSD <- function(data,alternative=c("two.sided","less","greater")) {
                                                    # data: com.1, com.2, ..., com.m
  m <- ncol(data)                                   # m: no of companies
  n <- nrow(data)                                   # n: sample size of each series
  N <- m*n                                          # N: sample size total
  h <- 1:(n-1)                                      # h: lag of estimated acf

  x.avg <- numeric(length=m)                        # sample mean of each company
  x.var <- numeric(length=m)
  d.est <- numeric(length=m)
  sig.est <- numeric(length=m)
  n.ess <- numeric(length=m)

  for (j in 1:m) {
    x <- data[,j]

    x.avg[j] <- mean(x,na.rm=TRUE)
    x.var[j] <- var(x,na.rm=TRUE)

    fit.0d0  <- fracdiff(x-mean(x),nar=0,nma=0,M=50)
    d.est[j] <- fit.0d0$d
    sig.est[j] <- fit.0d0$sigma

    acf.est <- acf.ARFIMA0d0(n=n,d=d.est[j],sig=sig.est[j])
    #n.ess[j] <- max(n-2*sum((1-h/n)*acf.est$rho.h),2.5)
    n.ess[j] <- max(n/(1+2*sum((1-h/n)*acf.est$rho.h)),2.5)
  }

  N.ess <- sum(n.ess)

  sse <- sum((n-1)*x.var)
  mse.unc <- sse/(N-m)
  mse.ess <- sse/(N.ess-m)

  Table.unc <- matrix(NA,nrow=m,ncol=m)
  Table.ess <- matrix(NA,nrow=m,ncol=m)
  for (i in 2:m) {
    for (j in 1:(i-1)) {
      dif.val <- x.avg[i] - x.avg[j]
      dif.se.unc <- sqrt(mse.unc*(1/n+1/n))
      dif.se.ess <- sqrt(mse.ess*(1/n+1/n))

      t.val.unc <- dif.val/dif.se.unc
      t.val.ess <- dif.val/dif.se.ess

      if (alternative == "two.sided") {
        p.val.unc <- 2*pt(-abs(t.val.unc),N-m)
        p.val.ess <- 2*pt(-abs(t.val.ess),N.ess-m)
      }
      else {
        p.val.unc <- pt(t.val,N-m,lower.tail=(alternative == "less"))
        p.val.ess <- pt(t.val,N.ess-m,lower.tail=(alternative == "less"))
      }

      Table.unc[i,j] <- round(p.val.unc, digits = 7)     # rounds digits to 7 digits to avoid reporting in sci notation          
      Table.ess[i,j] <- round(p.val.ess, digits = 7)
    }
  }

  list(p.val_unc=Table.unc,p.val_ess=Table.ess)
}

### Data import
data.OHLC <- read.csv(file=paste(WD.inp,"OHLCprices.csv",sep=""))
data.OHLC[1:10,]

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

start_2 <- end_1 + 28*deltat(v.t_oracle)        # there were 21 trading days in january 2018 + 7 days to get rid of outliers
# doing this calculation gives the start time for feb 8 2018
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


### Dataset for the period 1
data.Volatility_prd.1 <- cbind.data.frame(v.t_oracle_1,
                                          v.t_microsoft_1,
                                          v.t_exxon_1,
                                          v.t_gm_1,
                                          v.t_ibm_1,
                                          v.t_facebook_1,
                                          v.t_chevron_1,
                                          v.t_apple_1,
                                          v.t_alibaba_1,
                                          v.t_pg_1,
                                          v.t_pfizer_1,
                                          v.t_johnson_1,
                                          v.t_disney_1,
                                          v.t_wellsfargo_1,
                                          v.t_jpmorgan_1,
                                          v.t_walmart_1,
                                          v.t_intel_1,
                                          v.t_bankofa_1,
                                          v.t_verizon_1,
                                          v.t_att_1,
                                          v.t_homedep_1,
                                          v.t_citi_1,
                                          v.t_amazon_1,
                                          v.t_chinamob_1,
                                          v.t_taiwan_1,
                                          v.t_novartis_1,
                                          v.t_visa_1,
                                          v.t_unhealth_1,
                                          v.t_busch_1,
                                          v.t_netflix_1)

dim(data.Volatility_prd.1)                # 503 observations for 30 companies in part 1




### Writing results to a csv file
write.csv(results_prd.1$p.val_unc, file = "results_prd_1_unc.csv")
