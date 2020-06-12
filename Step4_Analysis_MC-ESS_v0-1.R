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

      Table.unc[i,j] <- p.val.unc
      Table.ess[i,j] <- p.val.ess
    }
  }

  list(p.val_unc=Table.unc,p.val_ess=Table.ess)
}

### Dataset for the period 1
data.Volatility_prd.1 <- 

### Apply LSD method
ess.LSD(data=data.Volatility_prd.1,alternative="two.sided")



