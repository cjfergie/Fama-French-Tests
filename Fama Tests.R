rm(list = ls())

install.packages("fGarch")
install.packages("quantmod")
install.packages("AER")
install.packages("sandwich")
install.packages("stargazer")
install.packages("survival")
install.packages("plm")
install.packages("margins")
install.packages("readxl")
install.packages("xts")
install.packages("dynlm")
install.packages("zoo")
install.packages("urca")
install.packages("strucchange")
install.packages("orcutt")
install.packages("fGarch")
install.packages("quantmod")
install.packages("tidyquant")
install.packages("tidyverse")
install.packages("timetk")
install.packages("broom")
install.packages("glue")

library("AER")
library("sandwich")
library("stargazer")
library("survival")
library("plm")
library("margins")
library("readxl")
library("xts")
library("dynlm")
##library("zoo")
library("urca")
library("strucchange")
library("orcutt")
library("fGarch")
library("quantmod")
library("tidyquant")
library("tidyverse")
library("timetk")
library("broom")
library("glue")

setwd("~/Economics/Fama Tests")

##Retrieving Files From Kenneth French
temp <- tempfile()
base <-
  "https://mba.tuck.dartmouth.edu/pages/faculty/ken.french/data_library.html/"

factor <-
  "Fama/French_3_Factors[Weekly]"

format <-
  "_CSV.zip"

full_url <- glue(base, factor, format, sep = "")

##Downloading File
download.file(full_url, temp, quiet = TRUE) ##getting error

##Reading File
FF_3_Factors <- read.csv(unz(temp, 
                             "F-F_Research_Data_Factors_weekly_CSV"))

FF_3_Factors <- read.csv("F-F_Research_Data_Factors_weekly.csv")


##Fama French Testing
symbols <- c("SPY", "EFA", "IJS", "EEM", "AGG")
prices <- 
  getSymbols(symbols, src = "yahoo", 
                     from = "2012-12-31", 
                     to = "2022-05-31", 
                     auto.assign = TRUE, warnings = FALSE) %>%

map(-Ad(get(.))) %>%
  reduce(merge) %>%
  colnames <- (symbols)

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

asset_returns_long <- 
  prices %>%
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>%
  gather(asset, returns, -date) %>%
  mutate(returns = (log(returns) - log(lag(returns)))) %>%
  na.omit()

portfolio_returns_tq_rebalanced_monthly <-
  asset_returns_long %>%
  tq_portfolio(assets_col = asset, 
               returns_col = returns, 
               weights = w, 
               col_rename = "returns", 
               rebalance_on = "months")
+ portfolio_returns_tq_rebalanced_monthly

