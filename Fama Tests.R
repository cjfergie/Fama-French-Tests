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
                             "F-F_Research_Data_Factors_weekly_CSV.csv"))

FF_3_Factors <- read.csv("F-F_Research_Data_Factors_weekly.csv")


##Fama French Testing
symbols <- c("SPY", "EFA", "IJS", "EEM", "AGG")
prices <- 
  getSymbols(symbols, src = "yahoo", 
                     from = "2012-12-31", 
                     to = "2022-05-31", 
                     auto.assign = TRUE, warnings = FALSE) %>%

head(FF_3_Factors)
### A tibble: 6 x 1
## This file was created using the 201802 Bloomberg database
##<chr>
## 1 Missing data is indicated by -99.99
## 2 <NA>
## 3 199007
## 4 199008
## 5 199009
## 6 199010

## Should see column with wierdly imported dates
## can be fixed by skipping certain number of rows containing metadata
## do this....
FF_3_Factors <- read.csv(unz(temp, 
                             "F-F_Research_Data_Factors_weekly_CSV.csv"), 
                        skip = 6)

FF_3_Factors <- read.csv(("F-F_Research_Data_Factors_weekly.csv"), skip = 6)
head(FF_3_Factors)

map(FF_3_Factors) ##reveals data has been coerced into character format
##_______________________________________________________________________________
## DO EITHER
## This specifically,
FF_3_Factors <- read.csv(unz(temp, 
                             "F-F_Research_Data_Factors_weekly_CSV.csv"), 
                        skip = 6, 
                        col_types = cols(
                        Mkt-RF = col_double(), 
                        SMB = col_double(), 
                        HML = col_double(), 
                        RF = col_double())

head(FF_3_Factors)                         

## OR THIS GENERALLY AT THE BEGINNING,
FF_3_Factors <- read.csv(unz(temp, 
                             "F-F_Research_Data_Factors_weekly_CSV.csv"), 
                        skip = 6) %>%
rename(date = X1) %>%
mutate_at(vars(-date), as.numeric)

head(FF_3_Factors)
##__________________________________________________________________________________

## Moving forward...
FF_3_Factors <- read.csv(unz(temp, 
                             "F-F_Research_Data_Factors_weekly_CSV.csv"), 
                        skip = 6) %>%
rename(date = X1) %>%
mutate_at(vars(-date), as.numeric) %>%
mutate(date = ymd(parse_date_time(date, "%Y%m"))

head(FF_3_Factors)

FF_3_Factors %>%
select(date) %>%
mutate(date = lubridate::rollback(date)) %>%
head(1)

##___________________________________________________________________________________
## If we want to reset dates to last month, add one first and then start rollback
FF_3_Factors %>%
select(date) %>%
mutate(date = lubridate::rollback(date + months(1))) %>%
head(1)                  
##____________________________________________________________________________________

## ONLY FACTOR THAT ALIGNS WITH OUR PORTFOLIO DATA
## FLTER BY FIRST AND LAST DATE
## IN PORTFOLIO RETURNS OBJECT
FF_3_Factors <- read.csv(unz(temp, 
                             "F-F_Research_Data_Factors_weekly_CSV.csv"), 
                        skip = 6) %>%
rename(date = X1) %>%
mutate_at(vars(-date), as.numeric) %>%
mutate(date = ymd(parse_date_time(date, "%Y%m") + months(1)))) %>%
filter(date >=
       first(portfolio_returns_tq_rebalanced_monthly$date) & date <=
       last(portfolio_returns_tq_rebalanced_monthly$date))

head(FF_3_Factors, 3)

## MERGE DATA OBJECTS
## CONVERT FF TO DECIMAL
## CREATE NEW COLUMN TO HOLD RETURNS ABOVE RISK
ff_portfolio_returns <- 
                         portfolio_returns_tq_rebalanced_monthly %>%
                         left_join(FF_3_Factors, by = "date") %>%
                         mutate(MKT_RF = FF_3_Factors$'Mkt-Rf'/100,
                                SMB = FF_3_Factors$SMB/100, 
                                HML = FF_3_Factors$HML/100, 
                                RF = FF_3_Factors$RF/100, 
                                R_excess = round(returns - RF, 4))
                         
head(ff_portfolio_returns, 4)

## Running a linear model with 95% confidence interval for model coef
ff_dplyr_byhand <- 
                    ff_portfolio_returns %>%
                         do(model = 
                            lm(R_excess ~ MKT_RF + SMB + HML, data = .)) %>%
                         tidy(model, conf.int = T, conf.level = 0.95)

ff_dplyr_byhand %>%
                         mutate_if(is.numeric, funs(round(., 3))) %>%
                         select(-statistic)

## Pipe results to ggplot
ff_dplyr_byhand %>% 
  mutate_if(is.numeric, funs(round(., 3))) %>%
  filter(term != "(Intercept)") %>% 
  ggplot(aes(x = term, y = estimate, shape = term, color = term)) + 
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high)) +
  labs(title = "FF 3-Factor Coefficients for Our Portfolio",
       subtitle = "nothing in this post is investment advice",
       x = "",
       y = "coefficient",
       caption = "data source: Fama French website and yahoo! Finance") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption  = element_text(hjust = 0))
  
                         
##Returning to this momentarily
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

