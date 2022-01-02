# pkg = c('magrittr', 'quantmod', 'rvest', 'httr', 'jsonlite',
#         'readr', 'readxl', 'stringr', 'lubridate', 'dplyr',
#         'tidyr', 'ggplot2', 'corrplot', 'dygraphs',
#         'highcharter', 'plotly', 'PerformanceAnalytics',
#         'nloptr', 'quadprog', 'RiskPortfolios', 'cccp',
#         'timetk', 'broom', 'stargazer', 'timeSeries')
# 
# new.pkg = pkg[!(pkg %in% installed.packages()[, "Package"])]
# if (length(new.pkg)) {
#   install.packages(new.pkg, dependencies = TRUE)}
#########################################

library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
#########################################


symbols = c('SPY', # US stock
            'IEV', # Eu stock
            'EWJ', # Japan stock
            'EEM', # Emerging
            'TLT', # US long Bond
            'IEF', # US middle Bond
            'IYR', # Us Rits
            'RWX', # Global Rits
            'GLD', # Gold
            'DBC', # Commodity
            'BTC-USD',  # Bitcoin USD 
            'ETH-USD'   # Etherium USD 
)
getSymbols(symbols, src = 'yahoo')

getSymbols('BTC-USD', src = 'yahoo')

###########################################################

prices = do.call(cbind,
                 lapply(symbols, function(x) Ad(get(x)))) %>%
  setNames(symbols)

rets = Return.calculate(prices) %>% na.omit()

##############################################################
library(tidyr)

##############################################################
library(dplyr)
library(corrplot)

cor(rets) %>%
  corrplot(method = 'color', type = 'lower',
           addCoef.col = 'black', number.cex = 0.7,
           tl.cex = 1, tl.srt = 0, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar = c(0,0,0.5,0))


#################################################################

covmat = cov(rets)
#################### Chart  ###########################

chart_Series(Ad(GLD),Ad(SPY))
chartSeries(`BTC-USD`)
chartSeries(`ETH-USD`)
##################### Image ###################################
save.image(file="data_quant.RData") 


#################
###############################################################
yi <- list("`BTC-USD`","'ETH-USD'","'BTC-USD' + 'ETH-USD'")
i = 1
lm_formula = paste0(yi[i] , " ~ ", "SPY + IEV + EWJ + EEM + TLT",
                      " + IEF + IYR + RWX + GLD + DBC")
lm_formula

model_qf <- 0
lm_data <- rets
model_qf <- lm(formula = lm_formula, data = lm_data)
summary(model_qf)
               

###############################################################
# HK test , Step test of Bitcoin

model_q <- lm(`BTC-USD` ~ SPY + IEV + EWJ + EEM + TLT + IEF + IYR + RWX + GLD + DBC , data = rets)
summary(model_q)

model_q <- lm(`BTC-USD` ~ . -`ETH-USD` , data = rets)
summary(model_q)

##########   zoo -> data frame  ########################
# df_rets <- fortify.zoo(rets)
# model_qf <- lm(`BTC-USD` ~ SPY + IEV + EWJ + EEM + TLT + IEF + IYR + RWX + GLD + DBC , data = df_rets)
# summary(model_qf)
###################################################################


model_q$coefficients[1]   # alpha 
sum(model_q$coefficients) - model_q$coefficients[1]  # beta

##### HK test
library(car)
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
HK_test_q <- lht(model_q,lhs,c(0,1))
HK_test_q
HK_test_q[2,5]  # F test  - HK test
HK_test_q[2,6]  # Pr(>F)  - HK test

##### step-1 test  : alpha = 0
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
step1_test_q <- lht(model_q,lhs,c(0))
step1_test_q
step1_test_q[2,5]  # F test  - step-1 test
step1_test_q[2,6]  # Pr(>F)  : step-1 test

##### step- test 2 : 
##### unresrticted model condition on alpha =0  
model_q2 <- lm(`BTC-USD` ~ 0 + SPY + IEV + EWJ + EEM + TLT + IEF + IYR + RWX + GLD + DBC , data = rets)
summary(model_q2)

##### step- test 2 : beta=1 condition alpha = 0
lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
step2_test_q <- lht(model_q2,lhs,c(1))
step2_test_q[2,5]  # F test  : step- test 2
step2_test_q[2,6]  # Pr(>F)  : step- test 2


