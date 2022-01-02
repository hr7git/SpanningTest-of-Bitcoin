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
library(car)  # lht 
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
load("data_quant.RData")

##########################################################
################# lm formula #############################
lm_formula <- list( "`BTC-USD` ~ . -`ETH-USD`",
                    "`ETH-USD` ~ . -`BTC-USD`",
                    "`BTC-USD` + `ETH-USD` ~ . ") # i = 1:3

lm_formula2 <- list( "`BTC-USD` ~ . -1 -`ETH-USD`",   # intercept = 0
                       "`ETH-USD` ~ . -1 -`BTC-USD`",
                       "`BTC-USD` + `ETH-USD` ~ . -1 ") # i = 1:3
                  
year <- list( "2017" , "2018" , "2019" , "2020" , "2021") # j=1:5

                 

i = 1
j = 4

fmla <- as.formula(lm_formula[[i]])
fmla2 <- as.formula(lm_formula2[[i]])
lm_data <- rets[year[[j]]]


######### Function #################################
###############################################################
#  model_q  : regression
#  model_q2 : regression with intercept = 0
###################################################################
model_q <- lm(fmla, data = lm_data)    # regression 
str(fmla)
summary(model_q)

model_q2 <- lm(formula = fmla2, data = lm_data)  # regression alpha=0
str(fmla2)
summary(model_q2)
###############################################################
#  from original regression alpha, beta  : model_q1
#  HK test - F, Pr
#  Step1 test - F, Pr
#  step2 test - R, Pr    : model_q2
###################################################################

##### regression model_q
model_q$coefficients[1]   # alpha 
sum(model_q$coefficients) - model_q$coefficients[1]  # beta

##### HK test
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

##### step- test 2 : beta=1 condition on alpha = 0
##### unresrticted model condition on alpha =0  : model_q2
lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
step2_test_q <- lht(model_q2,lhs,c(1))
step2_test_q
step2_test_q[2,5]  # F test  : step- test 2
step2_test_q[2,6]  # Pr(>F)  : step- test 2


