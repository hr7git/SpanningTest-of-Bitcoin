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

# https://hyunyulhenry.github.io/quant_cookbook/

##########################################################
#  Data make
##########################################################
# 
library(quantmod)  # getsymbols
library(PerformanceAnalytics)
library(magrittr)
library(car)  # lht 
library(tidyr)
library(dplyr)
##############  get data from yahoo   ##########################################

symbols = c('SPY', # US stock S&P 500
            'QQQ', # US tech stock
            'IEV', # Eu stock
            'EWJ', # Japan stock
            'EEM', # Emerging
            'TLT', # US long Bond
            'IEF', # US middle Bond
            'IYR', # Us Rits
            'RWX', # Global Rits
            'GLD', # Gold
            'DBC'  # Commodity
)


getSymbols(symbols, src = 'yahoo', from = '2014-01-01') 

# Bitcoin USD  & Etherium USD 
BTC = getSymbols('BTC-USD', src = 'yahoo', from = '2014-01-01',auto.assign=FALSE) 
ETH = getSymbols('ETH-USD', src = 'yahoo', from = '2014-01-01',auto.assign=FALSE) 
# QQQ = getSymbols('QQQ', src = 'yahoo', from = '2014-01-01',auto.assign=FALSE) 

save.image(file="data_getsymbols.RData") 
##############   data price ret=returns    #####################################

# symbols_BTC = c( symbols , 'BTC')
# symbols_ETH = c( symbols,  'ETH')

### variables setting : symbol + BTC
# assets <- c( symbols , 'BTC','ETH')
assets <- c( symbols , 'BTC')
### Data procedure - main
prices = do.call(cbind,
                 lapply(assets, function(x) Ad(get(x)))) %>%
  setNames(assets)

rets = Return.calculate(prices) %>% na.omit()

### correlation graph
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
chart_Series(Ad(BTC))
chartSeries(`BTC`)
chartSeries(`SPY`)
##################### Image ###################################
# save.image(file="data_quant.RData") 
# load("data_quant.RData")





# Not run below
##########################################################
#  Regression
##########################################################
model_q <- lm(`BTC` ~ . -`ETH`, data=rets)    # regression 
model_q <- lm(BTC ~ (SPY + IEV + EWJ + EEM + TLT + IEF + IYR + RWX + GLD + DBC), data=rets)
model_q <- lm(BTC ~ (SPY + EEM + TLT + IEF + IYR + RWX + GLD + DBC), data=rets)
model_q <- lm(BTC ~ (SPY + EEM + TLT + IYR + RWX + GLD + DBC), data=rets)
model_q <- lm(BTC ~ (SPY + EEM + TLT + IYR + GLD + DBC), data=rets)
model_q <- lm(BTC ~ (SPY + EEM + TLT + IYR + GLD + DBC), data=rets["/2019"])
model_q <- lm(BTC ~ (SPY + EEM + TLT + IYR + GLD + DBC), data=rets["2020/"])
model_q <- lm(`BTC` ~ . -`ETH`, data=rets)    # regression 
model_q <- lm(`BTC` ~ . -`ETH`, data=rets["/2019"])    # regression 
model_q <- lm(`BTC` ~ . -`ETH`, data=rets["2020/"])    # regression 
model_q2 <- lm(`BTC` ~ . -1 -`ETH`, data=rets)    # regression 
# bench Aseets : SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC
model_q <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets)
model_q <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets["/2019"])
model_q <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets["2020/"])

summary(model_q)
model_q$coefficients[1]   # alpha
sum(model_q$coefficients) - model_q$coefficients[[1]] # beta

##### HK test
# lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
# 
# The hypothesis matrix : 
# the rows of which specify linear combinations of the model coefficients
hypothesis.matrix <- rbind(c(1,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1))
rhs=c(0,1)   # right-hand-side vector for hypothesis

HK_test_q <- lht(model_q,hypothesis.matrix,rhs)
HK_test_q

HK_test_qw <- lht(model_q,hypothesis.matrix,rhs,white.adjust='hc3')
HK_test_qw

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
model_q2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
summary(model_q2)
sum(model_q2$coefficients) # beta
lhs <- rbind(c(1,1,1,1,1,1,1,1))
step2_test_q <- lht(model_q2,lhs,c(1))
step2_test_q
step2_test_q[2,5]  # F test  : step- test 2
step2_test_q[2,6]  # Pr(>F)  : step- test 2


################# lm formula #############################
lm_formula <- list( "`BTC` ~ . -`ETH`",   # yi = BTC
                    "`ETH` ~ . -`BTC`",   # yi = ETH
                    "`BTC` + `ETH` ~ . ") # i = 1:3

lm_formula2 <- list( "`BTC` ~ . -1 -`ETH`",   # intercept = 0
                     "`ETH` ~ . -1 -`BTC`",
                     "`BTC` + `ETH` ~ . -1 ") # i = 1:3
                  
* year <- list( "2017" , "2018" , "2019" , "2020" , "2021") # j=1:5
year <- c( "", "/2019", "2020/") 
                 

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


