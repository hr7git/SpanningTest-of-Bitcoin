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
# 1. 달러헷지 - dollar index , gold, vix
# 2. 새로운 포트폴리오 편입
##########################################################
#  Data make
##########################################################
# 
library(quantmod)    # getsymbols, xts->dataframe in lm
library(PerformanceAnalytics)   # Return.calculate() 
library(magrittr)
library(car)        # lht 
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


getSymbols(symbols, src = 'yahoo', from ='2018-01-01', to ='2022-02-10') 

# Bitcoin USD  & Etherium USD 
BTC = getSymbols('BTC-USD', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) 
ETH = getSymbols('ETH-USD', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) 
SNP = getSymbols('^GSPC', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) #S&P500 index
# TYX = getSymbols('^TYX', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) #30Y bonds
# TNX = getSymbols('^TNX', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) #10Y bonds
# QQQ = getSymbols('QQQ', src = 'yahoo', from = '2014-01-01',auto.assign=FALSE) 

save.image(file="data_getsymbols.RData") 
##############   data price ret=returns    #####################################
load("data_getsymbols.RData")
# symbols_BTC = c( symbols , 'BTC')
# symbols_ETH = c( symbols,  'ETH')

### variables setting : symbol + BTC
# assets <- c( symbols , 'BTC','ETH')

### Data procedure rets - main : BTC
    assets <- c( symbols, 'BTC')
    prices = do.call(cbind,
                 lapply(assets, function(x) Ad(get(x)))) %>%
              setNames(assets)
    rets = Return.calculate(prices) %>% na.omit()
    # save rets : xts.zoo
    save(rets, file="rets.Rdata")

### Data procedure rets2 - main : BTC ETH
    assets <- c( symbols, 'BTC', 'ETH', 'SNP')
    prices2 = do.call(cbind,
                  lapply(assets, function(x) Ad(get(x)))) %>%
              setNames(assets)
    rets2 = Return.calculate(prices2) %>% na.omit()
    # save rets : xts.zoo
    save(rets2, file="rets2.Rdata")
    
  
##### convert zoo into timeSeries
    library(timeSeries)
    library(fPortfolio)
    # library(quantmod)
    # library(dplyr)
    # library(PerformanceAnalytics)
    library(ggplot2)
    
    ### Data procedure - TEST only : BTC ETH
    #  all - without restriction
    #  before covid19 - 
    #  after covid19
    load("rets2.Rdata")
 
    rets2 <- rets2["/2019"]  # before covid19
    # rets2 <- rets2["2020/"]  # after covid19
    ####
    testset <- c('BTC', 'ETH', 'SNP', 'TLT' ,'GLD')
    rets_test <- rets2[ ,testset]
    
    benchset <- c( 'SNP', 'TLT','GLD')
    rets_bench <- rets2[ , benchset]
    
    # test data
    portfolio_rets <- rets_test %>% 
                      as.timeSeries() * 100
    eff_Frontier <- portfolioFrontier(portfolio_rets, constraints = "LongOnly")
    # bench data
    portfolio_rets <- rets_bench %>% 
                      as.timeSeries() * 100
    eff_Frontier2 <- portfolioFrontier(portfolio_rets, constraints = "LongOnly")
    #Frontier line
    longFrontier <- eff_Frontier
    tailoredFrontierPlot2(object = longFrontier,  twoAssets = TRUE, title = FALSE,
                        risk = "Cov", sharpeRatio = FALSE, xlim = c(0,6))
    # SNP-TLT Frontier
    longFrontier <- eff_Frontier2
    # twoAssetsLines(object = longFrontier,col = c("Red"))
    frontierPlot(object = longFrontier, add = TRUE, title = FALSE, 
                         risk = "Cov", xlim = c(0,6), )
    tangencyLines(object = longFrontier, col = "blue",
                 risk = "Cov", xlim = c(0,6), )
    
          
### correlation graph
library(corrplot)
corr_data <- rets2
cor(corr_data) %>%
  corrplot(method = 'color', type = 'lower',
           addCoef.col = 'black', number.cex = 0.5,
           tl.cex = 0.7, tl.srt = 0, tl.col = 'black',
           col =
             colorRampPalette(c('blue', 'white', 'red'))(200),
           mar = c(0,0,0.5,0))
###
data <- rets2
ggcorr(data, method = c("everything", "pearson")) 
#################################################################

covmat = cov(rets)
#################### Chart  ###########################
library(ggplot2)
library(dplyr)
library(hrbrthemes)

chart_Series(Ad(GLD),Ad(SPY))
chart_Series(apply.monthly( Ad(BTC),mean) )
chart_Series(rets_test$BTC)
chart_Series(apply.monthly( rets_test$BTC,mean))
chartSeries(`BTC`)
chart.TimeSeries.ggplot2(`BTC`)

load("rets2.Rdata")
###
SPY %>%
  ggplot(aes(x = Index, y = SPY.Adjusted)) +
  geom_line( color="#69b3a2")  
BTC %>%
  ggplot(aes(x = Index, y = Ad(BTC))) +
  geom_line( color="#69b3a2") 
rets2 %>%
  ggplot(aes(x = Index, y = rets2$BTC)) +
  geom_line( color="blue") 
###
library(highcharter)

highchart(type = 'stock') %>%
  hc_add_series(Ad(BTC)) %>%
  hc_scrollbar(enabled = FALSE)

###
library(plotly)
SPY %>%
  fortify.zoo %>%
  plot_ly(x= ~Index, y = ~SPY.Close ) %>%
  add_lines()

###
###
# library(PerformanceAnalytics)
# library(quantmod)
library(dygraphs)
data <- apply.monthly(rets_test,mean)  # montly return
dygraph(data, main = "Montly Return") %>%
  dyAxis("y", label = "return") %>%
  dyOptions(colors = RColorBrewer::brewer.pal(4, "Set2"))
###


##################### Image ###################################
save.image(file="data_quant.RData")
# load("data_quant.RData")



##########################################################
#  fportfolio - efficient frontier line
#  https://www.youtube.com/watch?v=5gmhZEl0kI8
#  fportfolio in r -- googling




# Not run below
##########################################################
#  Regression model
##########################################################
model  <- lm(`BTC` ~ . -`ETH`, data=rets)    # regression 
model  <- lm(BTC ~ (SPY + IEV + EWJ + EEM + TLT + IEF + IYR + RWX + GLD + DBC), data=rets)
model <- lm(BTC ~ (SPY + EEM + TLT + IEF + IYR + RWX + GLD + DBC), data=rets)
model <- lm(BTC ~ (SPY + EEM + TLT + IYR + RWX + GLD + DBC), data=rets)
model <- lm(BTC ~ (SPY + EEM + TLT + IYR + GLD + DBC), data=rets)
model <- lm(BTC ~ (SPY + EEM + TLT + IYR + GLD + DBC), data=rets["/2019"])
model <- lm(BTC ~ (SPY + EEM + TLT + IYR + GLD + DBC), data=rets["2020/"])
model <- lm(`BTC` ~ . -`ETH`, data=rets)    # regression 
model <- lm(`BTC` ~ . -`ETH`, data=rets["/2019"])    # regression 
model <- lm(`BTC` ~ . -`ETH`, data=rets["2020/"])    # regression 
model2 <- lm(`BTC` ~ . -1 -`ETH`, data=rets)    # regression 
# bench Aseets : SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC
model <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets)
model <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets["/2019"])
model <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets["2020/"])
##############################################################################
#### Regression Model
model <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets)
model2 <- lm(BTC ~ 0 + (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC), data=rets)

summary(model)
model$coefficients[1]   # alpha
sum(model$coefficients) - model$coefficients[[1]] # beta

##### HK test
# the rows of which specify linear combinations of the model coefficients
hypothesis.matrix <- rbind(c(1,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1))
rhs=c(0,1)   # right-hand-side vector for hypothesis

HK_test <- lht(model,hypothesis.matrix,rhs)
HK_test

HK_testw <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
HK_testw

HK_test[2,5]  # F test  - HK test
HK_test[2,6]  # Pr(>F)  - HK test

##### step-1 test  : alpha = 0
lhs <- c(1,0,0,0,0,0,0,0,0)
step1_test <- lht(model,lhs,c(0))
step1_test
step1_test[2,5]  # F test  - step-1 test
step1_test[2,6]  # Pr(>F)  : step-1 test

##### step- test 2 : beta=1 condition on alpha = 0
##### unresrticted model condition on alpha =0  : model2
# model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
summary(model2)
sum(model2$coefficients) # beta
lhs <- rbind(c(1,1,1,1,1,1,1,1))
step2_test <- lht(model2,lhs,c(1))
step2_test
step2_test[2,5]  # F test  : step- test 2
step2_test[2,6]  # Pr(>F)  : step- test 2

#
model$coefficients[1]   # alpha
sum(model$coefficients) - model$coefficients[[1]] # beta
HK_test[2,5]  # F test  - HK test
HK_test[2,6]  # Pr(>F)  - HK test
step1_test[2,5]  # F test  - step-1 test
step1_test[2,6]  # Pr(>F)  : step-1 test
step2_test[2,5]  # F test  : step- test 2
step2_test[2,6]  # Pr(>F)  : step- test 2

##############################################################################

# library(dynlm)
# dynlm(BTC ~ (SPY + TLT + GLD), data=rets)
#### Regression Model - QQQ GLD exclusive
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets)
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets)
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets["/2019"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets["/2019"])
model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets["2020/"])
model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets["2020/"])


summary(model)
model$coefficients[1]   # alpha
sum(model$coefficients) - model$coefficients[[1]]  # beta

##### HK test
# the rows of which specify linear combinations of the model coefficients
hypothesis.matrix <- rbind(c(1,0,0,0),c(0,1,1,1))
rhs=c(0,1)   # right-hand-side vector for hypothesis

HK_test <- lht(model,hypothesis.matrix,rhs)
# HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
HK_test

HK_test[2,5]  # F test  - HK test
HK_test[2,6]  # Pr(>F)  - HK test

##### step-1 test  : alpha = 0
lhs <- c(1,0,0,0)
step1_test <- lht(model,lhs,c(0))
# step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
step1_test
step1_test[2,5]  # F test  - step-1 test
step1_test[2,6]  # Pr(>F)  : step-1 test

##### step- test 2 : beta=1 condition on alpha = 0
##### unresrticted model condition on alpha =0  : model2
# model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
summary(model2)
sum(model2$coefficients) # beta
lhs <- rbind(c(1,1,1))
step2_test <- lht(model2,lhs,c(1))
# step2_test <- lht(model2,lhs,c(1),white.adjust='hc3')
step2_test
step2_test[2,5]  # F test  : step- test 2
step2_test[2,6]  # Pr(>F)  : step- test 2

#
alpha  = model$coefficients[[1]]   # alpha
beta   = sum(model$coefficients) - model$coefficients[[1]]  # beta
HK_F   = HK_test$F[2]            # F test  - HK test
HK_Pr  = HK_test$`Pr(>F)`[2]     # Pr(>F)  - HK test HK_test$`Pr(>F)`
st1_F  = step1_test$F[2]         # F test  - step-1 test
st1_Pr = step1_test$`Pr(>F)`[2]  # Pr(>F)  : step-1 test
st2_F  = step2_test$F[2]         # F test  : step- test 2
st2_Pr = step2_test$`Pr(>F)`[2]  # Pr(>F)  : step- test 2
lm_model = as.character(model$call)[2]
lm_data  = as.character(model$call)[3]

df <- data.frame(
      alpha,
      beta,
      HK_F,
      HK_Pr,
      st1_F,
      st1_Pr,
      st2_F,
      st2_Pr,
      lm_model,
      lm_data )

# df <- df %>% add_row(
#       alpha,
#       beta,
#       HK_F,
#       HK_Pr,
#       st1_F,
#       st1_Pr,
#       st2_F,
#       st2_Pr,
#       lm_model,
#       lm_data )


###################### Bench mark set 1 ############################
# Test : BTC + ETH
# Bench : set compare
# data : after covid19
# result - aftger add IEF : bench can span test(BTC+ETH)
############################ bench mark 
load("rets2.Rdata")
model  <- lm(BTC+ETH~(SPY+TLT+GLD), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0),c(0,1,1,1))
df01 <- regress_bench(model, hypothesis.matrix)  # initialize

model  <- lm(BTC+ETH~(SPY+TLT+GLD+QQQ), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0,0),c(0,1,1,1,1))
df02 <- regress_bench(model, hypothesis.matrix); df01 <- rbind(df01,df02)  # add-rbind
#
model  <- lm(BTC+ETH~(SPY+TLT+GLD+EWJ), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0,0),c(0,1,1,1,1))
df02 <- regress_bench(model, hypothesis.matrix); df01 <- rbind(df01,df02)  # add-rbind
#
model  <- lm(BTC+ETH~(SPY+TLT+GLD+EEM), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0,0),c(0,1,1,1,1))
df02 <- regress_bench(model, hypothesis.matrix); df01 <- rbind(df01,df02)  # add-rbind
#
model  <- lm(BTC+ETH~(SPY+TLT+GLD+IEF), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0,0),c(0,1,1,1,1))
df02 <- regress_bench(model, hypothesis.matrix); df01 <- rbind(df01,df02)  # add-rbind
#
model  <- lm(BTC+ETH~(SPY+TLT+GLD+IYR), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0,0),c(0,1,1,1,1))
df02 <- regress_bench(model, hypothesis.matrix); df01 <- rbind(df01,df02)  # add-rbind
#
model  <- lm(BTC+ETH~(SPY+TLT+GLD+RWX), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0,0),c(0,1,1,1,1))
df02 <- regress_bench(model, hypothesis.matrix); df01 <- rbind(df01,df02)  # add-rbind
#
model  <- lm(BTC+ETH~(SPY+TLT+GLD+DBC), data=rets2["2020/"])
hypothesis.matrix <- rbind(c(1,0,0,0,0),c(0,1,1,1,1))
df02 <- regress_bench(model, hypothesis.matrix); df01 <- rbind(df01,df02)  # add-rbind
#

df_bench <- df01
save(df_bench, file="df_bench.Rdata")
######

  regress_bench <- function(model, hypothesis.matrix) {
    ##### HK test
    # the rows :linear combinations of the model coefficients
    # hypothesis.matrix <- rbind(c(1,0,0,0),c(0,1,1,1))
    rhs=c(0,1)   # right-hand-side vector for hypothesis
    HK_test <- lht(model,hypothesis.matrix,rhs)
    # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
  
    #
    alpha  = model$coefficients[[1]]   # alpha
    beta   = sum(model$coefficients) - model$coefficients[[1]]  # beta
    HK_F   = HK_test$F[2]            # F test  - HK test
    HK_Pr  = HK_test$`Pr(>F)`[2]     # Pr(>F)  - HK test HK_test$`Pr(>F)`
  
    lm_model = as.character(model$call)[2]
    lm_data  = as.character(model$call)[3]
    
    df <- data.frame(alpha, beta,
                     HK_F, HK_Pr,
                     lm_model, lm_data )
    
    return(df)
  }

###################### Bench mark set 2 ############################
# wald test : Omitted variable Bias
# Frome above IEF adding to bench test
library(lmtest)
load("rets2.Rdata")
model  <- lm(BTC+ETH~(SPY+TLT+GLD), data=rets2["2020/"])
model_2  <- lm(BTC+ETH~(SPY+TLT+GLD+IEF), data=rets2["2020/"])
waldtest(model, model_2, test = "F")
# result : Pr(>F) = 0.858 is insignificant
# how to interpret ?
mean(rets2$SPY)
var(rets2$SPY, rets2$TLT)
###################### BASE ####################################
############################ BTC / Data:2014.1 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets)
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets)
# df01 <- wald_test1(model, model2)  # initialize
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets["/2019"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets["/2019"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets["2020/"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets["2020/"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ BTC / Data:2017.11
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ ETH / Data:2017.11
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ BTC + ETH / Data:2017.11
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

###################### BASE 2 period order ############################
load("rets2.Rdata")
############################ all period
model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2)
model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2)
df01 <- wald_test1(model, model2)  # initialize

model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2)
model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2)
model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

############################ Befroe covid19
model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["/2019"])
model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

############################ After covid19
model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["2020/"])
model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
#######################
df_base2 <- df01
save(df_base2, file="df_base2.Rdata")
######
wald_test1 <- function(model, model2) {
  ##### HK test
    # the rows :linear combinations of the model coefficients
    hypothesis.matrix <- rbind(c(1,0,0,0),c(0,1,1,1))
    rhs=c(0,1)   # right-hand-side vector for hypothesis
    HK_test <- lht(model,hypothesis.matrix,rhs)
    # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
  
  ##### step-1 test  : alpha = 0
    lhs <- c(1,0,0,0)
    step1_test <- lht(model,lhs,c(0))
    # step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
  
  ##### step- test 2 : beta=1 condition on alpha = 0
    ##### unresrticted model condition on alpha =0  : model2
    # model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
    lhs <- rbind(c(1,1,1))
    step2_test <- lht(model2,lhs,c(1))
    # step2_test <- lht(model2,lhs,c(1),white.adjust='hc3')
    #
    alpha  = model$coefficients[[1]]   # alpha
    beta   = sum(model$coefficients) - model$coefficients[[1]]  # beta
    HK_F   = HK_test$F[2]            # F test  - HK test
    HK_Pr  = HK_test$`Pr(>F)`[2]     # Pr(>F)  - HK test HK_test$`Pr(>F)`
    st1_F  = step1_test$F[2]         # F test  - step-1 test
    st1_Pr = step1_test$`Pr(>F)`[2]  # Pr(>F)  : step-1 test
    st2_F  = step2_test$F[2]         # F test  : step- test 2
    st2_Pr = step2_test$`Pr(>F)`[2]  # Pr(>F)  : step- test 2
    lm_model = as.character(model$call)[2]
    lm_data  = as.character(model$call)[3]
  
    df <- data.frame(alpha, beta,
                     HK_F, HK_Pr,
                     st1_F, st1_Pr,
                     st2_F, st2_Pr,
                     lm_model, lm_data )
    
    return(df)
}

##################### heteroskedasticity ##############################
load("rets2.Rdata")
############################ all period
model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2)
model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2)
df01 <- wald_test2(model, model2)  # initialize

model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2)
model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2)
model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

############################ Befroe covid19
model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["/2019"])
model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

############################ After covid19
model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["2020/"])
model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
#######################
df_hetero <- df01
save(df_hetero, file="df_hetero.Rdata")
############################ BTC / Data:2014.1 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets)
# df01 <- wald_test2(model, model2)  # initialize
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ ETH / Data:2017.11
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ BTC + ETH / Data:2017.11
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind


######
wald_test2 <- function(model, model2) {
  ##### HK test
  # the rows :linear combinations of the model coefficients
  hypothesis.matrix <- rbind(c(1,0,0,0),c(0,1,1,1))
  rhs=c(0,1)   # right-hand-side vector for hypothesis
  HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
  # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
  
  ##### step-1 test  : alpha = 0
  lhs <- c(1,0,0,0)
  step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
  # step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
  
  ##### step- test 2 : beta=1 condition on alpha = 0
  ##### unresrticted model condition on alpha =0  : model2
  # model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
  lhs <- rbind(c(1,1,1))
  step2_test <- lht(model2,lhs,c(1),white.adjust='hc3')
  # step2_test <- lht(model2,lhs,c(1),white.adjust='hc3')
  #
  alpha  = model$coefficients[[1]]   # alpha
  beta   = sum(model$coefficients) - model$coefficients[[1]]  # beta
  HK_F   = HK_test$F[2]            # F test  - HK test
  HK_Pr  = HK_test$`Pr(>F)`[2]     # Pr(>F)  - HK test HK_test$`Pr(>F)`
  st1_F  = step1_test$F[2]         # F test  - step-1 test
  st1_Pr = step1_test$`Pr(>F)`[2]  # Pr(>F)  : step-1 test
  st2_F  = step2_test$F[2]         # F test  : step- test 2
  st2_Pr = step2_test$`Pr(>F)`[2]  # Pr(>F)  : step- test 2
  lm_model = as.character(model$call)[2]
  lm_data  = as.character(model$call)[3]
  
  df <- data.frame(alpha, beta,
                   HK_F, HK_Pr,
                   st1_F, st1_Pr,
                   st2_F, st2_Pr,
                   lm_model, lm_data )
  
  return(df)
}


# ##################### benchmark set ##############################
# ### correlation graph
# library(corrplot)
# 
# cor(rets) %>%
#   corrplot(method = 'color', type = 'lower',
#            addCoef.col = 'black', number.cex = 0.7,
#            tl.cex = 1, tl.srt = 0, tl.col = 'black',
#            col =
#              colorRampPalette(c('blue', 'white', 'red'))(200),
#            mar = c(0,0,0.5,0))
# 
# ############################ BTC / Data:2014.1 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets)
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets)
# df01 <- wald_test2(model, model2)  # initialize
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets["/2019"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets["/2019"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets["2020/"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets["2020/"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ BTC / Data:2017.11
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ ETH / Data:2017.11
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# ############################ BTC + ETH / Data:2017.11
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# df_hetero <- df01
# save(df_hetero, file="df_hetero.Rdata")
# ######
# wald_test2 <- function(model, model2) {
#   ##### HK test
#   # the rows :linear combinations of the model coefficients
#   hypothesis.matrix <- rbind(c(1,0,0,0),c(0,1,1,1))
#   rhs=c(0,1)   # right-hand-side vector for hypothesis
#   HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
#   # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
#   
#   ##### step-1 test  : alpha = 0
#   lhs <- c(1,0,0,0)
#   step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
#   # step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
#   
#   ##### step- test 2 : beta=1 condition on alpha = 0
#   ##### unresrticted model condition on alpha =0  : model2
#   # model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
#   lhs <- rbind(c(1,1,1))
#   step2_test <- lht(model2,lhs,c(1),white.adjust='hc3')
#   # step2_test <- lht(model2,lhs,c(1),white.adjust='hc3')
#   #
#   alpha  = model$coefficients[[1]]   # alpha
#   beta   = sum(model$coefficients) - model$coefficients[[1]]  # beta
#   HK_F   = HK_test$F[2]            # F test  - HK test
#   HK_Pr  = HK_test$`Pr(>F)`[2]     # Pr(>F)  - HK test HK_test$`Pr(>F)`
#   st1_F  = step1_test$F[2]         # F test  - step-1 test
#   st1_Pr = step1_test$`Pr(>F)`[2]  # Pr(>F)  : step-1 test
#   st2_F  = step2_test$F[2]         # F test  : step- test 2
#   st2_Pr = step2_test$`Pr(>F)`[2]  # Pr(>F)  : step- test 2
#   lm_model = as.character(model$call)[2]
#   lm_data  = as.character(model$call)[3]
#   
#   df <- data.frame(alpha, beta,
#                    HK_F, HK_Pr,
#                    st1_F, st1_Pr,
#                    st2_F, st2_Pr,
#                    lm_model, lm_data )
#   
#   return(df)
# }
########################################################################
### function : modify tailoredFrontierPlot --> ...2
tailoredFrontierPlot2 <-
  function (object, return = c("mean", "mu"), risk = c("Cov", 
                                                       "Sigma", "CVaR", "VaR"), mText = NULL, 
            col = NULL, xlim = NULL, ylim = NULL, twoAssets = FALSE, 
            sharpeRatio = TRUE, title = TRUE, ...) 
  {
    offset <- 0.1
    risk <- match.arg(risk)
    return <- match.arg(return)
    if (is.null(xlim)) {
      if (risk == "Cov") {
        xmax <- max(sqrt(diag(getCov(object))))
      }
      if (risk == "Sigma") {
        xmax <- max(sqrt(diag(getSigma(object))))
      }
      if (risk == "CVaR") {
        alpha <- getAlpha(object)
        quantiles <- colQuantiles(getSeries(object), prob = alpha)
        n.max <- which.max(-quantiles)
        r <- getSeries(object)[, n.max]
        r <- r[r < quantiles[n.max]]
        xmax <- -mean(r)
      }
      if (risk == "VaR") {
        xmax <- max(-colQuantiles(getSeries(object), prob = alpha))
      }
      xlim <- c(0, xmax)
      Xlim <- c(xlim[1] - diff(xlim) * offset, xlim[2] + diff(xlim) * 
                  offset)
    }
    else {
      Xlim <- xlim
    }
    if (is.null(ylim)) {
      if (return == "mean") {
        ylim <- range(getMean(object))
      }
      else {
        ylim <- range(getMu(object))
      }
      Ylim <- c(ylim[1] - diff(ylim) * offset, ylim[2] + diff(ylim) * 
                  offset)
    }
    else {
      Ylim = ylim
    }
    frontierPlot(object, labels = FALSE, return = return, risk = risk, 
                 auto = FALSE, xlim = Xlim, ylim = Ylim, title = title, 
                 pch = 19, ...)
    grid()
    abline(h = 0, col = "grey")
    abline(v = 0, col = "grey")
    data <- getData(object)
    spec <- getSpec(object)
    constraints <- getConstraints(object)
    mvPortfolio <- minvariancePortfolio(data, spec, constraints)
    # minvariancePoints(object, return = return, risk = risk, auto = FALSE, 
    #                   pch = 19, col = "red")
    # tangencyPoints(object, return = return, risk = risk, auto = FALSE, 
    #                pch = 19, col = "blue")
    tangencyLines(object, return = return, risk = risk, auto = FALSE, 
                  col = "red")
    # xy <- equalWeightsPoints(object, return = return, risk = risk, 
    #                          auto = FALSE, pch = 15, col = "grey")
    # text(xy[, 1] + diff(xlim)/20, xy[, 2] + diff(ylim)/20, "Efficient-Frontier Line", 
    #      font = 3, cex = 0.7)
    if (is.null(col)) 
      # col = rainbow(6)
       col = "black"
    xy <- singleAssetPoints(object, return = return, risk = risk, 
                            auto = FALSE, cex = 1.5, col = col, lwd = 2)
    text(xy[, 1] + diff(xlim)/20, xy[, 2] + diff(ylim)/20, rownames(xy), 
         font = 2, cex = 1)  # cex : 0.7 -> 1.0
    if (twoAssets) {
      twoAssetsLines(object, return = return, risk = risk, 
                     auto = FALSE, lty = 3, col = "grey")
    }
    # if (sharpeRatio) {
    #   sharpeRatioLines(object, return = return, risk = risk, 
    #                    auto = FALSE, col = "orange", lwd = 2)
    # }
    mtext("", adj = 0, side = 4, cex = 0.7, col = "darkgrey")
    invisible(list(object = object, xlim = Xlim, ylim = Ylim))
  }


