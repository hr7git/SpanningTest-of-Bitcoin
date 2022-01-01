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
            'BTC-USD'  # Bitcoin USD dollar
)
getSymbols(symbols, src = 'yahoo')

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

chart_Series(Ad(GLD))
##################### Image ###################################
save.image(file="data_quant.RData") 


#################
###############################################################
###############################################################
# HK test , Step test of Bitcoin

model_q <- lm(`BTC-USD` ~ SPY + IEV + EWJ + EEM + TLT + IEF + IYR + RWX + GLD + DBC , data = rets)
summary(model_q)


##### HK test
library(car)
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
HK_test_q <- lht(model_q,lhs,c(0,1))
HK_test_q

##### step-1 test  : alpha = 0
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
step1_test_q <- lht(model_q,lhs,c(0))
step1_test_q

##### step- test 2 : 
##### unresrticted model condition on alpha =0  
model_q2 <- lm(`BTC-USD` ~ 0 + SPY + IEV + EWJ + EEM + TLT + IEF + IYR + RWX + GLD + DBC , data = rets)
summary(model_q2)


##### step- test 2 : beta=1 condition alpha = 0
lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
step2_test_q <- lht(model_q2,lhs,c(1))
step2_test_q



