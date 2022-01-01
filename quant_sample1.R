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

chart_Series(Ad(BTC-USD))
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
