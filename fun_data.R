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
# https://www.rmetrics.org/
# https://bookdown.org/compfinezbook/introcompfinr/
#
# 1. 달러헷지 - dollar index , gold, vix
# 2. 새로운 포트폴리오 편입
#########################################################
# 
library(quantmod)    # getsymbols, xts->dataframe in lm
library(PerformanceAnalytics)   # Return.calculate() 
library(magrittr)
library(car)        # lht 
library(tidyr)
library(dplyr)
library(timeSeries)
library(fPortfolio)
library(ggplot2)
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
            'DBC')  # Commodity

getSymbols(symbols, src = 'yahoo', from ='2018-01-01', to ='2022-02-10') 

# Bitcoin USD  & Etherium USD 
BTC = getSymbols('BTC-USD', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) 
ETH = getSymbols('ETH-USD', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) 
SNP = getSymbols('^GSPC', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) #S&P500 index
SHY = getSymbols('SHY', src = 'yahoo', from ='2018-01-01', to ='2022-02-10',auto.assign=FALSE) 
save.image(file="data_getsymbols.RData") 
##############   data price ret=returns    #####################################
load("data_getsymbols.RData")
### Data procedure rets - main : BTC
    assets <- c( symbols, 'BTC')
    prices = do.call(cbind,
                 lapply(assets, function(x) Ad(get(x)))) %>%
              setNames(assets)
    rets = Return.calculate(prices) %>% na.omit()
    # save rets : xts.zoo
    save(rets, file="rets.Rdata")

### Data procedure rets2 - main : BTC ETH
    assets <- c( symbols, 'BTC', 'ETH', 'SNP', 'SHY')
    prices2 = do.call(cbind,
                  lapply(assets, function(x) Ad(get(x)))) %>%
              setNames(assets)
    rets2 = Return.calculate(prices2) %>% na.omit()
    # save rets : xts.zoo
    save(rets2, file="rets2.Rdata")
    
  
##### convert zoo into timeSeries
    load("rets2.Rdata")
    
    # eff_data <- rets2              # all period
    # eff_data <- rets2["/2019"]  # before covid19
    # eff_data <- rets2["2020/"]      # after covid19
    ####
    #   riskfree_Rate <- 0  # initialize
    #   riskfree_Rate
    # # riskfree_Rate is 0.01244129
    # setRiskFreeRate <- riskfree_Rate
    
    source("./function/tailoredFrontierPlot2.R")
    source("./function/frontierPlot2.R")
    source('./function/eff_line_plot.R')
    # Entire period
    benchset <- c( 'SNP', 'TLT')
    testset <- c('SNP', 'TLT', 'BTC')
    eff_result0    <- eff_line_plot(rets2, testset, benchset)
    testset <- c('SNP', 'TLT', 'ETH')    
    eff_result    <- eff_line_plot(rets2, testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result[2,])
    testset <- c('SNP', 'TLT', 'BTC','ETH')
    eff_result    <- eff_line_plot(rets2, testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result[2,])
    # First period : Before covid19
    benchset <- c( 'SNP', 'TLT')
    testset <- c('SNP', 'TLT', 'BTC')
    eff_result <- eff_line_plot(rets2["/2019"], testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result)
    testset <- c('SNP', 'TLT', 'ETH')    
    eff_result <- eff_line_plot(rets2["/2019"], testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result[2,])
    testset <- c('SNP', 'TLT', 'BTC','ETH')
    eff_result <- eff_line_plot(rets2["/2019"], testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result[2,])
    # First period : After covid19
    benchset <- c( 'SNP', 'TLT')
    testset <- c('SNP', 'TLT', 'BTC')
    eff_result  <- eff_line_plot(rets2["2020/"], testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result)
    testset <- c('SNP', 'TLT', 'ETH')    
    eff_result  <- eff_line_plot(rets2["2020/"], testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result[2,])
    testset <- c('SNP', 'TLT', 'BTC','ETH')
    eff_result  <- eff_line_plot(rets2["2020/"], testset, benchset)
    eff_result0 <- rbind(eff_result0, eff_result[2,])
    ###
    print(eff_result0)
    kable(eff_result0)
    write.csv(eff_result0, file = 'eff_result0.csv')
    ##
    ########### item ##########################
    # testset <- c('SNP', 'TLT', 'BTC')
    # benchset <- c( 'SNP', 'TLT')
    # eff_result_all    <- eff_line_plot(rets2, testset, benchset)
    # eff_result_before <- eff_line_plot(rets2["/2019"], testset, benchset)
    # eff_result_after  <- eff_line_plot(rets2["2020/"], testset, benchset)
    # 
    # testset <- c('SNP', 'TLT', 'ETH')
    # benchset <- c( 'SNP', 'TLT')
    # eff_result_all    <- eff_line_plot(rets2, testset, benchset)
    # eff_result_before <- eff_line_plot(rets2["/2019"], testset, benchset)
    # eff_result_after  <- eff_line_plot(rets2["2020/"], testset, benchset)
    # 
    # testset <- c('SNP', 'TLT', 'BTC','ETH')
    # benchset <- c( 'SNP', 'TLT')
    # eff_result_all    <- eff_line_plot(rets2, testset, benchset)
    # eff_result_before <- eff_line_plot(rets2["/2019"], testset, benchset)
    # eff_result_after  <- eff_line_plot(rets2["2020/"], testset, benchset)   
    # 
    # testset <- c('SNP', 'TLT', 'QQQ', 'BTC')
    # benchset <- c( 'SNP', 'TLT')
    # eff_result_all    <- eff_line_plot(rets2, testset, benchset)
    # eff_result_before <- eff_line_plot(rets2["/2019"], testset, benchset)
    # eff_result_after  <- eff_line_plot(rets2["2020/"], testset, benchset) 
    ### Data procedure - TEST only : BTC ETH
    #  all - without restriction
    #  before covid19 - 
    #  after covid19
    # load("rets2.Rdata")
    # 
    # eff_data <- rets2              # all period
    # eff_data <- rets2["/2019"]  # before covid19
    # eff_data <- rets2["2020/"]      # after covid19
    # ####
    # #
    # eff_line_plot(eff_data)
    # #
    # ##################### function : eff_line_plot
    # eff_line_plot <- function(data) {
    #   testset <- c('SNP', 'TLT', 'BTC','ETH')
    #       rets_test <- data[ ,testset]
    #   benchset <- c( 'SNP', 'TLT')
    #       rets_bench <- data[ , benchset]
    #       
    #       shortSpec <- portfolioSpec()
    #       setNFrontierPoints(shortSpec) <- 50
    #       setSolver(shortSpec) <- "solveRshortExact"    
    #      
    #   
    #   # test data
    #   portfolio_rets <- rets_test %>% as.timeSeries() * 100
    #       eff_Frontier <- portfolioFrontier(portfolio_rets, spec = shortSpec,
    #                                         constraints = "Short")  # test-set
    #   # bench data
    #   portfolio_rets <- rets_bench %>% as.timeSeries() * 100
    #       eff_Frontier2 <- portfolioFrontier(portfolio_rets, spec = shortSpec,
    #                                          constraints = "Short")  # bench-set
    #   #Frontier line of test + bench
    #   longFrontier <- eff_Frontier
    #       tailoredFrontierPlot2(object = longFrontier,  twoAssets = TRUE, title = FALSE, 
    #                           risk = "Cov", sharpeRatio = FALSE, xlim = c(0,6))
    #       t_line <- tangencyLines(object = longFrontier, col = "red",
    #                           risk = "Cov", xlim = c(0,6), )
    #       t_mvpoint <- minvariancePoints(object = longFrontier, return = "mean",
    #                           risk = "Cov", auto = TRUE, )
    #   # Frontier of benchmark
    #   longFrontier <- eff_Frontier2
    #       frontierPlot2(object = longFrontier, add = TRUE, title = TRUE, 
    #                         col = c("blue","blue"), risk = "Cov", xlim = c(0,6), )
    #       b_line <- tangencyLines(object = longFrontier, col = "blue",
    #                         risk = "Cov", xlim = c(0,6), )
    #       b_mvpoint <- minvariancePoints(object = longFrontier, return = "mean", 
    #                                  risk = "Cov", auto = TRUE, )
    #   # slop slop_x slop_y GMV_x GMV_Y
    # 
    #     eff1_result <- c(slop = t_line$slope, t_line$assets, # tangency slop
    #                   GMV_X = t_mvpoint[1], # GMV x
    #                   GMV_Y = t_mvpoint[2], # GMV y
    #                   f_data = paste(eff_Frontier@data@data$names, collapse = ","))
    #     
    #     eff2_result <-c(slop =  b_line$slope, b_line$assets,
    #                   GMV_X =  b_mvpoint[1],
    #                   GMV_Y =  b_mvpoint[2],
    #                   f_data = paste(eff_Frontier2@data@data$names, collapse = ","))
    #     eff_result <- rbind(eff1_result, eff2_result)
    #     print(eff_result)
    #     
    #     shortFrontier <- eff_Frontier
    #         weightsPlot(shortFrontier)
    #         text <- "MV Portfolio - Short Constrained Portfolio"
    #         mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
    #         weightedReturnsPlot(shortFrontier)
    #         covRiskBudgetsPlot(shortFrontier)
    #     
    #     shortFrontier <- eff_Frontier2
    #         weightsPlot(shortFrontier)
    #         text <- "MV Portfolio - Short Constrained Portfolio"
    #         mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
    #         weightedReturnsPlot(shortFrontier)
    #         covRiskBudgetsPlot(shortFrontier)
    # }
    ################# end function : eff_line_plot
    
################### constrints = short 
      shortSpec <- portfolioSpec()
      setNFrontierPoints(shortSpec) <- 50
      setSolver(shortSpec) <- "solveRshortExact"
      shortFrontier <- portfolioFrontier(
        data = portfolio_rets,
        spec = shortSpec,
        constraints = "Short")
      print(shortFrontier)
      short_data <- shortFrontier@portfolio@portfolio
      ###
      setNFrontierPoints(shortSpec) <- 50
      shortFrontier <- portfolioFrontier(data = portfolio_rets, spec = shortSpec,
                                       constraints = "Short")
      tailoredFrontierPlot(object = shortFrontier, mText = "MV Portfolio - Short Constraints",
                           twoAssets = TRUE, risk = "Cov")
      weightsPlot(shortFrontier)
      text <- "MV Portfolio - Short Constrained Portfolio"
      mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
      weightedReturnsPlot(shortFrontier)
      covRiskBudgetsPlot(shortFrontier)
      
      
      
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
data <- rets_test

covmat = cbind(colMeans(data)*100, cov(data) )
write.csv(covmat, file="covmat.csv")
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
# Not run below
##########################################################

##############################################################################

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
# library(lmtest)
# load("rets2.Rdata")
# model  <- lm(BTC+ETH~(SPY+TLT+GLD), data=rets2["2020/"])
# model_2  <- lm(BTC+ETH~(SPY+TLT+GLD+IEF), data=rets2["2020/"])
# waldtest(model, model_2, test = "F")
# result : Pr(>F) = 0.858 is insignificant
# how to interpret ?
# mean(rets2$SPY)
# var(rets2$SPY, rets2$TLT)
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

# ###################### BASE 2 period order ############################
# load("rets2.Rdata")
# ############################ all period
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2)
# df01 <- wald_test1(model, model2)  # initialize
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2)
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2)
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# ############################ Befroe covid19
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["/2019"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["/2019"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# ############################ After covid19
# model  <- lm(BTC ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# 
# model  <- lm(BTC + ETH ~ (SPY + TLT + GLD), data=rets2["2020/"])
# model2 <- lm(BTC + ETH ~ 0 + (SPY + TLT + GLD), data=rets2["2020/"])
# df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
# #######################
# df_base2 <- df01
# df_base2.m <- purrr::modify_if(df_base2, ~is.numeric(.), ~round(., 6))
# print(df_base2)
# save(df_base2, file="df_base2.Rdata")
# ######
# wald_test1 <- function(model, model2) {
#   ##### HK test
#     # the rows :linear combinations of the model coefficients
#     hypothesis.matrix <- rbind(c(1,0,0,0),c(0,1,1,1))
#     rhs=c(0,1)   # right-hand-side vector for hypothesis
#     HK_test <- lht(model,hypothesis.matrix,rhs)
#     # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
#   
#   ##### step-1 test  : alpha = 0
#     lhs <- c(1,0,0,0)
#     step1_test <- lht(model,lhs,c(0))
#     # step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
#   
#   ##### step- test 2 : beta=1 condition on alpha = 0
#     ##### unresrticted model condition on alpha =0  : model2
#     # model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
#     lhs <- rbind(c(1,1,1))
#     step2_test <- lht(model2,lhs,c(1))
#     # step2_test <- lht(model2,lhs,c(1),white.adjust='hc3')
#     #
#     alpha  = model$coefficients[[1]]   # alpha
#     beta   = sum(model$coefficients) - model$coefficients[[1]]  # beta
#     HK_F   = HK_test$F[2]            # F test  - HK test
#     HK_Pr  = HK_test$`Pr(>F)`[2]     # Pr(>F)  - HK test HK_test$`Pr(>F)`
#     st1_F  = step1_test$F[2]         # F test  - step-1 test
#     st1_Pr = step1_test$`Pr(>F)`[2]  # Pr(>F)  : step-1 test
#     st2_F  = step2_test$F[2]         # F test  : step- test 2
#     st2_Pr = step2_test$`Pr(>F)`[2]  # Pr(>F)  : step- test 2
#     lm_model = as.character(model$call)[2]
#     lm_data  = as.character(model$call)[3]
#   
#     df <- data.frame(alpha, beta,
#                      HK_F, HK_Pr,
#                      st1_F, st1_Pr,
#                      st2_F, st2_Pr,
#                      lm_model, lm_data )
#     
#     return(df)
# }
###################### BASE 3 period order : SNP + TLT only 2 items ###
load("rets2.Rdata")
############################ all period
model  <- lm(BTC ~ (SNP + TLT), data=rets2)
model2 <- lm(BTC ~ 0 + (SNP + TLT), data=rets2)
df01 <- wald_test1(model, model2)  # initialize

model  <- lm(ETH ~ (SNP + TLT), data=rets2)
model2 <- lm(ETH ~ 0 + (SNP + TLT), data=rets2)
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SNP + TLT), data=rets2)
model2 <- lm(BTC + ETH ~ 0 + (SNP + TLT), data=rets2)
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
############################ Befroe covid19
model  <- lm(BTC ~ (SNP + TLT), data=rets2["/2019"])
model2 <- lm(BTC ~ 0 + (SNP + TLT), data=rets2["/2019"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SNP + TLT), data=rets2["/2019"])
model2 <- lm(ETH ~ 0 + (SNP + TLT), data=rets2["/2019"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SNP + TLT), data=rets2["/2019"])
model2 <- lm(BTC + ETH ~ 0 + (SNP + TLT), data=rets2["/2019"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
############################ After covid19
model  <- lm(BTC ~ (SNP + TLT), data=rets2["2020/"])
model2 <- lm(BTC ~ 0 + (SNP + TLT), data=rets2["2020/"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SNP + TLT), data=rets2["2020/"])
model2 <- lm(ETH ~ 0 + (SNP + TLT), data=rets2["2020/"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SNP + TLT), data=rets2["2020/"])
model2 <- lm(BTC + ETH ~ 0 + (SNP + TLT), data=rets2["2020/"])
df02 <- wald_test1(model, model2); df01 <- rbind(df01,df02)  # add-rbind
#######################
df_base2 <- df01
print(df_base2)
# purrr::modify_if(df_base2, ~is.numeric(.), ~round(., 6))
# print(df_base2)

write.csv(df_base2, file = "df_base2.csv")
save(df_base2, file="df_base2.Rdata")
######
# wald_test1 <- function(model, model2) {
#   ##### HK test
#   # the rows :linear combinations of the model coefficients
#   # model  <- lm(BTC ~ (SNP + TLT), data=rets2)
#   # model2 <- lm(BTC ~ 0 + (SNP + TLT), data=rets2)
#   df01 <- wald_test1(model, model2) 
#   # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
#   
#   ##### step-1 test  : alpha = 0
#   lhs <- c(1,0,0)
#   step1_test <- lht(model,lhs,c(0))
#   # step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
#   
#   ##### step- test 2 : beta=1 condition on alpha = 0
#   ##### unresrticted model condition on alpha =0  : model2
#   # model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
#   lhs <- rbind(c(1,1))
#   step2_test <- lht(model2,lhs,c(1))
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

##################### heteroskedasticity ##############################
load("rets2.Rdata")
############################ all period
model  <- lm(BTC ~ (SNP + TLT), data=rets2)
model2 <- lm(BTC ~ 0 + (SNP + TLT), data=rets2)
df01 <- wald_test2(model, model2)  # initialize

model  <- lm(ETH ~ (SNP + TLT), data=rets2)
model2 <- lm(ETH ~ 0 + (SNP + TLT), data=rets2)
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SNP + TLT), data=rets2)
model2 <- lm(BTC + ETH ~ 0 + (SNP + TLT), data=rets2)
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
############################ Befroe covid19
model  <- lm(BTC ~ (SNP + TLT), data=rets2["/2019"])
model2 <- lm(BTC ~ 0 + (SNP + TLT), data=rets2["/2019"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SNP + TLT), data=rets2["/2019"])
model2 <- lm(ETH ~ 0 + (SNP + TLT), data=rets2["/2019"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SNP + TLT), data=rets2["/2019"])
model2 <- lm(BTC + ETH ~ 0 + (SNP + TLT), data=rets2["/2019"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
############################ After covid19
model  <- lm(BTC ~ (SNP + TLT), data=rets2["2020/"])
model2 <- lm(BTC ~ 0 + (SNP + TLT), data=rets2["2020/"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(ETH ~ (SNP + TLT), data=rets2["2020/"])
model2 <- lm(ETH ~ 0 + (SNP + TLT), data=rets2["2020/"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind

model  <- lm(BTC + ETH ~ (SNP + TLT), data=rets2["2020/"])
model2 <- lm(BTC + ETH ~ 0 + (SNP + TLT), data=rets2["2020/"])
df02 <- wald_test2(model, model2); df01 <- rbind(df01,df02)  # add-rbind
#######################
df_hetero <- df01
df_hetero
write.csv(df_hetero, file = "df_hetero.csv")
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
######
wald_test1 <- function(model, model2) {
  ##### HK test
  # the rows :linear combinations of the model coefficients
  hypothesis.matrix <- rbind(c(1,0,0),c(0,1,1))
  rhs=c(0,1)   # right-hand-side vector for hypothesis
  HK_test <- lht(model,hypothesis.matrix,rhs)
  # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
  
  ##### step-1 test  : alpha = 0
  lhs <- c(1,0,0)
  step1_test <- lht(model,lhs,c(0))
  # step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
  
  ##### step- test 2 : beta=1 condition on alpha = 0
  ##### unresrticted model condition on alpha =0  : model2
  # model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
  lhs <- rbind(c(1,1))
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
######
wald_test2 <- function(model, model2) {
  ##### HK test
  # the rows :linear combinations of the model coefficients
  hypothesis.matrix <- rbind(c(1,0,0),c(0,1,1))
  rhs=c(0,1)   # right-hand-side vector for hypothesis
  HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
  # HK_test <- lht(model,hypothesis.matrix,rhs,white.adjust='hc3')
  
  ##### step-1 test  : alpha = 0
  lhs <- c(1,0,0)
  step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
  # step1_test <- lht(model,lhs,c(0),white.adjust='hc3')
  
  ##### step- test 2 : beta=1 condition on alpha = 0
  ##### unresrticted model condition on alpha =0  : model2
  # model2 <- lm(BTC ~ (SPY + QQQ + EEM + TLT + IEF + IYR + GLD + DBC) -1, data=rets)
  lhs <- rbind(c(1,1))
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
    # frontierplot  --> frontierplot2  : : modified function
    frontierPlot2(object, labels = FALSE, return = return, risk = risk, 
                 auto = FALSE, xlim = Xlim, ylim = Ylim, title = title, 
                 col = c("red", "red"),  # change color of frontier
                 pch = 19, ...)
    grid()
    abline(h = 0, col = "grey")
    abline(v = 0, col = "grey")
    data <- getData(object)
    spec <- getSpec(object)
    constraints <- getConstraints(object)
    mvPortfolio <- minvariancePortfolio(data, spec, constraints)
    minvariancePoints(object, return = return, risk = risk, auto = FALSE,
                      pch = 19, col = "black")
    tangencyPoints(object, return = return, risk = risk, auto = FALSE,
                   pch = 19, col = "black")
    # tangencyLines(object, return = return, risk = risk, auto = FALSE, 
    #               col = "red")
    tangencyLines(object, return = return, risk = risk, auto = FALSE, 
                  col = "red")    # t_line add
    # xy <- equalWeightsPoints(object, return = return, risk = risk, 
    #                          auto = FALSE, pch = 15, col = "grey")
    # text(xy[, 1] + diff(xlim)/20, xy[, 2] + diff(ylim)/20, "Efficient-Frontier Line", 
    #      font = 3, cex = 0.7)
    if (is.null(col)) 
      col = rainbow(6)
       # col = "black"
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

  }

#############################################################################
frontierPlot2 <-
  function (object, frontier = c("both", "lower", "upper"), 
            col = c("black", "grey"), add = FALSE, labels = TRUE, 
            return = c("mean", "mu"), risk = c("Cov", 
                                               "Sigma", "CVaR", "VaR"), auto = TRUE, 
            title = TRUE, ...) 
  {
    stopifnot(length(col) == 2)
    frontier <- match.arg(frontier)
    fullFrontier = frontierPoints(object, frontier = "both", 
                                  return = return, risk = risk, auto = auto)
    upperFrontier <- frontierPoints(object, frontier = "upper", 
                                    return = return, risk = risk, auto = auto)
    lowerFrontier <- frontierPoints(object, frontier = "lower", 
                                    return = return, risk = risk, auto = auto)
    Arg <- match.call(expand.dots = TRUE)
    m <- match(c("xlim", "ylim"), names(Arg), Arg)
    xArg <- as.character(Arg[c(1, m)])[2]
    yArg <- as.character(Arg[c(1, m)])[3]
    if (xArg == "NULL" & yArg == "NULL") {
      yLim <- range(fullFrontier[, 2])
      xRange <- range(fullFrontier[, 1])
      xDiff <- diff(xRange)
      xLim <- c(xRange[1] - 2.5 * xDiff/10, xRange[2] + xDiff/10)
      if (!add) {
        if (frontier == "upper" | frontier == "both") {
          plot(upperFrontier, col = col[1], xlim = xLim, 
               ylim = yLim, ann = FALSE, ...)
        }
        else {
          if (frontier == "both") {
            points(fullFrontier, col = col[2], xlim = xLim, 
                   ylim = yLim, ...)
          }
          if (frontier == "lower") {
            plot(lowerFrontier, col = col[2], xlim = xLim, 
                 ylim = yLim, ann = FALSE, ...)
          }
        }
      }
      if (frontier == "upper" | frontier == "both") {
        points(upperFrontier, col = col[1], ...)
      }
      if (frontier == "lower" | frontier == "both") {
        points(lowerFrontier, col = col[2], ...)
      }
    }
    else if (xArg != "NULL" & yArg == "NULL") {
      yLim = range(fullFrontier[, 2])
      if (!add) {
        if (frontier == "upper" | frontier == "both") {
          plot(upperFrontier, col = col[1], ylim = yLim, 
               ann = FALSE, ...)
        }
        else {
          if (frontier == "both") {
            points(fullFrontier, col = col[2], ylim = yLim, 
                   ...)
          }
          if (frontier == "lower") {
            plot(fullFrontier, col = col[2], ylim = yLim, 
                 ann = FALSE, ...)
          }
        }
      }
      if (frontier == "upper" | frontier == "both") {
        points(upperFrontier, col = col[1], ...)
      }
      if (frontier == "lower" | frontier == "both") {
        points(lowerFrontier, col = col[2], ...)
      }
    }
    else if (xArg == "NULL" & yArg != "NULL") {
      xRange = range(fullFrontier[, 1])
      xDiff = diff(xRange)
      xLim = c(xRange[1] - 2.5 * xDiff/10, xRange[2] + xDiff/10)
      if (!add) {
        if (frontier == "upper" | frontier == "both") {
          plot(upperFrontier, col = col[1], xlim = xLim, 
               ann = FALSE, ...)
        }
        else {
          if (frontier == "both") {
            points(fullFrontier, col = col[2], xlim = xLim, 
                   ...)
          }
          if (frontier == "lower") {
            plot(lowerFrontier, col = col[2], xlim = xLim, 
                 ann = FALSE, ...)
          }
        }
      }
      if (frontier == "upper" | frontier == "both") {
        points(upperFrontier, col = col[1], ...)
      }
      if (frontier == "lower" | frontier == "both") {
        points(lowerFrontier, col = col[2], ...)
      }
    }
    else if (xArg != "NULL" & yArg != "NULL") {
      if (!add) {
        if (frontier == "upper" | frontier == "both") {
          plot(fullFrontier, type = "n", ann = FALSE, 
               ...)
          points(upperFrontier, col = col[1], ...)
        }
        if (frontier == "both") {
          points(lowerFrontier, col = col[2], ...)
        }
        if (frontier == "lower") {
          plot(lowerFrontier, col = col[2], ann = FALSE, 
               ...)
        }
      }
      else {
        if (frontier == "upper" | frontier == "both") {
          points(upperFrontier, col = col[1], ...)
        }
        if (frontier == "lower" | frontier == "both") {
          points(lowerFrontier, col = col[2], ...)
        }
      }
    }
    if (title) {
      labs = attr(fullFrontier, "control")
      # title(main = "Efficient Frontier", xlab = paste("Target Risk[", 
      #                                                 labs[1], "]", sep = ""), ylab = paste("Target Return[", 
      #                                                                                       labs[2], "]", sep = ""))
      title(main = "Efficient Frontier", 
            xlab = "Risk",
            ylab = "Return")
      
      }
    # mtext("Rmetrics", adj = 0, side = 4, cex = 0.7, col = "darkgrey")
    # invisible(fullFrontier)
  }
###################################################################
