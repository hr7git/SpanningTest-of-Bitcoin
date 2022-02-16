library(quantmod)    # getsymbols, xts->dataframe in lm
library(PerformanceAnalytics)   # Return.calculate() 
library(magrittr)
library(car)        # lht 
library(tidyr)
library(dplyr)

load("rets2.rdata")
rets <- rets2[,1:10]
covmat = cov(rets)
###### slsqp() 함수를 이용한 최적화
objective = function(w) {
  obj = t(w) %*% covmat %*% w
  return(obj)
}

hin.objective = function(w) {
  return(w)
}

heq.objective = function(w) {
  sum_w = sum(w)
  return( sum_w - 1 )
}

library(nloptr)

result = slsqp( x0 = rep(0.1, 10),  # number of column
                fn = objective,
                hin = hin.objective,
                heq = heq.objective)

print(result$par)
print(result$value)
print(result$message)

w_1 = result$par %>% round(., 4) %>%
  setNames(colnames(rets))

print(w_1)

##### 11.1.2 solve.QP() 함수를 이용한 최적화
# data <- rets2[,1:10]
# covmat = cov(data)
#
Dmat = covmat
dvec = rep(0, 10)
Amat = t(rbind(rep(1, 10), diag(10), -diag(10)))
bvec = c(1, rep(0, 10), -rep(1, 10))
meq = 1

library(quadprog)
result = solve.QP(Dmat, dvec, Amat, bvec, meq)

print(result$solution)




###### 11.1.3 optimalPortfolio() 함수를 이용한 최적화

library(RiskPortfolios)

w_3 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                       constraint = 'lo')) %>%
      round(., 6) %>%
      setNames(colnames(rets))

print(w_3)  

w_4 = optimalPortfolio(covmat,
                       control = list(type = 'minvol',
                                      constraint = 'user')) %>%
  round(., 10) %>%
  setNames(colnames(rets))

print(w_4)  


#####
#####
# https://bookdown.org/compfinezbook/introcompfinr/application-to-vanguard-mutual-fund.html
library(corrplot) 
corrplot.mixed(cov2cor(covmat), upper = "ellipse")
