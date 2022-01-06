
library(tidyr)
library(dplyr)
library(quantmod)
library(PerformanceAnalytics)
library(magrittr)
library(car) 
##########################################################
load("data_quant.RData")

# i = 1:3  yi
# j = 1:6  year of data : 1 = all
 nn_list <- list(list(),list(),list())
 
 ################# lm formula #############################
 lm_formula <- c( "`BTC-USD` ~ . -`ETH-USD`",
                     "`ETH-USD` ~ . -`BTC-USD`",
                     "`BTC-USD` + `ETH-USD` ~ . ") # i = 1:3
 
 lm_formula2 <- c( "`BTC-USD` ~ . -1 -`ETH-USD`",   # intercept = 0
                      "`ETH-USD` ~ . -1 -`BTC-USD`",
                      "`BTC-USD` + `ETH-USD` ~ . -1 ") # i = 1:3
 
 year <- c( "", "/2019", "2020/") # j=1:3 
# year <- list( "","2017" , "2018" , "2019" , "2020" , "2021") # j=1:6
######### Function ################################################
#  model_q  : regression
#  model_q2 : regression with intercept = 0
###################################################################

fun_regress = function(i,j){
  
  ########################
  fmla    <- as.formula(lm_formula[i])
  fmla2   <- as.formula(lm_formula2[i])
  lm_data <- rets[year[j]]
  
  model_q <- lm(fmla, data = lm_data)    # regression 
  # str(fmla)
  # summary(model_q)
  
  model_q2 <- lm(formula = fmla2, data = lm_data)  # regression alpha=0
  # str(fmla2)
  # summary(model_q2)
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
  # HK_test_q
  # HK_test_q[2,5]  # F test  - HK test
  # HK_test_q[2,6]  # Pr(>F)  - HK test
  
  ##### step-1 test  : alpha = 0
  lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
  step1_test_q <- lht(model_q,lhs,c(0))
  # step1_test_q
  # step1_test_q[2,5]  # F test  - step-1 test
  # step1_test_q[2,6]  # Pr(>F)  : step-1 test
  
  ##### step- test 2 : beta=1 condition on alpha = 0
  ##### unresrticted model condition on alpha =0  : model_q2
  lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
  step2_test_q <- lht(model_q2,lhs,c(1))
  # step2_test_q
  # step2_test_q[2,5]  # F test  : step- test 2
  # step2_test_q[2,6]  # Pr(>F)  : step- test 2
  
  # newlist <- list( model_q, model_q2, 
  #                  HK_test_q, step1_test_q, step2_test_q,
  #                  lm_formula[i],lm_formula2[i], year[j])
  #                # head(lm_data),tail(lm_data))
  
  newlist <- list( regressA = model_q, 
                   regressB = model_q2, 
                   HK = HK_test_q, 
                   step1 = step1_test_q, 
                   step2 = step2_test_q,
                   fmlaA = lm_formula[i],
                   fmlaB = lm_formula2[i], 
                   year = year[j])
  # head(lm_data),tail(lm_data))

  return(newlist)
  
}


for (i in 1:length(lm_formula))  {
  for (j in 1:length(year)) {
    
  nn_list[[i]][[j]] <- fun_regress(i,j)
  
  }
}
##################### Image ###################################
save.image(file="nn_list.RData") 
#  load("nn_list.RData")



