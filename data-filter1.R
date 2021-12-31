#  install.packages("huxtable")

library(dplyr)
library(readr)
library(car)
# library(lmtest)
library(tidyverse)
library(data.table)
library(knitr)
library(stringr)
library(interactions)
library(jtools)
library("ggplot2")
library(huxtable)
library("broom.mixed")
# library(kableExtra)
#######################################################
#######################################################
#  Data filtering
#######################################################
#######################################################
#######################################################
dir <- ("./DATA")
file_list <- list.files(dir)

print(file_list)
# Date <- c("2021-12-27")
# 
#######################################################
# data01 <- T-bill : IRX : 13weeks t-bill - finance.yahoo
data01 <- read.csv("IRX.csv")
data01 <- data01 %>% select(1,6) 
colnames(data01)[2] <- "Tbill"
# data <- read.csv("~/R/r_test/DATA/^IRX.csv")[ ,c("Date", "Adj.Close")]
# colnames(data)[2] <- "IRX"
# colnames(data)[2] <- col_name
str(data01)


for(i in file_list)  {
  
  col_name <- str_sub(i, -11, -5)
  col_name <- str_replace_all(col_name, '-','')
  col_name <- str_replace_all(col_name, "^","")
  print(col_name)
  
  # write_name <- str_sub(csvfile, -7, -1)
  # Store the csv file into the data frame of "df"

  print(i)
  file_name <- paste(dir, i, sep = "/")
  print(file_name)
  
  temp <- read.csv(file_name,
                  header = TRUE, 
                  sep=",", 
                  stringsAsFactors = FALSE )
                  # stringsAsFactors = FALSE )[ ,c("Date", "Adj.Close")]  
  temp <- temp %>% select(1,6)  
  
  temp <- temp %>%
  mutate( ratio = ( temp[,2] - shift(temp[,2])) / shift(temp[,2]) * 100)
  
  # print("3")
    # temp <- temp %>% 
  #     mutate( ratio = ( Adj.Close - shift(Adj.Close)) / shift(Adj.Close) * 100 )
  #     str(temp)    
  # print("4")
      
      colnames(temp)[2] <- paste0(col_name,"close")
      colnames(temp)[3] <- col_name
          
  
  # class(data01)
  # class(temp)
  str(data01)
  # str(temp)
 
  data01 <- merge(data01, temp, by = "Date" , all=TRUE )
  

  }

str(data01)
write.csv(data01, file = "data1225.csv", row.names = F)

#######################################################
#######################################################
#######################################################
# 3-2. impact of covid19
# 
# total 5 years : 2017.01 ~ 2021.12
# 
# before covid19 : 2017.01 ~ 2019.12
# 
# after covid19 : 2020.01 ~ 2021.12
#######################################################
#######################################################
#######################################################
########################################################
########################################################
# for loop ###########################################

# data01 <- read.csv("data1225.csv")

data01 <- data01 %>% select( -ends_with("close"))


# excess return  = ratio - Tbill
  for(i in 3:ncol(data01))  {
  
      data01[,i] = data01[,i] - data01$Tbill

     }


data02 <- data01 %>% filter(Date < '2020-01-01') 
data03 <- data01 %>% filter(Date >= '2020-01-01')
data_j <- list(data01,data02,data03)

write.csv(data01, file = "data01.csv", row.names = F)
write.csv(data02, file = "data02.csv", row.names = F)
write.csv(data03, file = "data03.csv", row.names = F)


formula <- list(); formula_con <- list()

model      <- list(list(),list(),list(),list())  
model_con  <- list(list(),list(),list(),list()) 
HK_test    <- list(list(),list(),list(),list())  
step1_test <- list(list(),list(),list(),list()) 
step2_test <- list(list(),list(),list(),list()) 

yi <- list("BTCUSD","ETHUSD","BNBUSD","BTCUSD + ETHUSD + BNBUSD")


for (i in 1:4)  {
    for (j in 1:3) {
      

    formula[[i]] = paste0(yi[i] , " ~ ", "VTI + VV + VB + IDHQ + VWO + BND",
                 "+ TIP + GSG + VNQ + VNQI")
                
    model[[i]][[j]] <- lm(formula[[i]], data = data_j[[j]])  ###
    
    ##### HK test
    lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
    HK_test[[i]][[j]] <- lht(model[[i]][[j]],lhs,c(0,1))
   
    ##### step-1 test  : alpha = 0
    lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
    step1_test[[i]][[j]] <- lht(model[[i]][[j]],lhs,c(0))
    
    ##### step- test 2 : 
    ##### unresrticted model condition on alpha =0  
    formula_con[[i]] = paste0(yi[i] , " ~ ","0 + ", "VTI + VV + VB + IDHQ ",
                          "+ VWO + BND + TIP + GSG + VNQ + VNQI")
        model_con[[i]][[j]] <- lm(formula_con[[i]], data = data_j[[j]])
        ##### step- test 2 : beta=1 condition alpha = 0
        lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
        step2_test[[i]][[j]] <- lht(model_con[[i]][[j]],lhs,c(1))
    }
}


### Column nameing 
c1 <- c("BTCUSD","ETHUSD","BNBUSD","ALL")

names(formula) <- c1
names(formula_con) <- c1
names(model) <- c1
names(model_con) <- c1
names(HK_test) <- c1
names(step1_test) <- c1
names(step2_test) <- c1

save.image(file="data1225.RData")
################################################################
# End of data making 
#
################################################################























################################################################
#
#     --> 
#     print output TEST
#
#
################################################################



huxreg(model[[1]][[1]], model[[1]][[2]],model[[1]][[3]])

huxreg(model[[1]][[1]], model[[1]][[2]],model[[1]][[3]],
             model[[2]][[1]], model[[2]][[2]],model[[2]][[3]],
             model[[3]][[1]], model[[3]][[2]],model[[3]][[3]])
             # model.names = c("BTC all","BTC before","BTC after",
             #                 "ETH all","ETH before","ETH after","BNB all","BNB before","BNB after"),
             # scale = TRUE)
################################################################
################################################################
################################################################
################################################################
################################################################
################################################################

formula
formula_con
model
HK_test

for (i in 1:3)  {
  
  formula[[i]]
  for (j in 1:3) {
    
    print( HK_test[[i]][[j]]  )
    print( step1_test[[i]][[j]]  )
    print( step2_test[[i]][[j]]  )
    }
  
}

# model[[1]][[1]]$terms
# model[[1]][[2]]$terms
# model[[1]][[3]]$terms
# model[[2]][[1]]$terms
# model[[2]][[2]]$terms
# model[[2]][[3]]$terms
# model[[3]][[1]]$terms
# model[[3]][[2]]$terms 
# model[[3]][[3]]$terms

summary(model$BTCUSDR)
summary(model$ETHUSDR)
summary(model$BNBUSDR)

HK_test[[1]][[1]]
HK_test[[1]][[2]]
HK_test[[1]][[3]]
HK_test[[2]][[1]]
HK_test[[2]][[2]]
HK_test[[2]][[3]]
HK_test[[3]][[1]]
HK_test[[3]][[2]]
HK_test[[3]][[3]]

step1_test[[1]][[1]]
step1_test[[1]][[2]]
step1_test[[1]][[3]]
step1_test[[2]][[1]]
step1_test[[2]][[2]]
step1_test[[2]][[3]]
step1_test[[3]][[1]]
step1_test[[3]][[2]]
step1_test[[3]][[3]]

step2_test[[1]][[1]]
step2_test[[1]][[2]]
step2_test[[1]][[3]]
step2_test[[2]][[1]]
step2_test[[2]][[2]]
step2_test[[2]][[3]]
step2_test[[3]][[1]]
step2_test[[3]][[2]]
step2_test[[3]][[3]]

kable(HK_test)
kable(step1_test)
kable(step2_test)



#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################



step1_test$BTCUSDR
#  summary(step1_test$BTCUSDR)

summary(model[[3]])
summary(model[3])
summary(model$BTCUSDR)
print(model$BTCUSDR)
print(model$BTCUSDR$terms)

model
class(model)
class(model[1])
class(model[[1]])

summary(model)
summary(model[1])
summary(model[[1]])















 yi <- list("BTCUSDR","ETHUSDR","BNBUSDR")
 formula = paste0(yi[2], " ~ ", "VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
                 GSGR + VNQR + VNQIR")
 yi_lm <- lm(formula, data = data01)
 summary(yi_lm)


btc.ols <- lm(BTCUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
                GSGR + VNQR + VNQIR
              , data = data01)
summary(btc.ols)

btc.ols <- lm(ETHUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
                GSGR + VNQR + VNQIR
              , data = data01)
summary(btc.ols)
# intercept = 0
btc.ols2 <- lm(BTCUSDR ~ 0 +VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
                 GSGR + VNQR + VNQIR
               , data = data01)
########################################################
#
#  function
#
########################################################
wald_regress <- function(x) {
  print(x)

summary(btc.ols)
##### HK test
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
btc.hk <- lht(btc.ols,lhs,c(0,1))
btc.hk
##### step- test 1 : alpha = 0
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
btc.step1 <- lht(btc.ols,lhs,c(0))
btc.step1
##### step- test 2 : 
##### unresticted : regression with alpha = 0 
summary(btc.ols2)
##### resticted model : beta = 0 condition on alpha = 0
lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
btc.step2 <- lht(btc.ols2,lhs,c(1))
btc.step2           

summ(btc.ols,  digit = 3)  # unrestricted regression
summ(btc.ols2,  digit = 3)  # unrestricted regression on condition alpha = 0
kable(list( coef(btc.ols),coef(btc.ols2) )) # coefficient 

kable(btc.hk)             # HK test, alpha = 0 and beta = 1
kable(btc.step1)          # step test 1, alpha = 0
kable( btc.step2 )        # step test 2, beta = 1 on condition alpha = 0

plot_summs(btc.ols, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
plot_summs(btc.ols, btc.ols2, scale = TRUE)

export_summs(btc.ols, btc.ols2, scale = TRUE)
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################
#########################################################


# test Bitcoin-usd
data01 <- read.csv("data1225.csv")
# Bitcoin ###########################################

btc.ols <- lm(BTCUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
                GSGR + VNQR + VNQIR
              , data = data01)
summary(btc.ols)

##### HK test
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
btc.hk <- lht(btc.ols,lhs,c(0,1))
btc.hk

##### step- test 1 : alpha = 0
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
btc.step1 <- lht(btc.ols,lhs,c(0))
btc.step1
##### step- test 2 : 
##### unresticted : regression with alpha = 0 
btc.ols2 <- lm(BTCUSDR ~ 0 +VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
                 GSGR + VNQR + VNQIR
               , data = data01)
summary(btc.ols2)

lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
btc.step2 <- lht(btc.ols2,lhs,c(1))
btc.step2           

summ(btc.ols,  digit = 3)  # unrestricted regression
summ(btc.ols2,  digit = 3)  # unrestricted regression on condition alpha = 0
kable(list( coef(btc.ols),coef(btc.ols2) )) # coefficient 

kable(btc.hk)             # HK test, alpha = 0 and beta = 1
kable(btc.step1)          # step test 1, alpha = 0
kable( btc.step2 )        # step test 2, beta = 1 on condition alpha = 0

plot_summs(btc.ols, scale = TRUE, plot.distributions = TRUE, inner_ci_level = .9)
plot_summs(btc.ols, btc.ols2, scale = TRUE)

export_summs(btc.ols, btc.ols2, scale = TRUE)
#########################################################
# eterrium ###########################################
# eterrium ###########################################
olseth <- lm(ETHUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
             GSGR + VNQR + VNQIR
           , data = data01)
summary(olseth)
##### HK test
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
lht(olseth,lhs,c(0,1))
##### 2 step- test
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
lht(olseth,lhs,c(0))

}
wald_regress('test regression')
# bnb coin ############################################
olsbnb <- lm(BNBUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
             GSGR + VNQR + VNQIR
           , data = data01)
summary(olsbnb)
##### HK test
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
lht(olsbnb,lhs,c(0,1))
##### 2 step- test
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
lht(olsbnb,lhs,c(0)) 


# cypto joint ############################################
olscypto <- lm(BTCUSDR + ETHUSDR + BNBUSDR ~ VTIR + VVR + VBR + IDHQR + 
                 VWOR + BNDR + TIPR + 
                 GSGR + VNQR + VNQIR
               , data = data01)
summary(olscypto)
##### HK test
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
lht(olscypto,lhs,c(0,1))
##### 2 step- test
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
lht(olscypto,lhs,c(0)) 



# kable( fit1 )
# kable( fit2 )
   #######################################################
   # before covid19 : 2017.01 ~ 2019.12
   #######################################################  
  data_before <- data01 %>% 
    filter(Date < '2020-01-01') %>% 
      select(Date , contains("TNX"), ends_with("R"))   # select only ratio-column
    # tail(data_before) 
    ols0 <- lm(BTCUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
               GSGR + VNQR + VNQIR
             , data = data_before)
    summary(ols0)
  
    lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
    lht(ols0,lhs,c(0,1)) 
    
    ##### 2 step- test
    lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
    lht(ols0,lhs,c(0)) 

    #######################################################
    # after covid19 : 2020.01 ~ 2021.12
    #######################################################  
    data_before <- data01 %>% 
      filter(Date >= '2020-01-01') %>% 
      select(Date , contains("TNX"), ends_with("R"))   # select only ratio-column
    # tail(data_before) 
    ols0 <- lm(BTCUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
                 GSGR + VNQR + VNQIR
               , data = data_before)
    summary(ols0)
    
    lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
    lht(ols0,lhs,c(0,1))    
######################################################
csvfile <-"DATA/^IRX.csv"
col_name <- str_sub(csvfile, -7, -5)
write_name <- str_sub(csvfile, -7, -1)
# Store the csv file into the data frame of "df"
df <- read.csv(csvfile)[ ,c("Date", "Adj.Close")]


colnames(df)[2] <- col_name

write.csv(df,write_name,row.names=FALSE)

# Show the first five rows
kable(head(df,5),
      caption = "data.frame - df",
      align = c("r"))

# df01 <- transmute(df,
#           Date = Date,
#           BTC = ( BTC - shift(BTC)) / shift(BTC) * 100 - Tbill,
#           DBC = ( DBC - shift(DBC)) / shift(DBC) * 100 - Tbill,
#           GLD = ( GLD - shift(GLD)) / shift(GLD) * 100 - Tbill,
#           QQQ = ( QQQ - shift(QQQ)) / shift(QQQ) * 100 - Tbill,
#           SPY = ( SPY - shift(SPY)) / shift(SPY) * 100 - Tbill,
#           TLT = ( TLT - shift(TLT)) / shift(TLT) * 100 - Tbill)
# # head(df01)
# kable(head(df01),
#       caption = "dataframe - df01",
#       align = c("r","r","r","r","r","r","r"))

