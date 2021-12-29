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
# library(kableExtra)

#######################################################
#  Data filter
#######################################################


dir <- ("./DATA")
file_list <- list.files(dir)

Date <- c("2021-12-27")

data01 <- data.frame(Date)

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
                  stringsAsFactors = FALSE )[ ,c("Date", "Adj.Close")]
  
  print("3")
  
  temp <- temp %>% 
      mutate( ratio = ( Adj.Close - shift(Adj.Close)) / shift(Adj.Close) * 100 )
      str(temp)    
  print("4")
 
      colnames(temp)[2] <- col_name
  print("5")
      colnames(temp)[3] <- paste0(col_name,"R")
      
  
  class(data01)
  class(temp)
  str(data01)
  str(temp)
 
  data01 <- merge(data01, temp, by = "Date" , all=TRUE )
  

  }

str(data01)
write.csv(data01, file = "data1225.csv", row.names = F)
#######################################################
#  Plot
#######################################################
# data01 <- read.csv(file = "./DATA/DGS10.csv")
# ggplot() +
#    geom_point(mapping=aes(x=Date, y=DGS10), data=data01)
# 
# 
# 
# data01 %>%
#   ggplot(aes(x=Date, y=TNX)) +
#   geom_point(size = 1.5, alpha=0.5, color='blue') +
# #  geom_smooth(formula = y ~ x, method = "lm", se = FALSE) +
#   # method = "lm" : linear regerssion
#   
#   
#   # scale_x_continuous(labels = NULL) +
#  
#   labs(
#     x = "Date(2017 - 2021)",
#     y = "Excess Return of TNX"
#   )
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
# total 5 years : 2017.01 ~ 2021.12
# HK test
# 2 step test
#######################################################
# test Bitcoin-usd

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
#########################################################
#########################################################

########################################################
########################################################
# Bitcoin ###########################################

data01 <- read.csv("data1225.csv")
data01 <- data01 %>%  
               select(Date , contains("TNX"), ends_with("R")) 
data02 <- data01 %>% filter(Date < '2020-01-01') %>% 
               select(Date , contains("TNX"), ends_with("R")) 
data03 <- data01 %>% filter(Date >= '2020-01-01') %>%
               select(Date , contains("TNX"), ends_with("R"))
data_j <- list(data01,data02,data03)

formula <- list(); model <- list() ; HK_test <- list()
step1_test <- list() ; step2_test <- list()
formula_con <- list() ; model_con <- list() # formaula condition on alpha = 0
yi <- list("BTCUSDR","ETHUSDR","BNBUSDR")


for (j in 1:3) {
    for (i in 1:3) {
    
    formula[[i]] = paste0(yi[i] , " ~ ", "VTIR + VVR + VBR + IDHQR + VWOR + BNDR",
                 "+ TIPR + GSGR + VNQR + VNQIR")
                
    model[[i]] <- lm(formula[[i]], data = data_j[[j]])
    
    ##### HK test
    lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
    HK_test[[i]] <- lht(model[[i]],lhs,c(0,1))
   
    ##### step-1 test  : alpha = 0
    lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
    step1_test[[i]] <- lht(model[[i]],lhs,c(0))
    
    ##### step- test 2 : 
    ##### unresrticted model condition on alpha =0  
    formula_con[[i]] = paste0(yi[i] , " ~ ","0 + ", "VTIR + VVR + VBR + IDHQR ",
                          "+ VWOR + BNDR + TIPR + GSGR + VNQR + VNQIR")
        model_con[[i]] <- lm(formula_con[[i]], data = data01)
        ##### step- test 2 : beta=1 condition alpha = 0
        lhs <- rbind(c(1,1,1,1,1,1,1,1,1,1))
        step2_test[[i]] <- lht(model_con[[i]],lhs,c(1))
    }
}



c1 <- c("BTCUSDR","ETHUSDR","BNBUSDR")

names(model) <- c1
names(model_con) <- c1
names(HK_test) <- c1
names(step1_test) <- c1
names(step2_test) <- c1

model
summary(model$BTCUSDR)
summary(model$ETHUSDR)
summary(model$BNBUSDR)

HK_test
step1_test
step2_test

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











HK_test$BTCUSDR
#  summary(HK_test$BTCUSDR)

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



for ( i in 1:10) {
     y = 10 + 5*x
     print(y)
     summary(model[[i]])
   }

attach(ggplot2::diamonds)
strCols = names(ggplot2::diamonds)

formula <- list(); model <- list()
for (i in 1:1) {
  formula[[i]] = paste0(strCols[7], " ~ ", strCols[7+i])
  model[[i]] = glm(formula[[i]]) 
  
  #then you can plot or do anything else with the result ...
  png(filename = sprintf("diamonds_price=glm(%s).png", strCols[7+i]))
  par(mfrow = c(2, 2))      
  plot(model[[i]])
  dev.off()
}


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

