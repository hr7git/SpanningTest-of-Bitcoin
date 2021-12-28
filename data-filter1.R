library(dplyr)
library(readr)
library(car)
# library(lmtest)
library(tidyverse)
library(data.table)
# library(knitr)
library(stringr)


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
data01 <- read.csv("data1225.csv")
# Bitcoin ###########################################
olsbtc <- lm(BTCUSDR ~ VTIR + VVR + VBR + IDHQR + VWOR + BNDR + TIPR + 
             GSGR + VNQR + VNQIR
           , data = data01)
summary(olsbtc)
summ(olsbtc,  digit = 3)
##### HK test
lhs <- rbind(c(1,0,0,0,0,0,0,0,0,0,0),c(0,1,1,1,1,1,1,1,1,1,1))
lht(olsbtc,lhs,c(0,1))
##### 2 step- test
lhs <- c(1,0,0,0,0,0,0,0,0,0,0)
lht(olsbtc,lhs,c(0))



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


kable( fit1 )
kable( fit2 )
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

