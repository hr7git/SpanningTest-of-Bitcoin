
# ### R-program / Spanning Test of Bitcoin 
# ---
#   variable | Ticker | explanation 
# :-------- | :----- | :-------------
#   yi0       |BTC     |Bitcoin   
# yi1       |TSLA    |Tesla, Inc   
# yi2       |SOXX    |iShares Trust - iShares Semiconductor ETF   
# yi3       |VOO     |Vanguard S&P 500 ETF   
# xi1       |DBC     |Invesco DB Commodity Index Tracking Fund     
# xi2       |GLD     |SPDR Gold Shares     
# xi3       |QQQ     |Invesco QQQ Trust  
# xi4       |SPY     |SPDR S&P 500 ETF Trust     
# xi5       |TLT     |iShares 20+ Year Treasury Bond ETF     
# 
# This is the price from yahoo.finance. For more details on price see <http://finance.yahoo>.  
# Risk free T-bill : Market Yield on U.S. Treasury Securities at 1-Month Constant Maturity (DGS1MO)
# Start date : mm/dd/yyyy ???
#   
#   ```{r ,echo=FALSE,message=FALSE}


# install.packages('rmarkdown')
# install.packages('tinytex')
# tinytex::install_tinytex() # install TinyTeX

library(dplyr)
library(car)
library(lmtest)



csvfile <-'data002.csv'
# Store the csv file into the data frame of "df"
df <- read.csv(csvfile)
# Show the first five rows
class(df)
head(df)

df01 <- transmute(df, 
          BTC = ( BTC - shift(BTC)) / shift(BTC) * 100 - Tbill,
          DBC = ( DBC - shift(DBC)) / shift(DBC) * 100 - Tbill,
          GLD = ( GLD - shift(GLD)) / shift(GLD) * 100 - Tbill,
          QQQ = ( QQQ - shift(QQQ)) / shift(QQQ) * 100 - Tbill,
          SPY = ( SPY - shift(SPY)) / shift(SPY) * 100 - Tbill,
          TLT = ( TLT - shift(TLT)) / shift(TLT) * 100 - Tbill)

head(df01)
summary(df01)


# summary(df)

yi0 <- df$BTC
yi1 <- df$TSLA
yi2 <- df$SOXX
yi3 <- df$VOO

xi1 <- df$DBC
xi2 <- df$GLD
xi3 <- df$QQQ
xi4 <- df$SPY
xi5 <- df$TLT


############################################################
# test Bitcoin-usd

ols0 <- lm(BTC ~ DBC + GLD + QQQ + SPY + TLT , data = df01)
summary(ols0)

# bptest(ols0) #BP-Test 

lhs <- rbind(c(1,0,0,0,0,0),c(0,1,1,1,1,1))
head(lhs)
lht(ols0,lhs,c(0,1))

############################################################
# test Tesla
ols1 <- lm(yi1 ~ xi1 + xi2 + xi3 + xi4 + xi5 , data = df)
summary(ols1)
lhs <- rbind(c(1,0,0,0,0,0),c(0,1,1,1,1,1))
lht(ols1,lhs,c(0,1))

############################################################
# test semicondutor industry etf
ols2 <- lm(yi2 ~ xi1 + xi2 + xi3 + xi4 + xi5 , data = df)
summary(ols2)
lhs <- rbind(c(1,0,0,0,0,0),c(0,1,1,1,1,1))
lht(ols2,lhs,c(0,1))

############################################################
# test VOO - bangard S&P 500
# Hypothesis: (Intercept) = 0 ,xi1  + xi2  + xi3  + xi4  + xi5 = 1
ols3 <- lm(yi3 ~ xi1 + xi2 + xi3 + xi4 + xi5 , data = df)
summary(ols3)
lhs <- rbind(c(1,0,0,0,0,0),c(0,1,1,1,1,1))
lht(ols3,lhs,c(0,1))

