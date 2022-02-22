##################### function : eff_line_plot
eff_line_plot <- function(data, testset, benchset) {
  # testset <- c('SNP', 'TLT', 'BTC','ETH')
  rets_test <- data[ ,testset]
  # benchset <- c( 'SNP', 'TLT')
  rets_bench <- data[ , benchset]
  
  shortSpec <- portfolioSpec()
  setNFrontierPoints(shortSpec) <- 50
  setRiskFreeRate <- mean(data$IEF)*100
  setSolver(shortSpec) <- "solveRshortExact"    
  
  
  # test data
  portfolio_rets <- rets_test %>% as.timeSeries() * 100
  eff_Frontier <- portfolioFrontier(portfolio_rets, spec = shortSpec,
                                    constraints = "Short")  # test-set
  # bench data
  portfolio_rets <- rets_bench %>% as.timeSeries() * 100
  eff_Frontier2 <- portfolioFrontier(portfolio_rets, spec = shortSpec,
                                     constraints = "Short")  # bench-set
  #Frontier line of test + bench
  longFrontier <- eff_Frontier
  tailoredFrontierPlot2(object = longFrontier,  twoAssets = TRUE, title = FALSE, 
                        risk = "Cov", sharpeRatio = FALSE, xlim = c(0,6))
  t_line <- tangencyLines(object = longFrontier, col = "red",
                          risk = "Cov", xlim = c(0,6), )
  t_mvpoint <- minvariancePoints(object = longFrontier, return = "mean",
                                 risk = "Cov", auto = TRUE, )
  # Frontier of benchmark
  longFrontier <- eff_Frontier2
  frontierPlot2(object = longFrontier, add = TRUE, title = TRUE, 
                col = c("blue","blue"), risk = "Cov", xlim = c(0,6), )
  b_line <- tangencyLines(object = longFrontier, col = "blue",
                          risk = "Cov", xlim = c(0,6), )
  b_mvpoint <- minvariancePoints(object = longFrontier, return = "mean", 
                                 risk = "Cov", auto = TRUE, )
  # slop slop_x slop_y GMV_x GMV_Y
  
  # eff1_GMV <- c(slop = t_line$slope, t_line$assets, # tangency slop
  eff1_GMV <- c(slop = t_line$slope,  # tangency slop              
                   GMV_X = t_mvpoint[1], # GMV x
                   GMV_Y = t_mvpoint[2], # GMV y
                   f_data = paste(eff_Frontier@data@data$names, collapse = ","))
  
  eff2_GMV <-c(slop =  b_line$slope, 
                  GMV_X =  b_mvpoint[1],
                  GMV_Y =  b_mvpoint[2],
                  f_data = paste(eff_Frontier2@data@data$names, collapse = ","))
  eff_GMV <- rbind(eff2_GMV, eff1_GMV)
  
  print(setRiskFreeRate)  # risk free rate : mean(IEF)*100 (given period)
  print(eff_GMV) # sharp rataio & GMV point
  
  # shortFrontier <- eff_Frontier
  # weightsPlot(shortFrontier)
  # text <- "Short Constrained Portfolio"
  # mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
  # weightedReturnsPlot(shortFrontier)
  # covRiskBudgetsPlot(shortFrontier)
  
  # my_list <- list(eff_Frontier, 
  #                 eff_Frontier2, 
  #                 eff_GMV)
  # return(my_list) 
  
  return(eff_GMV) 
  # shortFrontier <- eff_Frontier2
  # weightsPlot(shortFrontier)
  # text <- "MV Portfolio - Short Constrained Portfolio"
  # mtext(text, side = 3, line = 3, font = 2, cex = 0.9)
  # weightedReturnsPlot(shortFrontier)
  # covRiskBudgetsPlot(shortFrontier)
}
################# end function : eff_line_plot
