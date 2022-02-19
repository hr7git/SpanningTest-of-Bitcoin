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