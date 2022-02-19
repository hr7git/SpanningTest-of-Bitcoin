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
      title(main = "", 
            xlab = "Risk",
            ylab = "Return")
      
    }
    # mtext("Rmetrics", adj = 0, side = 4, cex = 0.7, col = "darkgrey")
    # invisible(fullFrontier)
  }
#######