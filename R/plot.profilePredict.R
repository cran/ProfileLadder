#' Plotting Predicted Run-Off Diagonal 
#'
#' The function provides a graphical visualization of a 1-step-ahead prediction 
#' for the functional (development) profiles (so called new running diagonal) obtained 
#' by the S3 method \code{predict()} applied to the output of the PARALLAX, REACT, 
#' or MACRAME algorithm---the R functions \code{parallelReserve()} or \code{mcReserve()}.
#'
#' @param x an object of the class \code{profileLadder} which is the output form
#' \code{parallelReserve()} or \code{mcReserve()} 
#' @param xlab label for the x axis
#' @param ylab label for the y axis
#' @param main title of the plot
#' @param trueProfiles optional parameter (set to \code{NULL} by default) providing 
#' true profiles, if available. In such case, the predicted diagonal is also 
#' graphically compared with the true profile developments. The parameter can be 
#' a vector (of the same length as the number of rows in the run-off triangle) 
#' providing true values of the next running diagonal or it can be a matrix (an 
#' object of the class \code{matrix} or \code{triangle}) with the dimensions \code{n x m}
#' (where \code{n x n} is the dimension of the run-off triangle and \code{m > n}). 
#' @param default.legend logical to indicate whether a default plot legend 
#' (utilizing the information from the R class \code{profileLadder}) should be 
#' provided  (DEFAULT)
#' @param ... other graphical parameters to plot
#' @return A graph with the observed functional development profiles from the 
#' input run-off triangle and the predicted new running diagonal (1-step-ahead 
#' prediction)
#' 
#' @seealso [[parallelReserve()], [mcReserve()], [predict.profileLadder()]
#' 
#' @examples
#' ## new running diagonal provided by PARALLAX
#' print(CameronMutual)
#' plot(predict(parallelReserve(CameronMutual)))
#' 
#' ## new running diagonal with the true profiles
#' print(covid19CZ)
#' plot(predict(mcReserve(covid19CZ[,4:7])), trueProfiles = covid19CZ)
#' 
#' @export
#' @method plot profilePredict
plot.profilePredict <- function(x, xlab = "Development period", 
                                  ylab = "Cumulative claims", main = "", 
                                  trueProfiles = NULL,
                                  default.legend = TRUE, ...){
  ### prediction method
  method <- unlist(strsplit(x$method, "-"))[1]
  chainLadder <- x$extTriangle
  n <- nrow(chainLadder) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout
  
  extendLadder <- chainLadder ### observed run-off triangle with the predicted diagonal
  chainLadder <- observed(chainLadder[,1:n]) ### observed run-off triangle
  
  if (!is.null(trueProfiles)){### true profiles are provided 
    if (inherits(trueProfiles, c("numeric"))){### values for the new diagonal are provided
      if (length(trueProfiles) != n){stop("The length of 'trueProfiles' must be same as the number of rows in the run-off triangle.")} else {
        trueLadder <- cbind(chainLadder, rep(NA, n))
        trueLadder[,2:(n + 1)][last] <- trueProfiles
      }
    } else {
      if (inherits(trueProfiles, c("triangle", "matrix"))){### the whole triangle/matrix for true profiles is provided
        if (dim(trueProfiles)[1] != n || dim(trueProfiles)[2] <= (n + 1)){stop("Incorrect dimensions of the 'trueProfiles' object! \n The number of columns in 'trueProfiles' must be larger than the dimension of the run-off triangle.")} 
        else {
          trueLadder <- cbind(trueProfiles[,1:(ncol(trueProfiles) - n)], observed(trueProfiles[,(ncol(trueProfiles) - n + 1):ncol(trueProfiles)]))
        }
      } else {stop("Incorrect values for 'trueProfies' are provided. \n The parameter must be either a numeric vector of the same length as the number of rows in the run-off triangle \n or it can be a matrix with the dimensions n times m where n is the number of rows in the run-off triangle (the input object x) and m > n.")}
    }
  }

  
  if (!is.null(trueProfiles)){### true profiles are available
    ### aling predicted triangle with the true profiles 
    if (!all((chainLadder == observed(trueLadder[, (ncol(trueLadder) - n):(ncol(trueLadder) - 1)]))[observed(n)])){
      stop("The provided 'trueProfiles' do not correspond with the input run-off triangle!")
    } else {
      if (ncol(trueLadder) > n + 1){
        ### observed triangle
        chainLadderExt <- cbind(trueLadder[, 1:(ncol(trueLadder) - (n + 1))], chainLadder)
        ### observed triangle with the predicted last diagonal
        extendLadder <- cbind(trueLadder[, 1:(ncol(trueLadder) - (n + 1))], extendLadder)
      } else {
        chainLadderExt <- chainLadder
      }
      m <- ncol(extendLadder)
    }
    
    ### plotting code
    maxMargin <- max(max(extendLadder, na.rm = TRUE), max(trueLadder, na.rm = TRUE))
    minMargin <- min(min(extendLadder, na.rm = TRUE), min(trueLadder, na.rm = TRUE))
    
    xMargin <- 1 - n/100
    yMargin <- maxMargin + (maxMargin - minMargin)/100
    
    layout1 <- t(matrix(rep(1:m, 2), ncol = 2))
    layout2 <- rbind(rep(minMargin, n), rep(maxMargin, n))
    
    graphics::matplot(1:ncol(trueLadder), t(trueLadder), type = 'l', 
                      xlab = xlab, ylab = ylab, main = main, 
                      col = "darkblue", lty = 3, lwd = 1.5, ...)
    
    graphics::matlines(1:m, t(extendLadder), col = "#CC00CC", lwd = 2, lty = 1)
    graphics::points((m - n + 1):m, extendLadder[,(m - n + 1):m][last], pch =21, bg = "darkred", cex = 1)
    
    graphics::matlines(1:ncol(chainLadderExt), t(chainLadderExt), 
                       col = "darkblue", lwd = 2, lty = 1)
    
    graphics::points((m - n):(m - 1), chainLadder[last], pch =22, bg = "darkblue", cex = 1)
    graphics::points((m - n + 1):m, trueLadder[,(m - n + 1):m][last], pch =22, bg = "darkgray", cex = 0.8)
    
    graphics::matlines(layout1, layout2, lty = 3, col = "black")
    
    if (default.legend){
      graphics::legend("topleft", legend = c(paste(method, " predicted diagonal: ", 
                                                       round(x$reserve,0), sep = ""),
                                                 paste("True new running diagonal: ", 
                                                       round(sum(trueLadder[,(m - n + 1):m][last]), 0), sep = "")),  
                       pch = 22, pt.bg = c("#CC00CC", "gray"), fill = "lightgray", 
                       border = "lightgray", box.lwd = 0, box.col = "white", bg = "white")
    }
  } else {
    maxMargin <- max(extendLadder, na.rm = TRUE)
    minMargin <- min(extendLadder, na.rm = TRUE)
    
    xMargin <- 1 - n/100
    yMargin <- maxMargin + (maxMargin - minMargin)/100
    
    layout1 <- t(matrix(rep(1:(n + 1), 2), ncol = 2))
    layout2 <- rbind(rep(minMargin, n), rep(maxMargin, n))
    
    graphics::matplot(1:ncol(extendLadder), t(extendLadder), type = 'l', 
                      xlab = xlab, ylab = ylab, main = main, 
                      col = "#CC00CC", lty = 1, lwd = 2, ...)
    
    graphics::points(2:(n + 1), extendLadder[,2:(n + 1)][last], pch =21, bg = "darkred", cex = 1)
    
    graphics::matlines(1:ncol(chainLadder), t(chainLadder), 
                       col = "darkblue", lwd = 2, lty = 1)
    
    graphics::points(1:n, chainLadder[last], pch =22, bg = "darkblue", cex = 1)
   
    graphics::matlines(layout1, layout2, lty = 3, col = "black")
    
    if (default.legend){
      graphics::legend("topleft", legend = c(paste(method, " predicted diagonal: ", 
                                                       round(x$reserve, 0), sep = "")),  
                       pch = 22, pt.bg = c("#CC00CC"), fill = "lightgray", 
                       border = "lightgray", box.lwd = 0, box.col = "white", bg = "white")
    }
  }
}







  
  
