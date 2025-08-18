#' Plotting Development Profiles 
#'
#' The function provides a graphical representation of the functional profiles 
#' estimated by the PARALLAX, REACT, or MACRAME algorithm (see Maciak, 
#' Mizera, and Pesta (2022) for further details). The function takes an object 
#' of the class \code{profileLadder} which is the output of the 
#' \code{parallelReserve()} function or the \code{mcReserve()} function. 
#' Alternatively, the function can be also applied to visualise the run-off triangle 
#' itself---if the triangle is of the class \code{profileLadder}. 
#'
#' @param x an object of the class \code{profileLadder} (output form
#' \code{parallelReserve()}, \code{mcReserve()}, or \code{as.profileLadder()}
#' @param xlab label for the x axis
#' @param ylab label for the y axis
#' @param main title of the plot
#' @param default.legend logical to indicate whether a default plot legend 
#' (utilizing the information from the R class \code{profileLadder}) should be 
#' provided  (DEFAULT)
#' @param ... other graphical parameters to plot
#' @return A graph with the observed functional development profiles from the 
#' input run-off triangle, the estimated/predicted functional segments (i.e., 
#' functional profile completion provided by the corresponding estimation 
#' method---PARALLAX, REACT, or MACRAME) the and the true future profiles 
#' (if these are available)
#' 
#' @seealso [as.profileLadder()], [parallelReserve()], [mcReserve()]
#' 
#' @examples
#' ## completed run-off triangle with the 'unknown' (future) payments
#' print(triangle <- GFCIB$bodilyInjury[1:15, 1:15])
#' plot(mcReserve(triangle))
#' 
#' ## completed run-off triangle with unknown future
#' print(observed(triangle))
#' plot(mcReserve(observed(triangle)))
#' 
#' ## the run-off triangle with future payments without MACRAME completion
#' plot(as.profileLadder(triangle))
#' 
#'
#' 
#' @export
#' @method plot profileLadder
plot.profileLadder <- function(x, xlab = "Development period", 
                                  ylab = "Cumulative claims", main = "", 
                                  default.legend = TRUE, ...){
  
  predictedReserve <- x$reserve[3]
  completedLadder <- x$FullTriangle
  chainLadder <- x$Triangle
  fullLadder <- x$trueComplete
  method <- unlist(strsplit(x$method, " "))[1]
  
  n <- nrow(chainLadder) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout
  
  ### completed triangle or not
  if (!all(is.na(completedLadder))){
    observedLadder <- chainLadder
    completedLadder[observed] <- NA 
    completedLadder[last] <- chainLadder[last]
    estimatedLadder <- TRUE
  } else {estimatedLadder <- FALSE}

  if (all(is.na(fullLadder))){### true/full chain-ladder not available
    completed <- FALSE
  } else {completed <- TRUE}
  
  maxMargin <- max(chainLadder, na.rm = TRUE)
  minMargin <- min(chainLadder, na.rm = TRUE)
  
  xMargin <- 1 - n/100
  yMargin <- maxMargin + (maxMargin - minMargin)/100
  
  layout1 <- t(matrix(rep(1:n, 2), ncol = 2))
  layout2 <- rbind(rep(minMargin, n), rep(maxMargin, n))
  
  if (completed == TRUE){
    graphics::matplot(1:ncol(fullLadder), t(fullLadder), type = 'l', 
                      xlab = xlab, ylab = ylab, main = main, 
                      col = "darkblue", lty = 3, lwd = 1.5, ...)
    graphics::matlines(1:ncol(chainLadder), t(chainLadder), 
                       col = "darkblue", lwd = 2, lty = 1)
    
    graphics::points(1:n, chainLadder[last], pch =22, bg = "darkblue", cex = 1)
    graphics::points(rep(n,n), fullLadder[1:n,n], pch =22, bg = "darkgray", cex = 0.8)
    
    graphics::matlines(layout1, layout2, lty = 3, col = "black")
    
    if (estimatedLadder == TRUE){
      graphics::matlines(1:ncol(completedLadder), t(completedLadder), col = "red", lwd = 1, lty = 1)
      graphics::points(rep(n,n), completedLadder[1:n,n], pch =21, bg = "darkred", cex = 1)
      
      if (default.legend){
        graphics::legend("bottomright", legend = c(paste(method, " predicted reserve: ", 
                                                         round(predictedReserve ,0), sep = ""),
                                                   paste("Running diagonal: ", 
                                                         round(sum(chainLadder[last],0)), sep = ""),
                                                   paste("True (unknown) reserve: ", 
                                                         round(sum(fullLadder[,n]) - sum(chainLadder[last]) ,0), sep = "")),  
                         pch = 22, pt.bg = c("red", "darkblue", "gray"), fill = "lightgray", 
                         border = "lightgray", box.lwd = 0, box.col = "white", bg = "white")
      }
    } else {
      graphics::legend("bottomright", legend = c(paste("Running dianonal: ", 
                                                       round(sum(chainLadder[last],0)), sep = ""), 
                                                 paste("True (unknown) reserve: ", 
                                                       round(sum(fullLadder[,n]) - sum(chainLadder[last]) ,0), sep = "")),  
             pch = 22, pt.bg = c("darkblue", "gray"), fill = "lightgray", 
             border = "lightgray", box.lwd = 0, box.col = "white", bg = "white")
    }
  } else {
    graphics::matplot(1:ncol(chainLadder), t(chainLadder), type = 'l', 
                      xlab = xlab, ylab = ylab, main = main, 
                      col = "darkblue", lty = 1, lwd = 2, ...)
    graphics::points(1:n, chainLadder[last], pch =22, bg = "darkblue", cex = 1)
    
    graphics::matlines(layout1, layout2, lty = 3, col = "black")
    
    if (estimatedLadder == TRUE){
      graphics::matlines(1:ncol(completedLadder), t(completedLadder), col = "red", lwd = 1, lty = 1)
      graphics::points(rep(n,n), completedLadder[1:n,n], pch =21, bg = "darkred", cex = 1)
      
      if (default.legend){
        graphics::legend("bottomright", legend = c(paste(method, " predicted reserve: ", 
                                                         round(predictedReserve ,0), sep = ""), 
                                                   paste("Running diagonal: ", 
                                                         round(sum(chainLadder[last],0)), sep = "")),  
                         pch = 22, pt.bg = c("red", "darkblue"), fill = "lightgray", 
                         border = "lightgray", box.lwd = 0, box.col = "white", bg = "white")
      }
    } else {
      if (default.legend){
        graphics::legend("bottomright", legend = c(paste("Running diagonal: ", 
                                                         round(sum(chainLadder[last],0)), sep = "")),  
                         pch = 22, pt.bg = c("darkblue"), fill = "lightgray", 
                         border = "lightgray", box.lwd = 0, box.col = "white", bg = "white")
      }
    }
  }
}







  
  
