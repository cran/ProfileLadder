#' Summary method for an object of the S3 class method \code{profileLadder}
#'
#' The function provides an overall summary of the output from the functions 
#' parallelReserve() and mcReserve() (summary of the object of the class
#' \code{profileLadder})
#'
#' @param object an object of the class \code{profileLadder} -- i.e., either a 
#' run-off triangle itself or the output form the \code{parallelReserve()} or 
#' \code{mcReserve()} functions
#' @param plotOption logical to indicate whether a graphical output should be 
#' also provided (set by DEFAULT to \code{FALSE}). If the incremental residuals 
#' (standard or back-fitted) are provided within the object \code{x} the plot 
#' provides a summary of the residuals (otherwise a simple barplot summarizing 
#' the estimated reserve is given)
#' @param ... not used
#' 
#' @return Summary of the completed functional profiles and the estimated reserve 
#' (provided by the function \code{parallelReserve()} or \code{mcReserve()}). 
#' Summary of the incremental residuals (standard or backfitted) is also provided 
#' if the residuals are available. The output is a list with the following items:
#' \item{origins}{a matrix with the row-specific summary of the completed 
#' functional profiles (except the first fully developed profile---i.e., the first 
#' row in the run-off triangle). The first column of the matrix (\code{First}) 
#' gives the first origin payments; The second column (\code{Latest}) gives 
#' the last available (cumulative) payments (i.e., values from the last running 
#' diagonal in the run-off triangle); The third column (\code{Dev.To.Date}) gives 
#' a relative proportion of the paid amount (\code{Latest}) with respect to the 
#' estimated ultimate (\code{Ultimate}) given in the fourth column; Finally, the 
#' last column (\code{IBNR}) gives the estimated amount still left to pay (Incurred 
#' But Not Reported)}
#' \item{overall}{Table with the summary of the true/estimated reserve: 
#' \code{Paid amount} represents the sum of the last running diagonal; 
#' \code{Estimated reserve} gives the reserve estimate provided by one of the 
#' estimation algorithm (PARALLAX, REACT, or MACRAME); \code{True reserve} is 
#' given as a sum of the last column (if available, \code{NA} otherwise); Finally, 
#' some Accuracy in terms of \code{Reserve\%} is given as a percentage of the 
#' estimated reserve with respect to the true reserve (see Maciak, Mizera, and 
#' Pešta (2022) and \code{Dev.To.Date} gives the proportion of the overall 
#' estimatd ultimate and the overall paid amount}
#' \item{resids}{Table with basic empirical description characteristics of the 
#' residuals (standard or back-fitted) if the residuals are provided in \code{x}}
#' 
#' @seealso [as.profileLadder()], [parallelReserve()], [mcReserve()]
#' 
#' @examples
#' data(CameronMutual)
#' summary(CameronMutual)
#' 
#' ## standard summary output
#' summary(mcReserve(CameronMutual))
#' 
#' ## summary output with plotOption = TRUE
#' summary(mcReserve(CameronMutual), plotOption = TRUE)
#' 
#' ## summary output with (standard) residuals and plotOption = TRUE
#' summary(mcReserve(CameronMutual, residuals = TRUE), plotOption = TRUE)
#' 
#' ## summary output with (back-fitted) residuals and plotOption = TRUE
#' summary(mcReserve(observed(CameronMutual), residuals = TRUE), plotOption = TRUE)
#' 
#' 
#' @export
#' @method summary profileLadder
summary.profileLadder <- function(object, plotOption = FALSE, ...){

  reserve <- object$reserve
  completedLadder <- object$completed
  chainLadder <- object$inputTriangle
  fullLadder <- object$trueComplete
  method <- unlist(strsplit(object$method, " "))[1]
  
  n <- nrow(chainLadder) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout
  
  ### 1. summary of the reserve estimation
  latest <- rev(chainLadder[last])[-1]
  if (!all(is.na(completedLadder))){
    ultimate <- completedLadder[,n][-1]
  } else {ultimate <- NA}
  
  dev.to.date <- latest/ultimate
  IBNR <- ultimate - latest
  first <- chainLadder[,1][-1]
  
  sTable <- data.frame(first, latest, dev.to.date, ultimate, IBNR)
  sTable <- rbind(sTable, apply(sTable, 2, sum))
  sTable[n, 3] <- sTable[n,2]/sTable[n,4]
  names(sTable) <- c("First", "Latest", "Dev.To.Date", "Ultimate", "IBNR")
  row.names(sTable) <- c(as.character(2:n), "total")
  
  if (is.na(reserve[3])){estimatedReserve <- NA} else 
    {estimatedReserve <- format(round(reserve[3], 2), nsmall = 2)}
  if (is.na(reserve[3]) | is.na(reserve[4])){estimationAccuracy <- NA} else 
    {estimationAccuracy <- format(round(100 * abs(reserve[3]/reserve[4] - 1), 2), nsmall = 2)}
  if (is.na(sTable[n,3])){dev2date <- NA} else 
    {dev2date <- format(round(100 * sTable[n,3], 2), nsmall = 2)}
 
  reserveSummary <- as.numeric(c(sum(chainLadder[last]), estimatedReserve, reserve[4], 
                                 estimationAccuracy, dev2date))
  names(reserveSummary) <- c("Paid Amount","   Estimated Reserve", 
                             "   True Reserve", "  Reserve%", "   Dev.To.Date")

  output <- list()
  output$origins <- sTable
  output$overall <- reserveSummary
  
  if (method == "Run-off"){### run-off triangle only
    print(paste(method, " triangle", sep = ""))
  } else {### estimated triangle (PARALLAX, REACT, MACRAME)
    print(paste(method, " estimated reserve", sep = ""))
  }
  
  
  ### 2. summary of residuals
  if (!is.null(object$residuals)){
    if (all(is.na(object$residuals[observed(n)]))){### standard residuals
      residType <- "(standard incremental residuals)" 
      xLabName <- "Standard incremental residuals"
    } else {### backfitted residuals
      residType <- "(backfitted incremental residuals)"
      xLabName <- "Backfitted incremental residuals"
    }
    
    message(residType)
    resids <- object$residuals[!is.na(object$residuals)]
    residSummary <- c(round(as.numeric(summary(resids))), round(stats::sd(resids)), 
                      length(resids))
    names(residSummary) <- c(" Min", " 1st Q.", " Median", " Mean", " 3rd Q.", 
                             " Max", " Std.Er.", " Total")

    if (plotOption == T){
      dEst <- stats::density(resids)
      graphics::hist(resids, col = "lightblue", xlab =  xLabName, freq = FALSE, 
                     breaks = n, main = "", ylim = c(0, max(dEst$y)))
      graphics::lines(rep(mean(resids), 2), c(0, max(dEst$y)), lwd = 3, 
                      col = "darkred", lty = 2)
      graphics::polygon(dEst$x, dEst$y, 
                        col = grDevices::adjustcolor( "red", alpha.f = 0.1))
      graphics::lines(dEst, lwd = 3, col = "red", lty = 1)
      
    }
    
    output$resids <- residSummary
  } else {
    if (plotOption == T){
      graphics::barplot(reserve[c(2,1,3)], ylim = c(0, 1.1 * max(reserve[c(2,1,3)])), 
                        col = c("gray", "gray", "lightblue"))
      if (!is.na(reserve[4])){
        graphics::abline(reserve[4], 0, lwd = 3, col = "darkred")
        graphics::legend("topright", legend = c(paste("Estimated Reserve:", reserve[3], sep = " "), 
                                                paste("True Reserve:", reserve[4], sep = " ")), 
                         pch = 22, pt.bg = c("lightblue", "darkred"),
               fill = "lightgray", border = "lightgray", box.lwd = 0, 
               box.col = "white", bg = "white")
      }
    }
  }
  
  return(output)
}