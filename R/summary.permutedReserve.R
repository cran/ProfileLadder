#' Summary Method for Objects of the S3 Class Method \code{permutedReserve}
#'
#' The function provides an overall summary of the output from the functions 
#' \code{permuteReserve()} (i.e., the summary of the object of the class
#' \code{permutedReserve})
#'
#' @param object an object of the class \code{permutedReserve} -- i.e., the output 
#' form the \code{permuteReserve()} functions
#' @param ... not used
#' 
#' @return Summary of the completed functional profiles (provided by one of the 
#' functions \code{parallelReserve()} or \code{mcReserve()})  and the overall 
#' reserve distribution obtained in terms of the permutation bootstrap -- the 
#' function \code{permuteReserve()}. 
#' The output is a list with the following items:
#' \item{origins}{a matrix with the row-specific summary of the completed 
#' functional profiles (except the first fully developed profile---i.e., the first 
#' row in the run-off triangle). The first column of the matrix (\code{First}) 
#' gives the first origin payments; The second column (\code{Latest}) gives 
#' the last available (cumulative) payments (i.e., values from the last running 
#' diagonal in the run-off triangle); The third column (\code{Dev.To.Date}) gives 
#' a relative proportion of the paid amount (\code{Latest}) with respect to the 
#' estimated ultimate (\code{Ultimate}) given in the fourth column; The column 
#' denoted as \code{IBNR} gives the estimated amount still left to pay (Incurred 
#' But Not Reported)}; The sixth column provides the estimated standard errors 
#' (\code{S.E.}) of IBNR obtained from the permutation bootstrap; The last column returns
#' the corresponding coefficients of variation (\code{CV}).
#' \item{overall}{Table with the summary of the true/estimated reserve: 
#' \code{Paid amount} represents the sum of the last running diagonal; 
#' \code{Estimated reserve} gives the reserve estimate provided by one of the 
#' estimation algorithm (PARALLAX, REACT, or MACRAME); \code{True reserve} is 
#' given as a sum of the last column (if available, \code{NA} otherwise); Finally, 
#' some Accuracy in terms of \code{Reserve\%} is given as a percentage of the 
#' estimated reserve with respect to the true reserve (see Maciak, Mizera, and 
#' Pešta (2022) and \code{Dev.To.Date} gives the proportion of the overall 
#' estimatd ultimate and the overall paid amount}
#' \item{dist}{Table with basic empirical characteristics of the overall reserve
#' distribution provided by the permutation bootstrap: \code{Boot.Mean} stands 
#' for the empirical mean of the bootstrap distribution; \code{Std.Er.} gives the
#' corresponding standard error of the bootstrap distribution; \code{BootCov\%} 
#' stands for a percentage proportion between the standard error and the empirical
#' mean of the bootstrap distribution; Finally, \code{BootVar.xxx} provides the 
#' estimated quantile of the bootstrap reserve distribution (0.995 by DEFAULT).}
#' 
#' @seealso [parallelReserve()], [mcReserve()], [permuteReserve()]
#' 
#' @examples
#' data(CameronMutual)
#' summary(CameronMutual)
#' 
#' ## summary for the point reserve prediction
#' summary(parallelReserve(CameronMutual))
#' 
#' ## summary for the overall reserve distribution
#' summary(permuteReserve(parallelReserve(CameronMutual)))
#' 
#' @export
#' @method summary permutedReserve
summary.permutedReserve <- function(object, ...){
  ### estimation methods
  methods <- c("PARALLAX", "REACT", "MACRAME")
  method <- methods[methods %in% unlist(regmatches(object$method, 
                    gregexpr(paste(methods, collapse = "|"), object$method)))]
  
  ### overall summaries
  reserve <- object$eSummary
  distSummary <- object$pSummary
  
  ### permuted values
  pReserve <- object$pReserves
  pUltimates <- object$pUltimates
  pLatest <- object$pLatestCum ### cumulative (simulated) diagonals
  
  ### input/output data
  inputTriangle <- object$inputTriangle
  completed <- object$completed
  trueLadder <- object$trueComplete
  trueUltimate <- object$tUltimate
  trueLatest <- object$tLatest
  
  method <- gsub("\\(", "", strsplit(object$method, " ")[[1]][3])
  B <- as.numeric(strsplit(object$info[3], " = ")[[1]][2])
  n <- as.numeric(strsplit(object$info[4], " = ")[[1]][2])
  
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout
  
  ### 1. summary of the reserve estimation (origin specific)
  latest <- rev(inputTriangle[last])[-1] ### true latest diagonal
  ultimate <- completed[,n][-1] ### predicted ultimates
  
  dev.to.date <- latest/ultimate
  IBNR <- ultimate - latest
  first <- inputTriangle[,1][-1]
  
  ### caltulating standard errors from origin specific (bootstrapped) IBNRs 
  SE <- apply(pUltimates - pLatest, 2, stats::sd)[-1]
  
  ### calculating CV 
  CV <- SE / IBNR
  
  sTable <- data.frame(first, latest, dev.to.date, ultimate, IBNR, SE, CV)
  sTable <- rbind(sTable, apply(sTable, 2, sum))
  
  sTable[n, 3] <- sTable[n,2]/sTable[n,4]
  sTable[n, 6] <- stats::sd(pReserve)
  sTable[n, 7] <- sTable[n, 6] / sTable[n, 5]
    
  names(sTable) <- c("First", "Latest", "Dev.To.Date", "Ultimate", "IBNR", "S.E", "CV")
  row.names(sTable) <- c(as.character(2:n), "total")
  
  ### 2. summary of the reserve prediction (overall)
  if (!is.na(reserve[4])){### true reserve is available
    reserve <- c(reserve[1:2], sum(trueLadder[,n]), reserve[3:4])
    names(reserve) <- c("Paid Amount", "Est.Ultimate", "True Ultimate", "Est.Reserve", "True Reserve")
  } else {
    reserve <- reserve[1:3]
  }
  
  ### 3. summary of the overall reserve distribution
  
  output <- list()
  output$origins <- sTable
  output$overall <- reserve
  output$dist <- distSummary
  
  cat(paste(method, "based reserving (with B =", B, "bootstrap permutations)\n", sep = " "))
  print(sTable)
  cat("\n")
  
  message("Overall reserve distribution")
  print(distSummary)
  cat("\n")
}