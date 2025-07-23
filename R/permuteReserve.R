#' Permutation Bootstrap Reserve (PARALLAX, REACT, MACRAME)
#'
#' The function takes the output from the function \code{parallelReserve()} or 
#' \code{mcReserve} and estimates the overall reserve distribution in terms of the 
#' permutation bootstrap approach proposed in Maciak, Mizera, and Pešta (2022).
#'
#' @param object an object of the class \code{profileLadder} (output from 
#' \code{parallelReserve()} or \code{mcReserve()} respectively)
#' @param B number of permutations to be performed (DEFAULT \code{B = 500})
#' @param std logical to indicate whether the run-off triangle should be 
#' standardized by the first column increments (DEFAULT) or not (\code{std = FALSE}). 
#' For more details about the triangle standardization, see Maciak, Mizera, and 
#' Pešta (2022)
#' @param quantile quantile level for the \code{BootVar.} characteristic of the 
#' bootstrapped distribution (the DEFAULT choice \code{quantile = 0.995} is 
#' explicitly required  by the Solvency II principle used by actuaries in practice)  
#' 
#' @returns An object of the class \code{permutedReserve} which is a list with  
#' the following elements: 
#' \item{eSummary}{numeric vector with four values summarizing the estimated 
#' reserve: Paid amount (i.e., the sum of the last observed diagonal in the given  
#' cumulative run-off triangle); Estimated ultimate (i.e., the sum of the 
#' last column in the completed cumulative triangle); Estimated reserve (i.e., the 
#' sum of the last column in the completed cumulative triangle minus the sum of 
#' the last observed diagonal); True reserve if a completed (true) run-off triangle 
#' is available}
#' \item{pSummary}{numeric vector with four values summarizing the overall reserve 
#' distribution: \code{Boot.Mean} gives the verage of \code{B} permutation bootstrap 
#' reserves; \code{Std.Er.} provides the corresponding standard error of \code{B} 
#' permutation bootstrap reserves; The value of \code{BootCov\%} stands for  
#' a percentage proportion between the standard error and the average; 
#' Finally, \code{BootVar.995} provides the estimated 0.995 quantile (by DEFAULT)
#' of the bootstrap reserve distribution (for \code{quantile = 0.995} and, otherwise, 
#' it is modified acordingly) given relatively with respect to the permutation 
#' bootstrapped mean reserve}
#' \item{pReserves}{a numeric vector of the length \code{B} with the estimated 
#' (permuted) reserves for each row-permuted run-off triangle in \code{B} 
#' independent Monte Carlo simulation runs}
#' \item{pUltimates}{A matrix of the dimensions \code{B x n} (where \code{n} 
#' stands for the number of the origin/development periods) with \code{B} simulated 
#' ultimate payments -- the last column in the completed run-off triangle}
#' \item{pLatest}{A matrix of the dimensions \code{B x n} (where \code{n} again
#' stands for the number of the origin/development periods) with \code{B} simulated 
#' incremental diagonals}
#' \item{pLatestCum}{A matrix of the dimensions \code{B x n} (\code{n} being the 
#' number of the origin/development periods) with \code{B} simulated cumulative
#' diagonals}
#' \item{inputTriangle}{The input run-off triangle}
#' \item{completed}{The completed run-off triangle by using one of the PARALLAX, 
#' REACT, or MACRAME estimation method}
#' \item{trueComplete}{The true complete run-off triangle (if available) and \code{NA}
#' value otherwise}
#' \item{info}{a numeric vector summarizing the bootstrap compuational efficiency: 
#' In particular, the OS/Architecture type, the number of permutations (\code{B}), 
#' the input run-off triangle dimension (\code{n}) and the computation time needed
#' for the permutation bootstrap calculations}
#' 
#' @seealso [parallelReserve()], [mcReserve()], [plot.permutedReserve()]
#' 
#' @examples
#' ## REACT algorithm and the permutation bootstrap reserve 
#' data(CameronMutual)
#' output <- parallelReserve(CameronMutual, method = "react")
#' permuteReserve(output, B = 100)
#' 
#' ## MACRAME algorithm with a pre-specified number of states 
#' output <- mcReserve(CameronMutual, states = 5)
#' permuteReserve(output, B = 100)
#' 
#' 
#' @references Maciak, M., Mizera, I., and Pešta, M. (2022). Functional Profile 
#' Techniques for Claims Reserving. ASTIN Bulletin, 52(2), 449-482. DOI:10.1017/asb.2022.4
#' @references European Parliament and Council (2009). Directive 2009/138/EC of 
#' the European Parliament and of the Council of 25 November 2009 on the taking-up 
#' and pursuit of the business of Insurance and Reinsurance (Solvency II). Official 
#' Journal of the European Union, 1–155.\cr
#' \url{https://data.europa.eu/eli/dir/2009/138/oj}
#' 
#' @export
permuteReserve <- function(object, B = 500, std = TRUE, quantile = 0.995){
  ### input data checks
  if (!inherits(object, "profileLadder")){
    stop("The input object must be of a class 'profileLadder'")}
  if (inherits(object, "profileLadder") && all(is.na(object$completed))){
    stop("The input object must be a result of 'parallelReserve()' or 'mcReserve()'")}
  if (!is.numeric(B) || length(B) > 1){
    stop("The number of bootstrap permutations 'B' must be a numeric (integer) value")}
  if (round(B, 0) != B || B <= 0){
    stop("The number of boostrap permutations 'B' must be a positive integer value")} 
  if (B < 30){
    warning("The number of bootstrap permutations 'B' is too low")} 
  if (!is.numeric(quantile) || length(quantile) > 1){
    stop("The quantile value must be a single value in interval (0,1)")}
  if (quantile >= 1 || quantile <= 0){
    stop("The quantile value must be in interval (0,1)")}
  
  ### reserve summary
  reserve <- object$reserve 
  ### estimation method ()
  method <- unlist(strsplit(object$method, split = " "))[1]
  completed <- object$completed
  inputTriangle <- object$inputTriangle
  trueComplete <- object$trueComplete
  
  if (method == "MACRAME"){
    if (!is.null(object$MarkovChain)){
      states <- object$MarkovChain$states
      breaks <- object$MarkovChain$breaks
      std <- FALSE
    } else {
      states <- NULL
      breaks <- NULL
    }
  }

  n <- nrow(completed) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout 
  
  ### triangle standardization
  if (std == TRUE){
    firstPositive <- apply(completed, 1, function(row){return(row[row > 0][1])})
    firstPositive[is.na(firstPositive)] <- 0
    completed[firstPositive != 0, ] <- completed[firstPositive != 0, ] / 
                                       firstPositive[firstPositive != 0]
  }
  
  ### auxiliary permutation function
  permute <- function(matrix, method = NULL){
    pMatrix <- matrix[sample(1:n, n, replace = FALSE), ]
    zeroRows <- apply(pMatrix, 1, function(row){return(all(row == 0))})
    pMatrix[!observed] <- NA
    
    ### back-rescaling (back-standardization)
    if (std == TRUE){
      pMatrix[!zeroRows, ] <- pMatrix[!zeroRows, ] * firstPositive[firstPositive != 0]
    }
    
    ### applying PARALLAX, REACT, MACRAME
    if (method == "PARALLAX"){out <- parallelReserve(pMatrix, method = "parallax")$completed}
    if (method == "REACT"){out <- parallelReserve(pMatrix, method = "react")$completed}
    if (method == "MACRAME"){
      if (is.null(states) & is.null(breaks)){out <- mcReserve(pMatrix)$completed} 
        else {out <- mcReserve(pMatrix, states = states, breaks = breaks)$completed}
    }  
    return(c(out[,n], rev(out[last]), rev(ChainLadder::cum2incr(out)[last])))
  }
  
  ### permutation bootstrap with time efficiency  
  startTime <- Sys.time()
  pReserve <- replicate(B, permute(completed, method))
  endTime <- Sys.time() - startTime
    
  ### bootstrapped ultimates
  ultimates <- data.frame(t(pReserve[1:n,]))
  names(ultimates) <- paste("origin", 1:n, sep = " ")
  
  ### bootstrapped latest (cummulative)
  latestCum <- data.frame(t(pReserve[(n + 1):(2 * n),]))
  names(latestCum) <- paste("origin", 1:n, sep = " ")
  
  ### bootstrapped latest (incremental)
  latest <- data.frame(t(pReserve[(2 * n + 1):(3 * n),]))
  names(latest) <- paste("origin", 1:n, sep = " ")
  
  ### bootstrapped reserves
  reserves <- apply(pReserve[1:n,],2, sum) - apply(pReserve[(n + 1):(2 * n), ], 2, sum)
    
    

  ### OUTPUT section
  estimatedReserve <- c(sum(inputTriangle[last]), sum(object$completed[,n]), 
                        sum(object$completed[,n]) - sum(object$inputTriangle[last]), 
                        reserve[4])
  names(estimatedReserve) <- c("Paid Amount", "   Est.Ultimate", 
                               "   Est.Reserve", "   True Reserve")
  
  BootCoV <- 100 * stats::sd(reserves)/mean(reserves)
  BootVar995 <- stats::quantile(reserves, quantile)/mean(reserves)
  
  pSummary <- c(mean(reserves), stats::sd(reserves), BootCoV, BootVar995)
  userQ <- strsplit(as.character(format(quantile, nsmall = 3)), "\\.")[[1]][2]
  names(pSummary) <- c("Boot.Mean", "   Std.Er.", "   BootCov%", 
                       paste("   BootVar.", userQ, sep = ""))
  
 
  output <- list()
  output$eSummary <- estimatedReserve  ## PARALLAX/REACT/MACRAME
  output$pSummary <- pSummary
  
  output$method <- paste("Permutation bootstrap (", method, " method)", sep = "")
  
  output$pReserves <- as.numeric(reserves)
  output$pUltimates <- ultimates
  output$pLatest <- latest
  output$pLatestCum <- latestCum
  
  if (all(!is.na(trueComplete))){
    output$tUltimate <- trueComplete[,n]
  } else {
    output$tUltimate <- NA
  }
  output$tLatest <- rev(ChainLadder::cum2incr(inputTriangle)[last])
  
  output$inputTriangle <- inputTriangle
  output$completed <- object$completed
  output$trueComplete <- trueComplete
  
  time <- c(Sys.info()["sysname"], Sys.info()["machine"], paste("B = ", round(B,0), sep = ""), paste("n = ", n, sep = ""), 
            paste(round(endTime, 2), " sec.", sep = ""))
  names(time) <- c("OS System", "Architecture", "Permutation number", "   Triangle dimension", "   Run time")
  output$info <- time
  
  class(output) <- c('permutedReserve', 'list')
  return(output)
} ### end of permuteReserve() function

