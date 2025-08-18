#' Parallel Based Development Profile Reserve 
#'
#' The function takes a cumulative (or incremental) run-off triangle (partially 
#' or completely observed) and returns the reserve prediction obtained by the 
#' PARALLAX or REACT algorithm (see Maciak, Mizera, and Pešta (2022) for more 
#' details). If a full data matrix is provided as the input then the algorithms  
#' uses only on the relevant part of the data---the run-off triangle only (i.e., the 
#' top-left triangular part of the matrix) but standard incremental residuals 
#' (true incremental payments minus predicted increments) are returned for 
#' retrospective validation purposes (if \code{residuals = TRUE}). If the run-off 
#' triangle is provided only,then the algorithm caclulates so-called back-fitted 
#' (incremental) residuals instead (see Maciak, Mizera, and Pešta (2022) for details).
#'
#' @param chainLadder cumulative or incremental run-off triangle (the triangle 
#' must be of the class \code{triangle} or \code{matrix}) in terms of a square 
#' matrix (i.e., a fully observed run-off triangle) or a standard run-off triangle
#' instead (i.e, the top-left triangular part of the matrix
#' @param method prediction method to be used: PARALLAX (DEFAULT
#'  \code{method = "parallax"}) or REACT (\code{method = "react"}) 
#' @param cum logical (\code{TRUE} for a cumulative triangle and \code{FALSE} for 
#' an incremental triangle)
#' @param residuals logical to indicate whether  incremental residuals should be 
#' provided or not. If the run-off triangle is complete then the residuals are 
#' obtained in terms of true increments minus the predicted increments. If the 
#' bottom-right part of the triangle is not available the residuals are provided 
#' in terms of the backfitting approach (see Maciak, Mizera, and Pesta (2022) 
#' for further details)
#' 
#' @returns An object of the class \code{profileLadder} which is a list with 
#' the following elements: 
#' \item{reserve}{numeric vector with four values summarizing the reserve: Total 
#' paid amount (i.e., the sum of the last observed diagonal in a cumulative run-off 
#' triangle); Total estimated amount (i.e., the sum of the last column in the 
#' completed cumulative triangle); Estimated reserve (i.e., the sum of the last 
#' column in the completed cumulative triangle minus the sum of the last observed 
#' diagonal in \code{chainLadder}); True reserve---if the completed (true) 
#' \code{chainLadder} is provided in the input (i.e., the sum of the last column 
#' in \code{chainLadder} minus the sum of the last diagonal in \code{chainLadder})}
#' \item{method}{algorithm used for the reserve estimation (PARALLAX or REACT)}
#' \item{Triangle}{the run-off triangle considered as the input for the 
#' underlying estimation algorithm (PARALLAX or REACT)}
#' \item{FullTriangle}{completed functional development profiles (the 
#' lower-right triangular part in \code{completed}) estimated by the 
#' PARALLAX algorithm or the REACT algorithm}
#' \item{trueCompleted}{true (complete) run-off triangle (if available) and 
#' \code{NA} value provided otherwise}
#' \item{residuals}{a triangle with the corresponding residuals (for 
#' \code{residuals = TRUE}). The residuals are either provided in the upper-left 
#' triangle (so-called back-fitted incremental residuals if true completed triangle 
#' is not available) or the residuals are given in the lower-right triangle (i,e., 
#' standard incremental residuals---if the true completed triangle is given)}
#'  
#' @seealso [mcReserve()], [permuteReserve()], [summary.profileLadder()]
#'  
#' @examples
#' ## run-off (upper-left) triangle with NA values (bottom-right part)
#' data(MW2014, package = "ChainLadder")
#' print(MW2014) 
#' parallelReserve(MW2014, residuals = TRUE)
#' 
#' ## completed run-off triangle with 'unknown' truth (lower-bottom part)  
#' ## for the estimation purposes only the upper-left triangle is used 
#' data(CameronMutual)
#' parallelReserve(CameronMutual, residuals = TRUE)
#' 
#' ## the previous output is identical (in term of the reserve prediction) 
#' ## but back-fitted residuals are provided in the output instead 
#' print(observed(CameronMutual))
#' parallelReserve(observed(CameronMutual), residuals = TRUE)
#' 
#' 
#' @import raw
#' 
#' @references Maciak, M., Mizera, I., and Pešta, M. (2022). Functional Profile 
#' Techniques for Claims Reserving. ASTIN Bulletin, 52(2), 449-482. DOI:10.1017/asb.2022.4
#'
#' @export
parallelReserve <- function(chainLadder, method = "parallax", cum = TRUE, residuals = FALSE){
  ### input data checks
  if (method != "parallax" & method != "react"){
    stop("Not a correct prediction method = c('parallel', 'react') selected.")}
  if (!inherits(chainLadder, c("triangle", "matrix"))){
    stop("The input data must be of class 'triangle' or 'matrix'.")}
  if (dim(chainLadder)[1] != dim(chainLadder)[2]){
    stop("The input data do not form a run-off triangle (square matrix).")}
  
  
  n <- nrow(chainLadder) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout 
  
  if (!is.numeric(chainLadder[observed])){stop("The input values are not numeric.")}
  
  if (cum == TRUE){
    if (sum(chainLadder[last]) < sum(chainLadder[,1])){
      warning("The input run-off triangle seems to be not of the cumultative type!")}
  } else {
    if (sum(chainLadder[last]) > sum(chainLadder[,1])){
      warning("The input run-off triangle seems to be of a cumulative type!")}
    chainLadder <- ChainLadder::incr2cum(chainLadder)
  }
  
  if (sum(is.na(chainLadder[observed])) > 0){
    stop("The run-off triangle is not fully observed (missing values).")}
  
  if (sum(as.numeric(is.na(chainLadder[!observed]))) > 0){
    ### backfitted residuals
    backfitting <- TRUE
  }else{### standard residuals
    backfitting <- FALSE
  }
  
  if (method == "parallax"){
    methodType <- "PARALLAX"
    
    ### completing the run-off triangle
    completed <- chainLadder 
    ### auxiiary function for imputing values 
    impute_values <- function(i, j, x){
      diffVector <- x[1:(n - i + 1 - j), i - 1 + j] - x[n - i + 2, i - 1 + j]
      if (x[n - i + 2, i - 1] == 0) {
        return(0)
      } else {
        minIndex <- which.min(abs(diffVector))
        return(x[n - i + 2, i - 1 + j] + mean(x[minIndex, i + j] - x[minIndex, i - 1 + j]))
      }
    }
    
    for (j in 0:(n - 2)){
      index_pairs <- expand.grid(i = 2:(n - j), j = j)
      impute <- mapply(impute_values, index_pairs$i, index_pairs$j, list(x = completed))
      completed[row(completed) + col(completed) == n + 2 + j] <- impute
    }
    
    ### overall predicted reserve 
    reserve <- sum(completed[,n]) - sum(completed[last])
    
    ### backfitting for residuals
    if (residuals == TRUE){
      if (backfitting == TRUE){
        resids <- matrix(rev(as.vector(completed)), nrow = n, byrow = F)
        
        for (j in 0:(n - 2)){
          index_pairs <- expand.grid(i = 2:(n - j), j = j)
          impute <- mapply(impute_values, index_pairs$i, index_pairs$j, list(x = resids))
          resids[row(completed) + col(completed) == n + 2 + j] <- impute
        }
        
        resids <- matrix(rev(as.vector(resids)), nrow = n, byrow = F)
        resids <- ChainLadder::cum2incr(completed) - ChainLadder::cum2incr(resids)
        resids[!observed] <- NA
      } else {
        resids <- ChainLadder::cum2incr(chainLadder) - ChainLadder::cum2incr(completed)
        resids[observed] <- NA
      }}
  } ### end of PARALLAX method 
  
  if (method == "react"){
    methodType <- "REACT"
    
    ### completing the run-off triangle
    completed <- chainLadder
    ### auxiiary function for imputing values
    impute_values <- function(i, j, x){
      diffValue <- x[n - i + 2, i - 1 + j] - x[n - i + 1, i - 1 + j] 
      if (x[n - i + 2, i - 1] == 0) {
        return(0)
      } else {
        return(x[n - i + 1, i + j] + diffValue)
      }
    }
    
    for (j in 0:(n - 2)){
      index_pairs <- expand.grid(i = 2:(n - j), j = j)
      impute <- mapply(impute_values, index_pairs$i, index_pairs$j, list(x = completed))
      completed[row(completed) + col(completed) == n + 2 + j] <- impute
    }
    
    ### overall predicted reserve 
    reserve <- sum(completed[,n]) - sum(completed[last])
    
    
    ### backfitting for residuals
    if (residuals == TRUE){
      if (backfitting == TRUE){
        resids <- matrix(rev(as.vector(completed)), nrow = n, byrow = FALSE)
        
        for (j in 0:(n - 2)){
          index_pairs <- expand.grid(i = 2:(n - j), j = j)
          impute <- mapply(impute_values, index_pairs$i, index_pairs$j, list(x = resids))
          resids[row(completed) + col(completed) == n + 2 + j] <- impute
        }
        
        resids <- matrix(rev(as.vector(resids)), nrow = n, byrow = FALSE)
        resids <- ChainLadder::cum2incr(completed) - ChainLadder::cum2incr(resids)
        resids[!observed] <- NA
      } else {
        resids <- ChainLadder::cum2incr(chainLadder) - ChainLadder::cum2incr(completed)
        resids[observed] <- NA
      }}
  } ### end of REACT method
  
  ### OUTPUT section
  reserveOutput <- c(sum(chainLadder[last]), sum(completed[,n]), 
                     sum(completed[,n]) - sum(completed[last]), 
                     sum(chainLadder[,n]) - sum(chainLadder[last]))
  names(reserveOutput) <- c("Paid Amount", "   Estimated Ultimate", 
                            "   Estimated Reserve", "   True Reserve")
  
  inputTriangle <- chainLadder
  inputTriangle[!observed] <- NA
  
  output <- list()
  output$reserve <- reserveOutput
  output$method <- paste(methodType, " method (functional profile completion)", sep = "")
  output$Triangle <- ChainLadder::as.triangle(inputTriangle)
  output$FullTriangle <- ChainLadder::as.triangle(completed)
  if (all(is.na(chainLadder[!observed(n)]))){
    output$trueComplete <- NA
  } else {
    output$trueComplete <-  ChainLadder::as.triangle(chainLadder)
  }
  if (residuals == TRUE){output$residuals <- resids} else {output$residuals <- NULL}
  
  class(output) <- c('profileLadder', 'list')
  return(output)
} ### end of parallelReserve function