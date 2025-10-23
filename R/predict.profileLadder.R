#' One-year-ahead prediction based on PARALLAX, REACT, or MACRAME
#'
#' The function provides a one-year-ahead prediction for a given run-off triangle 
#' (i.e., estimating a new running diagonal based on some nonparametric, functional-based 
#' prediction algorithm, PARALLAX, REACT, or MACRAME). 
#'
#' @param object an object of the class \code{profileLadder} which is the output 
#' from the \code{parallelReserve()} function or the \code{mcReserve} function
#' @param ... further arguments passed to \code{predict()}
#' 
#' @returns An object of the class \code{profilePredict} which is a list with 
#' the following elements: 
#' \item{reserve}{The overall predicted amount of the next year payments---the new 
#' running diagonal in an extended (cumulative) run-off triangle minus the last 
#' observed diagonal (in the cumulative triangle again)}
#' \item{methods}{Provides the information about the underlying nonparametric 
#' (functional-based) method that is used  for the 1-year-ahead prediction (PRALLAX, 
#' REACT, or MACRAME)}
#' \item{newDiagonal}{Numeric vector representing the prediction of the new running 
#' diagonal (ordered in a way that first value corresponds to the payment for the last 
#' occurrence period (typically the largest amount) and the last value corresponds 
#' with the next-year payment for the first occurrence periods---typically the smallest
#' amount)}
#' \item{extTriangle}{The extended run-off triangle---the R object of the class 
#' \code{triangle} that also contains the new predicted diagonal---the 1-year-ahead 
#' prediction}
#' 
#' 
#' @seealso [parallelReserve()], [mcReserve()]
#' 
#' @examples
#' ## full run-off triangle complettion
#' parallelReserve(CameronMutual)
#' 
#' ## one-year-ahead prediction based on PARALLAX
#' predict(parallelReserve(CameronMutual))
#' 
#' @export
#' @method predict profileLadder
predict.profileLadder <- function(object, ...){
  ### input data checks
  if (!inherits(object, "profileLadder")){
    stop("The input object must be of a class 'profileLadder'")}
  if (inherits(object, "profileLadder") && all(is.na(object$FullTriangle))){
    stop("The input object must be an output of 'parallelReserve()' or 'mcReserve()'")}
  
  ### parallelReserve or mcReserve
  n <- nrow(object$FullTriangle) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1)
  method <- unlist(strsplit(object$method, " "))[1]
  firstColumn <- object$FullTriangle[,1]
  
  if (method != "MACRAME"){### PARALLAX or REACT
    if (object$FullTriangle[1,(n-1)] == object$FullTriangle[1,n]){### developed profile
      newColumn <- c(object$FullTriangle[1,n], rep(NA, n - 1))
    } else {### two different options to define the prediction in the first row
      newColumn <- c(max(object$FullTriangle[1:2,n]), rep(NA, n - 1))
    }
    newDiagonal <- cbind(object$FullTriangle[,1], observed(cbind(object$FullTriangle[,2:n], newColumn)))
  } else {### MACRAME
    incrTriangle <- ChainLadder::cum2incr(object$FullTriangle)
    if (object$FullTriangle[1,(n-1)] == object$FullTriangle[1,n]){### developed profile (i.e. zero increment)
      newColumn <- c(0, rep(NA, n - 1))
    } else {
      P <- object$MarkovChain$transitionMatrix
      states <- object$MarkovChain$states
      breaks <- object$MarkovChain$breaks
      
      inState <- rep(0, length(states))
      inState[which(levels(cut(incrTriangle[1,n], breaks=breaks, right = FALSE)) == cut(incrTriangle[1,n], breaks=breaks, right = FALSE))] <- 1
      newColumn <- c(inState %*% P %*% states, rep(NA, n - 1))
    }
    newDiagonal <- ChainLadder::incr2cum(cbind(incrTriangle[,1], observed(cbind(incrTriangle[,2:n], newColumn), cum = FALSE)))
  }
  colnames(newDiagonal) <- as.character(1:ncol(newDiagonal))
  
  ### preparing the output
  output <- list()
  output$reserve <- sum(newDiagonal[,-1][last] - newDiagonal[,-(n + 1)][last]) ### next year payments
  output$method <- paste(method, "-based 1-year-ahead prediction of the new running diagonal", sep = "")
  output$newDiagonal <- newDiagonal[,-1][last]
  output$extTriangle <- ChainLadder::as.triangle(newDiagonal)     
  
  class(output) <- c('profilePredict', 'list')
  return(output)
}


