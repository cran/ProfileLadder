#' Access Markov Chain Transition Matrix in the MACRAME Algorithm
#'
#' Retrieves the Markov chain components from a \code{profileLadder} object returned 
#' from the function \code{mcReserve()} -- in particular, the function returns 
#' the matrix of the estimated transition probabilities used by the underlying 
#' Markov Chain to provide the reserve prediction.
#'
#' @param object An object of class \code{profileLadder}.
#' 
#' @return The matrix of the estimated Markov chain transition probabilities
#' 
#' @seealso [mcReserve()], [mcBreaks()], [mcStates()]
#' 
#' @examples
#' ## MACRAME reserve prediction with the DEFAULT Markov chain setup 
#' output <- mcReserve(CameronMutual)
#' 
#' ## Extracting the corresponding break points
#' mcTrans(output)
#' 
#' @export
mcTrans <- function(object) {
  UseMethod("mcTrans")
}

#' @export
mcTrans.profileLadder <- function(object){
  ### input data checks
  if (!inherits(object, "profileLadder")){stop("The input object must be of class 'profileLadder'")}
  if (!grepl("MACRAME", object$method)){stop("The input object must be the output of the MACRAME algorithm -- the function mcReserve().\n There is no underlying Markov Chain in PARALLAX or REACT algorithm -- the function parallelReserve().")}

  return(object$MarkovChain$transitionMatrix)
}