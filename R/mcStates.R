#' Access Markov Chain States in the MACRAME Algorithm
#'
#' Retrieves the Markov chain components from a \code{profileLadder} object returned 
#' from the function \code{mcReserve()} -- in particular, the function returns 
#' the vector of states used by the underlying Markov Chain utilized in the MACRAME 
#' reserve prediction.
#'
#' @param object An object of class \code{profileLadder}.
#' 
#' @return The vector of the Markov chain states that are used by the MACRAME algorithm.
#' 
#' @seealso [mcReserve()], [mcBreaks()], [mcTrans()]
#' 
#' @examples
#' ## MACRAME reserve prediction with the DEFAULT Markov chain setup 
#' output <- mcReserve(CameronMutual)
#' 
#' ## Extracting the corresponding break points
#' mcStates(output)
#' 
#' @export
mcStates <- function(object) {
  UseMethod("mcStates")
}

#' @export
mcStates.profileLadder <- function(object){
  ### input data checks
  if (!inherits(object, "profileLadder")){stop("The input object must be of class 'profileLadder'")}
  if (!grepl("MACRAME", object$method)){stop("The input object must be the output of the MACRAME algorithm -- the function mcReserve().\n There is no underlying Markov Chain in PARALLAX or REACT algorithm -- the function parallelReserve().")}

  return(object$MarkovChain$states)
}

#' @export
mcStates.mcSetup <- function(object) {
  if (!inherits(object, "mcSetup")) stop("The input object must be of class 'mcSetup' which is an output from the function incrExplor()")
  if (is.null(object$userDefined)){### data-driven (default) states are returned
    return(object$MarkovChain$states)
  } else {### user-defined states are provided
    return(object$userDefined$states)
  }
}