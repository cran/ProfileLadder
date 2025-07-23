#' Access Markov Chain Breaks for Run-Off Triangle Increments
#'
#' Retrieves the Markov chain components from the \code{profileLadder} object returned 
#' from the function \code{mcReserve()} or the \code{mcSetup} object returned 
#' from the function \code{incrExplor()}. In particular, the function returns 
#' the set of breaks used to define the bins for the incremental run-off triangle
#' increments. 
#'
#' @param object An object of class \code{profileLadder}.
#' 
#' @return The vector of the break points that define the set bins for the run-off 
#' triangle increments. 
#' 
#' @seealso [mcReserve()], [mcStates()], [mcTrans()]
#' 
#' @examples
#' ## MACRAME reserve prediction with the DEFAULT Markov chain setup 
#' output <- mcReserve(CameronMutual)
#' 
#' ## Extracting the corresponding break points
#' mcBreaks(output)
#' 
#' @export
mcBreaks <- function(object) {
  UseMethod("mcBreaks")
}

#' @export
mcBreaks.profileLadder <- function(object){
  ### input data checks
  if (!inherits(object, "profileLadder")){stop("The input object must be of class 'profileLadder'")}
  if (!grepl("MACRAME", object$method)){stop("The input object must be the output of the MACRAME algorithm -- the function mcReserve().\n There is no underlying Markov Chain in PARALLAX or REACT algorithm -- the function parallelReserve().")}

  return(object$MarkovChain$breaks)
}

#' @export
mcBreaks.mcSetup <- function(object) {
  if (!inherits(object, "mcSetup")) stop("The input object must be of class 'mcSetup' which is an output from the function incrExplor()")
  if (is.null(object$userDefined)){### data-driven (default) breaks are returned
    return(object$MarkovChain$breaks)
  } else {### user-defined breaks are provided
    return(object$userDefined$breaks)
  }
}