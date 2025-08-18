#' Access Markov Chain Breaks for Run-Off Triangle Increments
#'
#' Retrieves the Markov chain components from the output of the \code{incrExplor()}
#' function or the \code{mcReserve()} function. In particular, the function returns 
#' the set of breaks used to define the bins for the incremental run-off triangle
#' increments. 
#'
#' @param object An object of the class \code{profileLadder} returned from the function 
#' \code{mcReserve()} or an object of the class \code{mcSetup} returned from the 
#' function \code{incrExplor()}.
#' 
#' @return The vector of the break points that define the set of bins for the run-off 
#' triangle increments. 
#' 
#' @seealso [mcReserve()], [incrExplor()], [mcStates()], [mcTrans()]
#' 
#' @examples
#' ## DEFAULT performance of the incrExplor() function and the MACRAME algorithm
#' output1 <- incrExplor(CameronMutual)
#' output2 <- mcReserve(CameronMutual)
#' 
#' ## Extracting the DEFAULT break points from both outputs
#' mcBreaks(output1)
#' mcBreaks(output2)
#' 
#' ## Extracting the corresponding break points for 4 Markov states
#' mcBreaks(incrExplor(CameronMutual, states = 4))
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