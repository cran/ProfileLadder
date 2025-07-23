#' Print Objects of the S3 Class \code{mcSetup}
#'
#' Function to organize and print the output provided by the function \code{incrExplor()}
#'
#' @param x an object of the class \code{mcSetup} resulting from a call  of the 
#' \code{incrExplor()} function
#' @param ... further arguments passed to \code{print}
#' 
#' @return displays information resulting from a call of the \code{incrExplor()} 
#' function
#' 
#' @seealso [incrExplor()], [mcReserve()], [mcBreaks()], [mcStates()]
#' 
#' @examples
#' data(CameronMutual)
#' x <- incrExplor(CameronMutual)
#' print(x) 
#' 
#' @export
#' @method print mcSetup
print.mcSetup <- function(x, ...){
  message("Data-driven (default) setting of the Markov Chain in MACRAME")
  cat("MC States: ")
  cat(x$MarkovChain$states)
  cat("\n\n")
  
  breaks <- x$MarkovChain$breaks
  intervals <- paste0("[", breaks[-length(breaks)], ", ",  breaks[-1], ")")
  cat("Corresponding bins for the run-off triangle increments\n")
  print(intervals)
  cat("\n")
  
  if (!is.null(x$userDefined)){### print also user modified setting
    message("User-modified MC setting")
    cat("MC States: ")
    cat(x$userDefined$states)
    cat("\n\n")
    
    breaks <- x$userDefined$breaks
    intervals <- paste0("[", breaks[-length(breaks)], ", ",  breaks[-1], ")")
    cat("Corresponding bins for the run-off triangle increments\n")
    print(intervals)
    cat("\n")
    
    cat(paste("Development periods (run-off triangle columns) not considered: ", 
        paste(x$userDefined$outColumns, collapse = ", "), "\n", sep = ""))
    cat(paste("Method selected to summarize the increments within each bin: ", x$userDefined$method, "\n", sep = ""))
  }
}