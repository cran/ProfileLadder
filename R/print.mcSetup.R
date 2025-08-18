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
  
  ## global colors for fancy print 
  colors <- getOption("profileLadder.colors")
  col.info      <- colors$col.info
  
  message(col.info("Data-driven (default) setting of the Markov Chain in MACRAME"))
  cat("MC States: ")
  cat(x$MarkovChain$states)
  cat("\n\n")
  
  
  
  breaks <- x$MarkovChain$breaks
  intervals <- paste0("[", breaks[-length(breaks)], ", ",  breaks[-1], ")")
  cat(col.info("Corresponding bins for the run-off triangle increments\n"))
  print(intervals)
  cat("\n")
  
  if (!is.null(x$userDefined)){### print also user modified setting
    message("User-modified MC setting")
    
    if (x$userDefined$setup == 3){### explicitly defined set of MC states
      cat("Explicitly provided MC states: ")
      cat(x$userDefined$states)
      cat("\n\n")
      
      breaks <- x$userDefined$breaks
      intervals <- paste0("[", breaks[-length(breaks)], ", ",  breaks[-1], ")")
      cat("Corresponding bins for the run-off triangle increments\n")
      print(intervals)
      cat("\n")
      
      cat(paste("Development periods (run-off triangle columns) not considered: ", 
                paste(x$userDefined$outColumns, collapse = ", "), "\n", sep = ""))
      cat(paste("Any method selected to summarize the increments within the bins is ignored\n", sep = ""))
      cat("(the breaks for the bins are determined as midpoints between the given MC states)\n")
    } else {
      if (x$userDefined$method == "DEFAULT (median)"){method <- "medians"}
      if (x$userDefined$method == "mean"){method <- "means"}
      if (x$userDefined$method == "min"){method <- "minima"}
      if (x$userDefined$method == "max"){method <- "maxima"}
      
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
}