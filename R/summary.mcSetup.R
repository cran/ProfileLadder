#' Summary Method for the S3 Class Object \code{mcSetup}
#'
#' The function provides an overall summary of the output from the function 
#' \code{incrExplor()}
#'
#' @param object an object of the class \code{mcSetup} -- the output from the 
#' \code{incrExplor()} function
#' @param ... not used
#' 
#' @return Returns a standard summary table (with basic description characteristics)
#' for raw run-off triangle increments and their standardized (by using the maximum 
#' increment) counterparts. The function also returns the corresponding bins for 
#' the increments and their representations in terms of the Markov chain states. 
#' 
#' @seealso [incrExplor()], [mcBreaks()], [mcStates()]
#' 
#' @examples
#' data(CameronMutual)
#' summary(CameronMutual)
#' 
#' ## default summary output
#' summary(incrExplor(CameronMutual))
#' 
#' ## summary output for user-modified settings
#' summary(incrExplor(CameronMutual, states = 5, method = "mean"))
#' 
#' @export
#' @method summary mcSetup
summary.mcSetup <- function(object, ...){
  
  summaryExt <- function(x, d){return(c(round(summary(x),d), round(stats::sd(x), d)))}
  
  ## global colors for fancy print 
  colors <- getOption("profileLadder.colors")
  col.info <- colors$col.info
  
  message(col.info(paste("Input triangle type: ", object$triangleType, sep = "")))
  if (is.null(object$userDefined)){### data-driven setup summary
    cat("Summary of the increments\n")
    ### raw and std. increments
    incrs <- object$MarkovChain$increments
    maxIncr <- max(incrs, na.rm = TRUE)
    increments <- rbind(summaryExt(incrs, 0), summaryExt(incrs/maxIncr, 4))
    
    row.names(increments) <- c("Raw increments", "Std. increments")
    colnames(increments) <- c("Min", "1st Q.", "Median", "Mean", "3rd Q.", "Max", "Std.Er.")
    
    print(increments)
    cat("\n")
    cat(paste("Total number of increments: ", length(incrs), 
              ",  Total number of unique increments: ", length(unique(incrs)), "\n", sep = ""))
    cat(paste("Number of suspicious increments (using 2\u03C3 rule): ", sum(abs(incrs) > 2 * stats::sd(incrs)), 
              ",  Outliers (3\u03C3 rule): ",  sum(abs(incrs) > 3 * stats::sd(incrs)),"\n", sep = ""))
    cat("\n")
    breaks <- object$MarkovChain$breaks
    intervals <- paste0("[", breaks[-length(breaks)], ", ",  breaks[-1], ")")
    cat("Data-driven bins for the run-off triangle increments\n")
    print(intervals)
    cat("\n")
    cat("Markov Chain states (medians of the increments within each bin)\n")
    print(object$MarkovChain$states)
  } else {### user-defined setup summary
    n <- ncol(object$incrTriangle)
    cat(paste("Summary of the increments (development periods: ", 
        paste(setdiff(1:n, object$userDefined$outColumns), collapse = ","),   ")\n", sep = ""))
    ### raw and std. (user selected) increments
    incrs <- object$userDefined$increments
    maxIncr <- max(incrs, na.rm = TRUE)
    increments <- rbind(summaryExt(incrs, 0), summaryExt(incrs/maxIncr, 4))
    
    row.names(increments) <- c("Raw increments", "Std.increments")
    colnames(increments) <- c("Min", "1st Q.", "Median", "Mean", "3rd Q.", "Max", "Std.Er.")
    
    print(increments)
    cat("\n")
    cat(paste("Total number of increments: ", length(incrs), 
              ",  Total number of unique increments: ", length(unique(incrs)), "\n", sep = ""))
    cat(paste("Number of suspicious increments (using 2\u03C3 rule): ", sum(abs(incrs) > mean(incrs) + 2 * stats::sd(incrs)), 
              ",  Outliers (3\u03C3 rule): ",  sum(abs(incrs) > mean(incrs) + 3 * stats::sd(incrs)),"\n", sep = ""))
    cat("\n")
    breaks <- object$userDefined$breaks
    intervals <- paste0("[", breaks[-length(breaks)], ", ",  breaks[-1], ")")
   
    if (object$userDefined$setup == 3){### explicitly defined set of MC states
      cat("Bins for the increments given by midpoints between the provided MC states\n")
      print(intervals)
      cat("\n")
      cat("Explicitly provided MC states\n")
      print(object$userDefined$states)
    } else {
      if (object$userDefined$method == "DEFAULT (median)"){method <- "medians"}
      if (object$userDefined$method == "mean"){method <- "means"}
      if (object$userDefined$method == "min"){method <- "minima"}
      if (object$userDefined$method == "max"){method <- "maxima"}
      cat("User-defined bins for the selected run-off triangle increments\n")
      print(intervals)
      cat("\n")
      cat(paste("Markov Chain states (", method, " of the increments within each bin)\n", sep = ""))
      print(object$userDefined$states)
    }
  }
}