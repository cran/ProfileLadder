#' Print Objects of the S3 Class \code{profilePredict}
#'
#' Function to organize and print the 1-year prediction based on the PARALLAX and REACT
#' algorithm  (\code{parallelReserve()}) or the MACRAME algorithm (\code{mcReserve()})
#'
#' @param x an object of the class \code{profileLadder} resulting from a call 
#' of one of the functions \code{parallelReserve()} or \code{mcReserve}
#' @param fancy.print logical to indicate whether a fancy run-off triangle should be 
#' printed in the output (DEFAULT) or a standard print option should be used instead. 
#' Note that that the fancy print option uses, by DEFAULT, zero number of decimal digits.
#' Specific colors for the fancy print option and the number of decimal points 
#' to be used can be set by the function \code{set.fancy.print()}. 
#' The fancy print option can be supressed by \code{options(profileLadder.fancy = FALSE)}.
#' @param ... further arguments passed to \code{print()}
#' 
#' @return displays information resulting from a call of the \code{predict()} applied to the 
#' output of the \code{parallelReserve()} or \code{mcReserve()} function---the one year ahead 
#' prediction in the run-off triagle
#' 
#' @seealso [parallelReserve()], [mcReserve()], [set.fancy.print()]
#' 
#' @examples
#' data(CameronMutual)
#' predict(parallelReserve(CameronMutual))
#' 
#' @export
#' @method print profilePredict
print.profilePredict <- function(x, fancy.print = getOption("profileLadder.fancy", TRUE), ...){

  ### global colors for fancy print
  colors <- getOption("profileLadder.colors")
  col.known     <- colors$col.known
  col.predicted <- colors$col.predicted
  col.unknown   <- colors$col.unknown
  col.info      <- colors$col.info
  digits.info   <- colors$display.digits
    
  ### FANCY print option (auxiliary function)
  print_color <- function(triangle, digits = digits.info, unknown = FALSE){
    triangle <- as.matrix(triangle)
    n <- nrow(triangle)
    m <- ncol(triangle)
    
    # Convert all values to character with fixed decimals
    char_mat <- matrix(NA_character_, n, m)
    for (i in 1:n) {
      for (j in 1:m) {
        val <- triangle[i, j]
        if (is.na(val)) {
          char_mat[i, j] <- "NA"
        } else {
          char_mat[i, j] <- formatC(val, format = "f", digits = digits)
        }
      }
    }
    
    # Split integer and fractional parts per column
    int_parts <- matrix(NA_character_, n, m)
    frac_parts <- matrix(NA_character_, n, m)
    for (j in 1:m) {
      for (i in 1:n) {
        x <- char_mat[i, j]
        if (x == "NA") {
          int_parts[i, j] <- "."
          frac_parts[i, j] <- ""
        } else {
          parts <- strsplit(x, "\\.", fixed = FALSE)[[1]]
          int_parts[i, j] <- parts[1]
          frac_parts[i, j] <- ifelse(length(parts) == 2, parts[2], "")
        }
      }
    }
    
    # Determine max width of integer and fraction parts for each column
    max_int_width <- apply(int_parts, 2, function(x) max(nchar(x), na.rm = TRUE))
    max_frac_width <- apply(frac_parts, 2, function(x) max(nchar(x), na.rm = TRUE))
    
    # Print row by row
    for (i in 1:n) {
      for (j in 1:m) {
        # Compose aligned string: right-align int, left-align frac with dot
        if (int_parts[i, j] == "NA") {
          aligned_num <- format("NA", width = max_int_width[j] + 1 + max_frac_width[j], justify = "centre")
        } else {
          int_str <- format(int_parts[i, j], width = max_int_width[j], justify = "right")
          frac_str <- format(frac_parts[i, j], width = max_frac_width[j], justify = "left")
          if (digits.info == 0){
            aligned_num <- paste0(int_str)
          } else {
            aligned_num <- paste0(int_str, ".", frac_str)
          }
        }
        
        val <- triangle[i, j]
        # Coloring logic:
        # If lower triangle or NA: violet (magenta)
        # Else upper triangle: dark gray
        if (is.na(val) || j > n - i + 1) {
          if (!unknown){
            cat(col.predicted(aligned_num), "\t")
          } else {
            cat(col.unknown(aligned_num), "\t")
          }
          
        } else {
          cat(col.known(aligned_num), "\t")
        }
      }
      cat("\n")
    }
  }
  


      if (fancy.print){
        print_color(x$extTriangle)
      } else {
        print(x$extTriangle)
      }
 
}



