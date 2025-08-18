#' Print Objects of the S3 Class \code{profileLadder}
#'
#' Function to organize and print the outputs provided by the function 
#' \code{parallelReserve()} and the function \code{mcReserve}
#'
#' @param x an object of the class \code{profileLadder} resulting from a call 
#' of one of the functions \code{parallelReserve()}, \code{mcReserve}, or
#' \code{as.profileLadder()}
#' @param fancy.print logical to indicate whether facty run-off triangle should be 
#' printed or standard output is used instead. The default choice is \code{TRUE}.
#' Fancy printng output can be supressed by \code{options(profileLadder.fancy = FALSE)}. 
#' @param ... further arguments passed to \code{print}
#' 
#' @return displays information resulting from a call of the \code{parallelReserve()} 
#' function or the \code{mcReserve} function
#' 
#' @seealso [as.profileLadder()], [parallelReserve()], [mcReserve()]
#' 
#' @examples
#' data(CameronMutual)
#' ## full run-off triangle printed with the fancy mode
#' x <- as.profileLadder(CameronMutual)
#' print(x) 
#' 
#' ## run-off triangle with unobserved future payments 
#' x <- as.profileLadder(observed(CameronMutual))
#' print(x) 
#' 
#' ## the same run-off triangle using a standard printing method 
#' options(profileLadder.fancy = FALSE)
#' print(x)
#' 
#' @export
#' @method print profileLadder
print.profileLadder <- function(x, fancy.print = getOption("profileLadder.fancy", TRUE), ...){
  cat(paste(unlist(strsplit(x$method, " "))[1], "Reserving \n", sep = " "))
  print(x$reserve[c(3,2,1,4)])
  cat("\n")
  
  ### global colors for fancy print
  colors <- getOption("profileLadder.colors")
  col.known     <- colors$col.known
  col.predicted <- colors$col.predicted
  col.unknown   <- colors$col.unknown
  col.info      <- colors$col.info
  
  ### FANCY print option (auxiliary function)
  print_color <- function(triangle, digits = 0, unknown = FALSE){
    triangle <- as.matrix(triangle)
    n <- nrow(triangle)
    
    

    # Convert all values to character with fixed decimals
    char_mat <- matrix(NA_character_, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        val <- triangle[i, j]
        if (is.na(val)) {
          char_mat[i, j] <- "NA"
        } else {
          char_mat[i, j] <- formatC(val, format = "f", digits = digits)
        }
      }
    }
    
    # Split integer and fractional parts per column
    int_parts <- matrix(NA_character_, n, n)
    frac_parts <- matrix(NA_character_, n, n)
    for (j in 1:n) {
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
      for (j in 1:n) {
        # Compose aligned string: right-align int, left-align frac with dot
        if (int_parts[i, j] == "NA") {
          aligned_num <- format("NA", width = max_int_width[j] + 1 + max_frac_width[j], justify = "centre")
        } else {
          int_str <- format(int_parts[i, j], width = max_int_width[j], justify = "right")
          frac_str <- format(frac_parts[i, j], width = max_frac_width[j], justify = "left")
          #aligned_num <- paste0(int_str, ".", frac_str)
          aligned_num <- paste0(int_str)
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
  
  message(col.info(x$method))
  if (all(is.na(x$FullTriangle))){### no imputed profiles available
    if (all(is.na(x$trueComplete[!observed(dim(x$Triangle)[1])]))){### no complete (true) triangle available
      if (fancy.print){
        print_color(x$Triangle)
      } else {
        print(x$Triangle)
      }
    } else {
      if (fancy.print){
        print_color(x$trueComplete, unknown = TRUE)
      } else {
        print(x$trueComplete)
      }
    }
  } else {
    if (fancy.print){
      print_color(x$FullTriangle)
    } else {
      print(x$FullTriangle)
    }
  }
  invisible(x)
}



