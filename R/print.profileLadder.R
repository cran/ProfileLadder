#' Print Objects of the S3 Class \code{profileLadder}
#'
#' Function to organize and print the outputs provided by the function 
#' \code{parallelReserve()} and the function \code{mcReserve}
#'
#' @param x an object of the class \code{profileLadder} resulting from a call 
#' of one of the functions \code{parallelReserve()}, \code{mcReserve}, or
#' \code{as.profileLadder()}
#' @param ... further arguments passed to \code{print}
#' 
#' @return displays information resulting from a call of the \code{parallelReserve()} 
#' function or the \code{mcReserve} function
#' 
#' @seealso [as.profileLadder()], [parallelReserve()], [mcReserve()]
#' 
#' @examples
#' data(CameronMutual)
#' x <- as.profileLadder(CameronMutual)
#' print(x) 
#' 
#' @export
#' @method print profileLadder
print.profileLadder <- function(x, ...){
  print(x$reserve)
  message(x$method)
  if (all(is.na(x$completed))){### no imputed profiles available
    if (all(is.na(x$trueComplete))){### no full/true triangle available
      print(x$inputTriangle)
    } else {
      print(x$trueComplete)
    }
  } else {
    print(x$completed)
  }
  invisible(x)
}