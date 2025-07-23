#' Print Objects of the S3 Class \code{permutedReserve}
#'
#' Function to organize and print the output provided by the permutation bootstrap
#' method implemented in the function  \code{permuteReserve()} 
#'
#' @param x an object of the class \code{permutedReserve} resulting from a call 
#' of the functions \code{permuteReserve()}
#' 
#' @param ... further arguments passed to \code{print}
#' 
#' @return Displays information about the estimated reserve (by one of the 
#' estimation algorithms -- PARALLAX, REACT, or MACRAME) and the overall reserve 
#' distribution resulting from a call of the \code{permuteReserve()} function 
#' 
#' @seealso [permuteReserve()]
#' 
#' @examples
#' ## reserve point prediction by the PARALLAX method
#' output <- parallelReserve(CameronMutual)
#' 
#' ## reserve distribution prediction by the permutation bootstrap
#' x <- permuteReserve(output, B = 100)
#' 
#' ## summary of the results
#' print(x) 
#' 
#' @export
#' @method print permutedReserve
print.permutedReserve <- function(x, ...){
  ### estimation methods
  methods <- c("PARALLAX", "REACT", "MACRAME")
  method <- methods[methods %in% unlist(regmatches(x$method, 
                    gregexpr(paste(methods, collapse = "|"), x$method)))]
  
  message(paste(method, " based reserving", sep = ""))
  print(round(x$eSummary, 3))
  
  B <- unlist(strsplit(x$info[3], " = "))[2]
  message(paste("Permutation bootstrap (B = ", B, ")", sep = ""))
  print(round(x$pSummary, 3))
}