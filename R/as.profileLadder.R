#' S3 Method Class \code{profileLadder}
#'
#' A function to make the work with the functional development profiles within 
#' run-off triangles more easy and straightforward---particularly when vizualizing 
#' the functional profiles (observed, completed, or both simultaneously) in a single plot
#'
#' @param x an object of the class \code{matrix} or \code{triangle}
#' 
#' @return an object of the class \code{profileLadder} which is a list with the 
#' following elements: 
#' \item{reserve}{basic summary of the run-off triangle and the predicted/true 
#' reserve (if it is available otherwise \code{NA} values are provided instead)}
#' \item{method}{type of the printed triangle (either a run-off triangle itself 
#' if no prediction method is applied or the completed triangle where the missing 
#' fragments are imputed by one of the algorithm, PARALLAX, REACT, or MACRAME)}
#' \item{Triangle}{input (triangular shaped)  run-off triangle}
#' \item{FullTriangle}{completed development profiles imputed by using one of the 
#' estimation algorithm (i.e., PARALLAX, REACT, or MACRAME)---if applied---value 
#' \code{NA} provided otherwise}
#' \item{trueComplete}{true fully developmed profiles of the run-off triangle 
#' (if available for back-testing purposes) or  \code{NA} returned otherwise}
#' 
#' @seealso [parallelReserve()], [mcReserve()], [permuteReserve()], 
#' [plot.profileLadder()]
#' 
#' @examples
#' data(CameronMutual)
#' print(CameronMutual) 
#' 
#' x <- as.profileLadder(CameronMutual)
#' 
#' print(x)
#' plot(x)
#' 
#' @rdname as.profileLadder
#' @export
as.profileLadder <- function(x){
  if (inherits(x, "triangle") || inherits(x, "matrix")){### matrix/triangle
    if (dim(x)[1] != dim(x)[2]){stop("The object 'x' dimensions do not correspond")}
    
    n <- nrow(x) ### number of occurrence/development years
    last <- n * (1:n) - 0:(n - 1) ### last running diagonal
    observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ## NA layout
    
    ### reserve information
    reserveOutput <- c(sum(x[last]), NA, NA, sum(x[,n]) - sum(x[last]))
    names(reserveOutput) <- c("Paid Amount", "   Estimated Ultimate", 
                              "   Estimated Reserve", "   True Reserve")
    
    output <- list()
    output$reserve <- reserveOutput
    if (all(is.na(x[!observed])) == F){### completed triangle
      output$method <- "Run-off triangle (complete/fully observed)"
    } else {
      output$method <- "Run-off triangle" 
    }
    
    output$Triangle <- ChainLadder::as.triangle(observed(x))
    output$FullTriangle <- NA
    
    if (all(is.na(x[!observed(n)]))){### incomplete run-off triangle
      output$trueComplete <- NA
    } else {### true completed triangle
      output$trueComplete <- ChainLadder::as.triangle(x)
    }
    output$residuals <- NULL 
    
    class(output) <- c('profileLadder', 'list')
    return(output)
  } else {
    if (!inherits(x, "profileLadder")){stop("The object 'x' is not compatible with the 'profileLadder' class")}
   return(x)
  }
}