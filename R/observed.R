#' Observed Run-Off Triangle Layout vs. Predicted (Unknown) Layout
#'
#' Simple layout function to  allow work with fully developed run-off triangles 
#' (i.e., completed squares that also contain typically unknown (future) claim 
#' payments). Such data are not common in actuarial practice but 
#' they are usefull for retrospective analysis and back-testing purposes.  
#'
#' @param object either an integer value to denote the dimension of the run-off 
#' triangle layout (i.e., the value that represents the number of origins (rows) 
#' and also the number of the development periods (columns)). Alternatively, 
#' a cumulative or incremental run-off triangle (i.e, an object of the class 
#' \code{matrix} or \code{triangle}) can be provided in \code{object}. In such 
#' case the output returns the standard run-off triangle with \code{NA} values 
#' in the lower-right triangular part of the matrix (regardless of wheter the
#' input triangle in \code{object} forms a run-off triangle or it is a fully 
#' observed triangle---data matrix)
#' @param cum logical to indicate whether the output run-off triangle is supposed to 
#' be of a cumulative type (\code{DEFAULT}) or an incremental type (\code{cum = FALSE}). 
#' If the input in \code{object} is an integer value (i.e., the dimension of the 
#' run-off triangle)  then the choice of the \code{cum} parameter is ignored
#' @return If \code{object} is an integer value then the function returns 
#' a TRUE/FALSE layout matrix with the \code{TRUE} values for the observed (known) 
#' part of the run-off triangle (the upper-left triangular part of the matrix) 
#' and values \code{FALSE} otherwise.  If \code{object} is a matrix (an object 
#' of the class \code{matrix} or \code{triangle}) then the function returns the 
#' observed (known) part of the run-off triangle with \code{NA} values elsewhere. 
#' Depending on the choice of the \code{cum} parameter, either a cumulative 
#' (\code{DEFAULT}) or incremental (\code{cum = FALSE}) run-off triangle is returned
#' 
#' @seealso [plot.profileLadder()], [parallelReserve()], [mcReserve()]
#' 
#' @examples
#' ## observed/unobserved layout for the run-off triangle with 5 origins
#' print(observed(5))
#' print(!observed(5))
#' 
#' ## fully observed run-off triangle with typically unknown (future) payments
#' ## included in the lower-right triangular part for evaluation purposes
#' data(CameronMutual) ## the full data matrix
#' observed(CameronMutual) ## cummulative run-off triangle
#' observed(CameronMutual, cum = FALSE) ## incremental run-off triangle
#' 
#' @export
observed <- function(object, cum = TRUE){
  if (is.numeric(object) & length(object) == 1){### 
    if (object <= 0 | round(object, 0) != object){
      stop("Incorrect input value of 'object' provided")}

    layout <- matrix(rep(TRUE, object^2), nrow = object)
    layout[row(layout) + col(layout) > object + 1] <- FALSE
    return(layout)
  } else {
    if (inherits(object, "profileLadder")){
      chainLadder <- object$Triangle
      chainLadder[row(chainLadder) + col(chainLadder) > nrow(chainLadder) + 1] <- NA
      return(as.profileLadder(chainLadder))
    } else {
      if (!inherits(object, c("triangle", "matrix"))){
        stop("Incorrect input value of 'object' provided")}
      if (dim(object)[1] != dim(object)[2]){
        stop("The dimensions of the imput object do not correspond.")}
      if (!is.logical(cum)){
        stop("Parameter 'cum' must be logical (TRUE/FALSE)")}
      if (cum){### return cumulative triangle
        if (sum(object[,1]) > sum(object[row(object) + col(object) == nrow(object) + 1])){
          ### object is incremental triangle
          object <- ChainLadder::incr2cum(object)
          object[row(object) + col(object) > nrow(object) + 1] <- NA
          return(object)
        } else {
          ### object is a cumulative triangle 
          object[row(object) + col(object) > nrow(object) + 1] <- NA
          return(object)
        }
      }
      if (!cum){### return incremental triangle
        if (sum(object[,1]) > sum(object[row(object) + col(object) == nrow(object) + 1])){
          ### object is incremental triangle
          object[row(object) + col(object) > nrow(object) + 1] <- NA
          return(object)
        } else {
          ### object is a cumulative triangle 
          object <- ChainLadder::cum2incr(object)
          object[row(object) + col(object) > nrow(object) + 1] <- NA
          return(object)
        }
      }
    }
  }
}  

