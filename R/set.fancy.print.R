#' Set Custom Color Styles for `profileLadder` Output
#' 
#' Function to set user-modified color layout for the run-off triangle visualization
#' and the overall output presentation
#'
#' @param color.known Color (e.g., a hexadecimal code) for the run-off triangle part (the upper-left 
#' triangle)
#' @param color.predicted Color (e.g., a hexadecimal code) for the predicted part of the run-off triangle (the bottom-right 
#' triangle)
#' @param color.unknown Color (e.g., a hexadecimal code) for the 'unknown' future (the bottom-right triangle 
#' which is typically not avalaialble for insurance practice but is often provided for retrospective 
#' evaluations)
#' @param color.info Color (e.g., a hexadecimal code) for the information messages in the outputs 
#' of the prediction functions \code{parallelReserve()}, \code{mcReserve()}, and \code{permuteReserve()}
#' 
#' @param display.digits the number of digits to be used when using the fancy print option 
#' for run-off triangles. No decimal point are used by DEFAULT (i.e., \code{display.digits = 0} by DEFAULT)
#'
#' @return Sets the  user-defined option for fancy print color styles
#' 
#' @seealso [print.profileLadder()]
#' 
#' @examples
#' ## fancy print option for the run-off triangle 
#' print(as.profileLadder(observed(CameronMutual)), fancy.print = TRUE)
#' 
#' ## fancy print option for the run-off triangle with two decimals
#' set.fancy.print(display.digits = 2)
#' print(as.profileLadder(observed(CameronMutual)))
#' 
#' ## standard print option for the run-off triangle
#' print(as.profileLadder(observed(CameronMutual)), fancy.print = FALSE)
#' 
#' ## PARALLAX based run-off triangle completion (fancy print)
#' options(profileLadder.fancy = TRUE)
#' parallelReserve(CameronMutual)
#' 
#' ## PARALLAX based run-off triangle completion (standard print)
#' options(profileLadder.fancy = FALSE)
#' parallelReserve(CameronMutual)
#' 
#' @export
set.fancy.print <- function(color.known = "#333333",
                            color.predicted = "#CC00CC",
                            color.unknown = "#999999", 
                            color.info = "#CC00CC",
                            display.digits = 0){
  styles <- list(
    col.known     = crayon::make_style(color.known),
    col.predicted = crayon::make_style(color.predicted),
    col.unknown   = crayon::make_style(color.unknown),
    col.info = crayon::make_style(color.info),
    display.digits = display.digits
  )
  options(profileLadder.colors = styles)
  invisible(styles)
}