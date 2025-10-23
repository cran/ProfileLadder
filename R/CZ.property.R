#' Property damages caused by fires, floods, windstorms, and explosions 
#'
#' Illustrative dataset provided by the major insurance company in the Czech Republic. 
#' The dataset contains large risks insurance portfolio in the Czech Republic and it 
#' covers usually commercial business and allows mainly for damages on varios property 
#' types caused by fire, flood, windstorm, explosion, and others.  
#' 
#' The data are structured in the the list object \code{CZ.property} with two 
#' elements within the list---the run-off triangle \code{GrossPaid} containing 
#' cumulative amounts of gross paid claims and the run-off triangle \code{RBNS} 
#' with the cummulative amounts of the RNNS reserve. The amounts of given in thousands
#' of the Czech crows (CZK).  
#' 
#' @name CZ.property
#' 
#' @docType data
#' @usage data(CZ.property)
#'
#' @format ## CZ.property
#' Two run-off triangles (objects of the class \code{triangle}) with the dimensions 
#' \code{17x17} with \code{17} origin years (from the period 2003 -- 2019) and \code{17} 
#' development periods (years again).
#' 
#' \describe{
#'   \item{origin}{matrix rows with the occurrence year (origin)}
#'   \item{dev}{matrix columns with the development period (development)}
#' }
#' @source An anonymous major insurance company in the Czech Republic.
NULL


