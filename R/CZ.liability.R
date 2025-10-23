#' Liability insurance of employees in the Czech Republic. 
#'
#' Illustrative dataset provided by the major insurance company in the Czech Republic. 
#' The dataset contains personal liability insurance policies and liability insurance 
#' of employees in the Czech Republic.  
#' 
#' The data are structured in the the list object \code{CZ.liability} with two 
#' elements within the list---the run-off triangle \code{GrossPaid} containing 
#' cumulative amounts of gross paid claims and the run-off triangle \code{RBNS} 
#' with the cummulative amounts of the RNNS reserve. The amounts of given in thousands
#' of the Czech crows (CZK). 
#' 
#' @name CZ.liability
#' 
#' @docType data
#' @usage data(CZ.liability)
#'
#' @format ## CZ.liability
#' Two run-off triangles (objects of the class \code{triangle}) with the dimensions 
#' \code{17x17} with \code{17} origin years (from the period 2003 -- 2019) and \code{17} 
#' development periods (years again).
#' 
#' \describe{
#'   \item{origin}{matrix rows with the occurrence year (origin)}
#'   \item{dev}{matrix columns with the development period (development)}
#' }
#' @source Annonymous major insurance company in the Czech Republic
NULL


