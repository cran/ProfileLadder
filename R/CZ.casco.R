#' Vehicles damages within a conventional full Casco insurance
#'
#' Illustrative dataset provided by the major insurance company in the Czech Republic. 
#' The dataset contains private and commercial business sold to policy holders based 
#' in the Czech Republic and covers the damages to the vehicles within 
#' a framework of the conventional full Casco insurance.
#' 
#' The data are structured in the the list object \code{CZ.casco} with two 
#' elements---the run-off triangle \code{GrossPaid} containing cumulative amounts 
#' of gross paid claims, and another cummulative run-off triangle \code{RBNS} 
#' providing the amounts of the RNNS reserve. The amounts of given in thousands
#'  of the Czech crows (CZK). 
#' 
#' @name CZ.casco
#' 
#' @docType data
#' @usage data(CZ.casco)
#'
#' @format ## CZ.casco
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


