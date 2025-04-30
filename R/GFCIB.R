#' Guarantee Fund of the Czech Insurers' Bureau data 
#'
#' Illustrative datasets provided by the  Guarantee Fund of the Czech Insurers' 
#' Bureau (GFCIB) for the mandatory car insurance in the Czech Republic. 
#' The quarterly based payments are aggregated in four run-off triangles with the 
#' paid amounts for four separate lines of business: bodily injury, material damage, 
#' technical provision, and annuities. 
#' 
#' The data are structured in the the list object \code{GCCIB} with four 
#' elements---one for each line of business: \code{\$bodilyInjury}, 
#' \code{\$materialDamage}, \code{\$provisions}, and \code{\$annuity}. 
#' The run-off triangles are all aggregated over the period from the first quartal
#' of 2008 (\code{Q1}) till the last quartal of 2022 (\code{Q4}). 
#' 
#' 
#' @name GFCIB
#' 
#' @docType data
#' @usage data(GFCIB)
#'
#' @format ## GFCIB
#' Four data matrices of the dimensions \code{60x60}  of a class \code{triangle} 
#' with \code{15} origin years (provided on a quarterly basis in individual rows) 
#' and \code{60} development periods/quartals (columns)
#' 
#' \describe{
#'   \item{origin}{matrix rows with the occurrence quartal (origin)}
#'   \item{dev}{matrix columns with the development period (development)}
#' }
#' @source The Czech Insurers’ Bureau \url{https://www.ckp.cz} 
NULL


