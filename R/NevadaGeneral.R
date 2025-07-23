#' Nevada General Insurance Company Data 
#'
#' An illustrative dataset---a matrix (of the dimensions 10\code{x}10) with ten 
#' completed years of claims payment developments of the Nevada General Insurance 
#' company from the period 1988 -- 1997.  However, the data matrix only contains 
#' four non-zero origin/occurrence years (from the period 1994 -- 1997) all being 
#' fully developed for ten consecutive development periods/years (in columns). 
#' The remaining matrix rows are all zeros. The resulting run-off triangle 
#' (the upper-left triangular part of the data matrix) is, therefore, sparse and 
#' very uninformative. 
#' 
#' In practice, the reserve for such sparse run-off triangles is not estimated 
#' by any stochastic model but, instead, an expert judgement is used to declare 
#' the reserve. Nevertheless, the nonparametric estimation performed by PARALLAX, 
#' REACT, or MACRAME can still provide resonable reserve estimates
#' 
#' @name NevadaGeneral
#' 
#' @docType data
#' @usage data(NevadaGeneral)
#'
#' @format ## NevadaGeneral
#' A simple \code{10x10} matrix of a class \code{triangle} with ten origin years 
#' (rows) each being fully developed within ten consecutive development 
#' periods (columns). However, only for development profiles are nonzero and 
#' standard (parametric) reserving techniques (e.g. the ODP model) are not 
#' applicable
#' 
#' \describe{
#'   \item{origin}{matrix rows with the occurrence year (origin)}
#'   \item{dev}{matrix columns with the development period (development)}
#' }
#' @source \url{https://www.casact.org/publications-research/research/research-resources}\cr 
#' (PP Auto Data Set, NAIC group code: 10007)
#' 
#' @references Meyers, G. G. and P. Shi (2011). Loss reserving data pulled from 
#' NAIC Schedule P. Available from 
#' \url{https://www.casact.org/publications-research/research/research-resources}
NULL


