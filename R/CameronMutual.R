#' Cameron Mutual Insurance Company data 
#'
#' An illustrative dataset---a matrix (of the dimensions 10\code{x}10) with ten 
#' completed years of claims payment developments of the Cameron Mutual Insurance 
#' company from the period 1988 -- 1997.  The data matrix contains ten 
#' origin/occurrence years (in rows where the first row represents the incident 
#' year 1988) with ten consecutive development periods/years (in columns).
#' 
#' The run-off triangle (the upper-left triangular part of the data matrix) 
#' contains only positive increments making the triangle suitable for the standard 
#' modelling approach---the over-dispersed Poisson model (GLM regression model). 
#' 
#' In practice, the upper-left triangle (the run-off triangle) is typically 
#' observed (known) while the bottom-right triangular part of the data matrix is 
#' treated as a future payments outcome (an "unknown" truth) that should be 
#' estimated/predicted. The Cameron Mutual Insurance data matrix is fully observed 
#' to allow for some goodness-of-fit evaluations. 
#' 
#' @name CameronMutual
#' 
#' @docType data
#' @usage data(CameronMutual)
#'
#' @format ## CameronMutual
#' A simple \code{10x10} matrix of a class \code{triangle} with ten origin years 
#' (rows) each being fully developed within ten consecutive development 
#' periods/years (columns)
#' 
#' \describe{
#'   \item{origin}{matrix rows with the occurrence year (origin)}
#'   \item{dev}{matrix columns with the development period (development)}
#' }
#' @source \url{https://www.casact.org/publications-research/research/research-resources}\cr
#' (PP Auto Data Set, NAIC group code: 5320)
#' 
#' @references Meyers, G. G. and P. Shi (2011). Loss reserving data pulled 
#' from NAIC Schedule P. Available from 
#' \url{https://www.casact.org/publications-research/research/research-resources}
#' @references Maciak, M., Mizera, I., and Pešta, M. (2022). Functional Profile 
#' Techniques for Claims Reserving. ASTIN Bulletin, 52(2), 449-482. DOI:10.1017/asb.2022.4
#' (Portfolio #1)
NULL


