#' First Occurrences of Covid-19 Cases in the Czech Republic 
#'
#' An illustrative dataset---a matrix (of the dimensions 4\code{x}8) with the 
#' cumulative counts of the first reported cases of the Covid-19 pandemic 
#' in the Czech Republic. Four cohorts are defined by the Czech counties where 
#' the first reported case occurred during the period March 1st -- 7th, 2020 
#' (e.g., Prague, Vsetín, or Dečín), March 8th -- March 14th (e.g, Brno, České 
#' Budějovice, Kladno, Mladá Boleslav, Plzeň), March 15th -- March 21st (e.g., 
#' Chomutov, Český Krumlov, Písek, Tábor), and, finally, during the week in 
#' March 22nd -- March 28th, 2020 (e.g., Jindřichův Hradec, Klatovy, Teplice). 
#' 
#' The cumulative reported cases are provided in the table for 8 consecutive 
#' development periods (where each period represents seven consecutive days) 
#' starting in March 1st, 2020. 
#' 
#' @name covid19CZ
#' 
#' @docType data
#' @usage data(covid19CZ)
#'
#' @format ## covid19CZ
#' A simple \code{4x8} matrix of a class \code{triangle} with four cohorts (rows) 
#' consecutively observed for \code{8} weeks (starting in March 1st 2020 with the 
#' first case in the first cohort (first row) reported in March 1st)  
#' 
#' @source Institute of Health Information and Statistics of the Czech Republic 
#' \url{https://www.uzis.cz:443/}
NULL


