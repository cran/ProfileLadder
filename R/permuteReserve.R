#' Permutation Bootstrap Reserve (PARALLAX, REACT, MACRAME)
#'
#' The function takes a completed run-off triangle provided either by some classical 
#' parametric reserving technique (ODP model, Mack model, or Tweedie model) or some 
#' functional-based alternative (PARALLAX, REACT, or MACRAME) and estimates the overall 
#' reserve distribution in terms of the permutation bootstrap approach proposed 
#' in Maciak, Mizera, and Pešta (2022).
#'
#' @param object an object which is the result of some functional-based reserving 
#' method implemented in the ProfileLadder package (functions \code{parallelReserve()} 
#' and \code{mcReserve()} in particular) or some parametric approach from the 
#' ChainLadder package (specifically the functions \code{chainladder()}, 
#' \code{glmReserve()}, \code{tweedieReserve()}, and \code{MackChainLadder()}). 
#' The following \code{object}'s classes are allowed: \code{profileLadder}, 
#' \code{ChainLadder}, \code{glmReserve}, \code{tweedieReserve}, and \code{MackChainLadder}. 
#' @param B number of the bootstrap permutations to be performed (by DEFAULT the 
#' number of permutations is set to \code{B = 500})
#' @param std logical to indicate whether the run-off triangle should be 
#' standardized by the first column increments (\code{TRUE} by DEFAULT) or not 
#' (\code{std = FALSE}).For more details about the triangle standardization, 
#' see Maciak, Mizera, and Pešta (2022)
#' @param quantile quantile level for the \code{BootVar.} characteristic of the 
#' bootstrapped distribution (the DEFAULT choice \code{quantile = 0.995} is 
#' explicitly required  by the Solvency II principle used by actuaries in practice)  
#' @param adjustMC logical (\code{TRUE} by DEFAULT) to indicate whether the Markov 
#' chain states and the corresponding breaks should be adjusted for every bootstrap 
#' permutation or the same set of Markov states and breaks is used for each permuted
#' run-off triangle (only applies if the input \code{object} is an output 
#' of the MACRAME algorithm---the function \code{mcReserve()})
#' @param outputAll logical to indicate whether whole permuted triangles should 
#' be stored and provided in the output (\code{outputAll = TRUE} set by default), 
#' or just the main summary characteristics are given instead (\code{outputAll = FALSE})
#' @param pb logical (\code{TRUE} by DEFAULT) to indicate whether a progress bar 
#' for bootstrap resampling should be used or not (required the R package \code{pbapply})
#' to be installed
#' 
#' @returns An object of the class \code{permutedReserve} which is a list with  
#' the following elements: 
#' \item{eSummary}{numeric vector with four values summarizing the estimated 
#' reserve: Paid amount (i.e., the sum of the last observed diagonal in the given  
#' cumulative run-off triangle); Estimated ultimate (i.e., the sum of the 
#' last column in the completed cumulative triangle); Estimated reserve (i.e., the 
#' sum of the last column in the completed cumulative triangle minus the sum of 
#' the last observed diagonal); True reserve if a completed (true) run-off triangle 
#' is available}
#' \item{pSummary}{numeric vector with four values summarizing the overall reserve 
#' distribution: \code{Boot.Mean} gives the verage of \code{B} permutation bootstrap 
#' reserves; \code{Std.Er.} provides the corresponding standard error of \code{B} 
#' permutation bootstrap reserves; The value of \code{BootCov\%} stands for  
#' a percentage proportion between the standard error and the average; 
#' Finally, \code{BootVar.995} provides the estimated 0.995 quantile (by DEFAULT)
#' of the bootstrap reserve distribution (for \code{quantile = 0.995} and, otherwise, 
#' it is modified acordingly) given relatively with respect to the permutation 
#' bootstrapped mean reserve}
#' \item{pReserves}{a numeric vector of the length \code{B} with the estimated 
#' (permuted) reserves for each row-permuted run-off triangle in \code{B} 
#' independent Monte Carlo simulation runs}
#' \item{pUltimates}{A matrix of the dimensions \code{B x n} (provided in the output 
#' if \code{outputAll = TRUE}) where \code{n} 
#' stands for the number of the origin/development periods and \code{B} is the number 
#' os simulated ultimate payments --the last column in the completed run-off triangle.}
#' \item{pLatest}{A matrix of the dimensions \code{B x n} (provided in the output 
#' if \code{outputAll = TRUE}) where \code{n} again stands for the number of the 
#' origin/development periods and \code{B} is the number of simulated incremental diagonals}
#' \item{pLatestCum}{A matrix of the dimensions \code{B x n} (provided in the output 
#' if \code{outputAll = TRUE}) where \code{n} is the number of the origin/development periods
#' and \code{B} stands for the number of simulated cumulative diagonals}
#' \item{pFirst}{A matrix of the dimension \code{B x n} (provided in the output 
#' if \code{outputAll = TRUE}) where \code{n} stands for the number of the origin/development 
#' periods and \code{B} is the number of simulated first payment columns 
#' (all columns are identical for \code{std = TRUE})}
#' \item{Triangle}{The input run-off triangle}
#' \item{FullTriangle}{The completed run-off triangle by using one of the PARALLAX, 
#' REACT, or MACRAME estimation method}
#' \item{trueComplete}{The true complete run-off triangle (if available) and \code{NA}
#' value otherwise}
#' \item{info}{a numeric vector summarizing the bootstrap computational efficiency: 
#' In particular, the OS/Architecture type, the number of permutations (\code{B}), 
#' the input run-off triangle dimension (\code{n}) and the computation time needed
#' for the permutation bootstrap calculations}
#' 
#' @seealso [parallelReserve()], [mcReserve()], [plot.permutedReserve()], [summary.permutedReserve()]
#' 
#' @examples
#' ## REACT algorithm and the permutation bootstrap reserve 
#' data(CameronMutual)
#' output <- parallelReserve(CameronMutual, method = "react")
#' summary(permuteReserve(output, B = 100))
#' 
#' ## MACRAME algorithm with a pre-specified number of states using the same MC 
#' ## states and the same break for each permuted run-off triangle
#' output <- mcReserve(CameronMutual, states = 5)
#' plot(permuteReserve(output, B = 100, adjustMC = FALSE))
#' 
#' ## Permutation bootstrap applied to a completed run-off triangle 
#' ## obtained by a parametric Over-dispersed Poisson model (from ChainLadder pkg)
#' library("ChainLadder")
#' output <- permuteReserve(glmReserve(MW2008), B = 100)
#' summary(output, triangle.summary = TRUE)
#' 
#' 
#' 
#' @references Maciak, M., Mizera, I., and Pešta, M. (2022). Functional Profile 
#' Techniques for Claims Reserving. ASTIN Bulletin, 52(2), 449-482. DOI:10.1017/asb.2022.4
#' @references European Parliament and Council (2009). Directive 2009/138/EC of 
#' the European Parliament and of the Council of 25 November 2009 on the taking-up 
#' and pursuit of the business of Insurance and Reinsurance (Solvency II). Official 
#' Journal of the European Union, 1–155.\cr
#' \url{https://data.europa.eu/eli/dir/2009/138/oj}
#' 
#' @export
permuteReserve <- function(object, B = 500, std = TRUE, quantile = 0.995, adjustMC = TRUE, outputAll = TRUE, pb = TRUE){
  ### input data checks
  if (!inherits(object, c("profileLadder", "glmReserve", "tweedieReserve", "MackChainLadder", "ChainLadder"))){
    stop("The input object must be of a class 'profileLadder', 'glmReserve', 'tweedieReserve', 'MackChainLadder' or 'ChainLadder'")}
  if (inherits(object, "profileLadder") && all(is.na(object$FullTriangle))){
    stop("The input object must be a result of 'parallelReserve()' or 'mcReserve()'")}
  if (!is.numeric(B) || length(B) > 1){
    stop("The number of bootstrap permutations 'B' must be a numeric (integer) value")}
  if (round(B, 0) != B || B <= 0){
    stop("The number of boostrap permutations 'B' must be a positive integer value")} 
  if (B < 30){
    warning("The number of bootstrap permutations 'B' is too low")} 
  if (!is.numeric(quantile) || length(quantile) > 1){
    stop("The quantile value must be a single value in interval (0,1)")}
  if (quantile >= 1 || quantile <= 0){
    stop("The quantile value must be in interval (0,1)")}
  
  
  ### reserving method | extract info
  inputTriangle <- object$Triangle
  if (inherits(object, "ChainLadder")){
    completedTriangle <- ChainLadder::predict(object) 
  } else {
    completedTriangle <- object$FullTriangle
  }
  
  
  if (inherits(object, c("glmReserve", "tweedieReserve", "MackChainLadder", "ChainLadder"))){### ChainLadder pkg
    reserve <- NA
    trueComplete <- NA
  }
  if (inherits(object, "glmReserve")){method <- "GLM"}
  if (inherits(object, "tweedieReserve")){
    method <- "Tweedie model"
    varPower <- log(object$family$variance(2))/log(2)
    linkPower <- as.numeric(strsplit(object$family$link, "\\^")[[1]][2])
  }
  if (inherits(object, "MackChainLadder")){method <- "Mack model"}
  
  if (inherits(object, "ChainLadder")){method <- "Triangle model"}
  
  if (inherits(object, "profileLadder")){### ProfileLadder pkg
    reserve <- object$reserve
    trueComplete <- object$trueComplete
    method <- unlist(strsplit(object$method, split = " "))[1]
    if (method == "MACRAME"){
      if (!is.logical(adjustMC)){stop("Parameter 'adjustMC' must be logical")}
      if (!adjustMC){
        states <- object$MarkovChain$states
        breaks <- object$MarkovChain$breaks
        std <- FALSE
      } else {
        states <- NULL
        breaks <- NULL
      }
    }
  }

  completed <- completedTriangle
  n <- nrow(completed) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout 
  
  ### reserve summary (if not inherited from profileLadder)
  if (any(is.na(reserve))){
    reserve <- c(sum(completed[last]), sum(completed[,n]), sum(completed[,n]) - sum(completed[last]), NA)
    names(reserve) <- c("Paid Amount", "Estimated Ultimate", "Estimated Reserve", "True Reserve")
  }
  
  ### triangle standardization
  if (std == TRUE){
    firstPositive <- apply(completed, 1, function(row){return(row[row > 0][1])})
    firstPositive[is.na(firstPositive)] <- 0
    completed[firstPositive != 0, ] <- completed[firstPositive != 0, ] / 
                                       firstPositive[firstPositive != 0]
  }
  
  ### auxiliary permutation function
  permute <- function(matrix, method = NULL){
    pMatrix <- matrix[sample(1:n, n, replace = FALSE), ]
    zeroRows <- apply(pMatrix, 1, function(row){return(all(row == 0))})
    pMatrix[!observed] <- NA
    
    ### back-rescaling (back-standardization)
    if (std == TRUE){
      pMatrix[!zeroRows, ] <- pMatrix[!zeroRows, ] * firstPositive[firstPositive != 0]
    }
    
    ### applying PARALLAX, REACT, MACRAME
    if (method == "GLM"){out <- ChainLadder::glmReserve(ChainLadder::as.triangle(pMatrix))$FullTriangle}
    if (method == "Tweedie model"){out <- ChainLadder::tweedieReserve(ChainLadder::as.triangle(pMatrix), bootstrap = 0, link.power = linkPower, var.power = varPower)$FullTriangle}
    if (method == "Mack model"){out <- ChainLadder::MackChainLadder(ChainLadder::as.triangle(pMatrix), est.sigma = "Mack")$FullTriangle}
    if (method == "Triangle model"){
      out <- ChainLadder::chainladder(ChainLadder::as.triangle(pMatrix))
      out <- ChainLadder::predict(out)
    }
    if (method == "PARALLAX"){out <- parallelReserve(pMatrix, method = "parallax")$FullTriangle}
    if (method == "REACT"){out <- parallelReserve(pMatrix, method = "react")$FullTriangle}
    if (method == "MACRAME"){
      if (is.null(states) & is.null(breaks)){out <- mcReserve(pMatrix)$FullTriangle} 
        else {out <- mcReserve(pMatrix, states = states, breaks = breaks)$FullTriangle}
    }  
    if (outputAll){
      ## last column | last diagonal | last incremental diagonal | first column
      return(c(out[,n], rev(out[last]), rev(ChainLadder::cum2incr(out)[last]), out[,1]))
    } else { 
      ## last column | last diagonal
      return(c(out[,n], rev(out[last])))
    }
    
  }
  
  ### permutation bootstrap with time efficiency  
  startTime <- Sys.time()
  if (pb == TRUE){ 
    if (requireNamespace("pbapply", quietly = TRUE)){
      pbapply::pboptions(type = "timer", char = ">", txt.width = 43)
      pReserve <- pbapply::pbreplicate(B, permute(completed, method))
    } else {
      message("The R package 'pbapply' must be installed to use the progress bar")
      pReserve <- replicate(B, permute(completed, method))
    } 
  } else {
    pReserve <- replicate(B, permute(completed, method))
  }
  endTime <- Sys.time() - startTime
    
  ### bootstrapped ultimates
  ultimates <- data.frame(t(pReserve[1:n,]))
  names(ultimates) <- paste("origin", 1:n, sep = " ")
  
  ### bootstrapped latest (cummulative)
  latestCum <- data.frame(t(pReserve[(n + 1):(2 * n),]))
  names(latestCum) <- paste("origin", 1:n, sep = " ")
  
  if (outputAll){### full information in the output
    ### bootstrapped latest (incremental)
    latest <- data.frame(t(pReserve[(2 * n + 1):(3 * n),]))
    names(latest) <- paste("origin", 1:n, sep = " ")
    
    ### bootstrapped first payments
    first <- data.frame(t(pReserve[(3 * n + 1):(4 * n),]))
    names(first) <- paste("origin", 1:n, sep = " ")
  } 
  
  
  ### bootstrapped reserves
  reserves <- apply(pReserve[1:n,],2, sum) - apply(pReserve[(n + 1):(2 * n), ], 2, sum)
  
    

  ### OUTPUT section
  estimatedReserve <- reserve
  names(estimatedReserve) <- c("Paid Amount", "   Est.Ultimate", 
                               "   Est.Reserve", "   True Reserve")
  
  BootCoV <- 100 * stats::sd(reserves)/mean(reserves)
  BootVar995 <- stats::quantile(reserves, quantile)/mean(reserves)
  
  pSummary <- c(mean(reserves), stats::sd(reserves), BootCoV, BootVar995)
  userQ <- strsplit(as.character(format(quantile, nsmall = 3)), "\\.")[[1]][2]
  names(pSummary) <- c("Boot.Mean", "   Std.Er.", "   BootCov%", 
                       paste("   BootVar.", userQ, sep = ""))
  
 
  output <- list()
  output$eSummary <- estimatedReserve  ## PARALLAX/REACT/MACRAME
  output$pSummary <- pSummary
  
  output$method <- paste("Permutation bootstrap (", method, " method)", sep = "")
  
  output$pReserves <- as.numeric(reserves)
  
  if (outputAll){
    output$pUltimates <- ultimates
    output$pLatest <- latest
    output$pLatestCum <- latestCum
    output$pFirst <- first
  } else {
    output$pUltimates <- NA
    output$pLatest <- NA
    output$pLatestCum <- NA
    output$pFirst <- NA
  }
  
  
  if (all(!is.na(trueComplete))){
    output$tUltimate <- trueComplete[,n]
  } else {
    output$tUltimate <- NA
  }
  output$tLatest <- rev(ChainLadder::cum2incr(inputTriangle)[last])
  
  output$Triangle <- inputTriangle
  output$FullTriangle <- completedTriangle
  output$trueComplete <- trueComplete
  
  time <- c(Sys.info()["sysname"], Sys.info()["machine"], paste("B = ", round(B,0), sep = ""), paste("n = ", n, sep = ""), 
            paste(round(endTime, 2), " sec.", sep = ""))
  names(time) <- c("OS System", "Architecture", "Permutation number", "   Triangle dimension", "   Run time")
  output$info <- time
  
  class(output) <- c('permutedReserve', 'list')
  return(output)
} ### end of permuteReserve() function

