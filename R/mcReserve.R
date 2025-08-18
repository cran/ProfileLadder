#' MACRAME Based Development Profile Reserve 
#'
#' The function takes a cumulative (or incremental) run-off triangle (partially 
#' or completely observed) and returns the reserve prediction obtained by the 
#' MACRAME algorithm (see Maciak, Mizera, and Pešta (2022) for further details).
#'
#' @param chainLadder a cumulative or incremental run-off triangle (the triangle 
#' must be of the class \code{triangle} or \code{matrix}) in terms of a square 
#' matrix with a fully observed upper-left triangular part. If the lower-right 
#' part is also provided the function will also return standard residuals but 
#' only the upper-left (run-off) triangle is be used for the reserve prediction 
#' purposes
#' @param cum logical to indicate the type of the input triangle that is provided 
#' (DEFAULT value is \code{TRUE} for the cumulative triangle, \code{FALSE} if 
#' \code{chainLadder} is of the incremental type)
#' @param residuals logical to indicate whether (incremental) residuals should 
#' be provided in output or not. If the run-off triangle is completely observed 
#' then the residuals are obtained in terms of the true increments minus the 
#' predicted ones. If the bottom-right triangle is not available (which is a typical 
#' situation in practice) then the residuals are obtained in terms of a back-fitting 
#' approach (see Maciak, Mizera, and Pešta (2022) for further details). 
#' However, the back-fitted residuals are only calculated when 
#' no user specification of the states (in \code{states}) and breaks 
#' (in \code{breaks}) is provided (as it is usually not appropriate to use the same 
#' states/breaks for the flipped run-off triangle)
#' @param states numeric value to provide either the number of the Markov states 
#' to be used or it can specify an explicit set of the states instead. 
#' The default setting (\code{states = NULL}) provides the set of states in a fully 
#' data-driven manner as proposed in Maciak, Mizera, and Pešta (2022) while any 
#' choice of  \code{breaks} is ignored. If the number of states is specified by 
#' \code{states}, the states are obtained analogously as in Maciak, Mizera,
#' and Pešta (2022), however, the number of actual  states  is 
#' adjusted and the parameter \code{breaks} is again ignored 
#' 
#' If parameter \code{states} provides an explicit vector of Markov chain states 
#' (the smallest state should be larger than the smallest observed increment in 
#' the run-off triangle and, similarly, the largest state should be smaller than 
#' the largest observed increment) then the corresponding bins (breaks) for the 
#' run-off triangle increments are defined automatically by the midpoints between 
#' the provided states (with \code{breaks} being set to \code{NULL} DEFAULT)
#' @param breaks vector parameter which provides explicit (unique and monotonly 
#' increasing) break points (disjoint bins) for the run-off triangle incremenets. 
#' Each bin should be represented by the corresponding Markov chain state---either 
#' the values given in \code{states} or provided automatically if \code{states} is 
#' not a valid vector of the Markov states. If the breaks are provided as 
#' \code{breaks = c(-Inf, ... , Inf)} defining \code{k} bins all together then 
#' \code{states} should be a vector of the same length \code{k}. Alternatively, 
#' the breaks can be also specified by a set of finite numbers defining again 
#' \code{k} bins---in such cases, the parameter \code{states} should be of the 
#' length \code{length(states) = k + 1}. Each value in \code{states} should 
#' represent one bin defined by \code{breaks}
#' 
#' @returns An object of the type \code{list} with with the following elements: 
#' \item{reserve}{numeric vector with four values: Total paid amount (i.e., the 
#' sum of the last observed diagonal in a cumulative run-off triangle); Estimated 
#' ultimate (i.e., the sum of the last column in the completed cumulative run-off
#' triangle); Estimated reserve (i.e., the sum of the last column in the completed 
#' cumulative triangle minus the sum of the last observed diagonal 
#' in \code{chainLadder}); True reserve if a completed \code{chainLadder} is 
#' provided for the input (i.e., the sum of the last column in \code{chainLadder} 
#' minus the sum of the last diagonal in \code{chainLadder})}
#' \item{method}{algorithm used for the reserve estimation}
#' \item{Triangle}{the input run-off triangle provided in \code{chainLadder}}
#' \item{FullTriangle}{completed run-off triangle (the upper-left triangular part 
#' is identical with the input triangle in \code{chainLadder} and the lower-right 
#' trianglular part is completed by the MACRAME algorithm}
#' \item{trueCompleted}{true completed triangle (if available) where the upper-left 
#' part is used by the MACRAME algorithm to estimate the reserve and the lower-right 
#' part is provided for some evaluation purposes. If the full triangle is not 
#' available \code{NA} is returned instead}
#' \item{residuals}{a triangle with the corresponding residuals (for 
#' \code{residuals = TRUE}). The residuals are either provided in the upper-left 
#' triangle (so-called back-fitted incremental residuals if true completed triangle 
#' is not available) or the residuals are given in the lower-right triangle (i,e., 
#' standard incremental residuals---if the true completed triangle is given)}
#' 
#' @seealso [incrExplor()], [permuteReserve()], [mcBreaks()], [mcStates()], [mcTrans()]
#' 
#' @examples
#' ## run-off (upper-left) triangle with NA values
#' data(MW2014, package = "ChainLadder")
#' print(MW2014) 
#' 
#' ## MACRAME reserve prediction with the DEFAULT Markov chain setting 
#' mcReserve(MW2014, residuals = TRUE)
#' 
#' ## complete run-off triangle with 'unknown' truth (lower-bottom run-off triangle)  
#' ## with incremental residuals (true increments minus predicted ones)  
#' data(CameronMutual)
#' mcReserve(CameronMutual, residuals = TRUE)
#' 
#' ## the same output in terms of the reserve prediction but back-fitted residuals 
#' ## are provided instead (as the run-off triangle only is provided)
#' data(observed(CameronMutual))
#' mcReserve(observed(CameronMutual), residuals = TRUE)
#' 
#' ## MACRAME reserve prediction with the underlying Markov chain with five 
#' ## explicit Markov chain states
#' mcReserve(CameronMutual, residuals = TRUE, states = c(200, 600, 1000))
#' 
#' @references Maciak, M., Mizera, I., and Pešta, M. (2022). Functional Profile 
#' Techniques for Claims Reserving. ASTIN Bulletin, 52(2), 449-482. DOI:10.1017/asb.2022.4
#' 
#' @export
mcReserve <- function(chainLadder, cum = TRUE, residuals = FALSE, 
                                   states = NULL, breaks = NULL){
  ### input data checks
  if (!(inherits(chainLadder, "triangle") || inherits(chainLadder, "matrix"))){
    stop("The input data must be of class 'triangle' or 'matrix'.")}
  if (dim(chainLadder)[1] != dim(chainLadder)[2]){
    stop("The input data do not form a run-off triangle (square matrix).")}
  
  n <- nrow(chainLadder) ### number of occurrence/development years
  last <- n * (1:n) - 0:(n - 1) ### last diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout
  statesSelect <- states
  breaksSelect <- breaks
  
  if (!is.numeric(chainLadder[observed])){
    stop("The input values are not numeric.")}
  if (sum(is.na(chainLadder[observed])) > 0){
    stop("The run-off triangle is not fully observed (missing values).")}
  
  ### cumulative vs. incremental triangle 
  if (cum == TRUE){
    if (sum(chainLadder[last]) < sum(chainLadder[,1])){
      warning("The run-off triangle seems to be not of the cumultative type!")}
    incrTriangle <- ChainLadder::cum2incr(chainLadder)
  } else {
    incrTriangle <- chainLadder
    chainLadder <- ChainLadder::incr2cum(chainLadder)
  }
  
  ### maximum (observed) increment (beside the first column)
  incrTriangle[!observed] <- NA
  maxIncr <- max(incrTriangle[,-1], na.rm = TRUE)
  
  ### observed increments and unique increments in the triangle
  incrs <- sort(incrTriangle[,-1])
  uniqueIncr <- unique(incrs)
  
  if (is.null(states) & is.null(breaks)){### 1 DEFAULT (Maciak, Mizera, and Pesta 2022) 
    setup <- 1 
  } else {
    if (is.numeric(states) & length(states) == 1){### 2. states provided (breaks ignored)
      if (round(states, 0) != states){
        stop("Integer value for the number of states must be provided")}
      if (states > length(uniqueIncr)/4){
        stop(paste("The number of provided states is too high. ",
                   "Maximum allowed number of states is ", 
                   floor(length(uniqueIncr)/4),".", sep= ""))}
      setup <- 2 
    } else {### vector of states is provided or NULL AND breaks are provided or NULL 
      if (is.numeric(states) & length(states) > 1 & is.null(breaks)){
        ### 3. explicit states (breaks is NULL)
        if (length(states) != length(unique(states))){
          stop("The numeric vector of states must provide a set of unique states")} 
        if (length(states) > length(uniqueIncr)/4){
          stop(paste("The number of provided states is too high. ",
                     "Maximum allowed number of states is ", 
                     floor(length(uniqueIncr)/4),".", sep= ""))}
        if (min(states) <= min(uniqueIncr) | max(states) > max(uniqueIncr)){
          stop(paste("The set of states is out of bounds. ",
                     "Minimum observed incremenet is  ", 
                     min(uniqueIncr)," and maximum observed increment is ", 
                     max(uniqueIncr),".", sep= ""))}
        setup <- 3
      } else {### states explicit or NULL and explicit breaks  
        if (!is.numeric(breaks) || length(breaks) == 1){
          stop("Parameter 'breaks' should provide a numeric vector of valid (unique) breaks")}
        if (breaks[1] != -Inf){breaks <- c(-Inf, breaks)}
        if (breaks[length(breaks)] != Inf){breaks <- c(breaks, Inf)}
        if (is.null(states)){### 4. states are NULL & breaks are provided
          if (length(unique(breaks)) != length(breaks)){
            stop("The vector of breaks must provide a unique set of breaks")}
          if (any(diff(breaks) < 0)){
            stop("The vector of breaks must provide a monotone (increasing) sequence")}
          setup <- 4
        } else {### 5. states and break are explicit numeric vectors
          if ((length(breaks) - 1) != length(states)){
            stop(paste("The number of breaks and the number of states do not correspond ",
                 "('length(states)' should be equal 'length(breaks) + 1')", sep = ""))}
          if (sum(table(cut(states, breaks, right = FALSE)) != 0) != length(states)){
            stop("The vector of states should represent intervals given by the 'breaks' parameter.")}
          setup <- 5
        }
      }
    }
  }
  
  ### incremental triangle without the first column
  incrTriangle1C <- incrTriangle[, -1]
  
  ### yGrid / breaks
  if (setup == 1){yGrid <- incrs[ceiling((1:(n - 1) * length(incrs)) / n) + 1]}
  if (setup == 2){yGrid <- incrs[ceiling((1:(states - 1) * length(incrs)) / states) + 1]}
  if (setup == 3){yGrid <- sort(states)[-length(states)] + diff(sort(states))/2}
  if (setup %in% 4:5){yGrid <- breaks}
  
  ### completed unique grid
  if (yGrid[1] != -Inf){yGrid <- c(-Inf, yGrid)}
  if (yGrid[length(yGrid)] != Inf){yGrid <- c(yGrid, Inf)}
  yGrid <- unique(yGrid)
  
  ### states
  if (setup %in% c(1,2,4)){
    states <- sapply(1:(length(yGrid) - 1), function(k){
      stats::median(incrTriangle1C[incrTriangle1C >= yGrid[k] & incrTriangle1C < yGrid[k + 1]], 
                    na.rm = TRUE)})} 
  
  yGrid <- yGrid[c(!is.na(states), TRUE)]
  states <- states[!is.na(states)]
  yGrid[1] <- -Inf
  
  ### rescaling for the transition matrix P estimation
  yGridStd <- yGrid / maxIncr
  statesStd <- states / maxIncr 
  incrTriangle1Std <- incrTriangle1C / maxIncr
  
  ### transition probability matrix
  P <- matrix(rep(0, length(statesStd)^2), ncol = length(statesStd))
  s <- 1 
  for (t in (1:(n - 2))){### time of transition
    v1 <- incrTriangle1Std[1:(n - t), t + (s - 1)] ### reweighted 
    v2 <- incrTriangle1Std[1:(n - t), t + 1 + (s - 1)]  ### reweighted
    P <- P + table(cut(v1,breaks=yGridStd, right = FALSE),cut(v2,breaks=yGridStd, right = FALSE)) 
  }
  
  P <- matrix(sweep(P,1,rowSums(P),`/`), ncol = length(statesStd))
  P[is.na(P)] <- 0
  
  P[statesStd == 0,] <- rep(0, length(statesStd))
  P[statesStd == 0, statesStd == 0] <- 1
  
  ### matrix perturbation / zero state excitation
  if (sum(P[, statesStd == 0] != 0) == length(statesStd)){
    P2 <- matrix(rep(0, length(statesStd)^2), nrow = length(statesStd))
    P2[,statesStd == 0] <- rep(1, length(statesStd))
    
    if (length(statesStd) > 1){
      delta <- (sum(P[, statesStd == 0]))/n * (10 / (length(statesStd) - 1))
    } else {### transition matrix id only a 1x1 matrix
      delta <- 0
    }
    
    P <- (1 - delta) * P + delta * P2
  }
  
  pow <- function(x, n) Reduce(`%*%`, replicate(n, x, simplify = FALSE))
  P_powers <- lapply(1:(n - 1), function(s) pow(P, s)) 
  
  ### Markov Chain prediction
  inState <- function(x){
    eVec <- rep(0, length(states))
    eVec[which(levels(cut(x, breaks=yGrid, right = FALSE)) == cut(x, breaks=yGrid, right = FALSE))] <- 1
    return(eVec)
  }
  
  startState <- t(sapply(incrTriangle[row(incrTriangle) + col(incrTriangle) == n + 1],  inState))
  for (j in 0:(n - 2)){
    if (length(P) == 1){### only one state
      where2go <- rep(states, n - j - 1)
    } else {
      startState <- startState[-nrow(startState), ]
      where2go <- startState %*% P_powers[[j + 1]] %*% states
    }
    
    ### check for fully developed profile 
    where2go[incrTriangle[row(incrTriangle) + col(incrTriangle) == n + 1 + j][-(n - j)] == 0] <- 0
    incrTriangle[row(incrTriangle) + col(incrTriangle) == n + 2 + j] <- where2go
  }
  
  ### completed run-off triangle
  completed <- ChainLadder::incr2cum(incrTriangle)
  
  ### overall predicted reserve 
  reserve <- sum(completed[,n]) - sum(completed[last])
  
  ### OUTPUT for the Markov chain setup
  MarkovChain <- list()
  MarkovChain$states <-states
  MarkovChain$breaks <- yGrid
  MarkovChain$transitionMatrix <- round(P, 4)
  
  
  ### backfitting for residuals
  if (residuals == TRUE){
    if (all(is.na(chainLadder[!observed(n)]))){### backfitted residuals
      if (is.null(breaksSelect) & is.null(statesSelect)){
        ### only obtained for DETAULT setting (states and breaks are NULL)
        ### flipped triangle
        resids <- matrix(rev(as.vector(completed)), nrow = n, byrow = F)
        
        incrTriangle <- ChainLadder::cum2incr(resids)
        
        ### maximum (observed) increment (beside the first column)
        incrTriangle[!observed] <- NA
        maxAbsIncr <- max(abs(incrTriangle[,-1]), na.rm = T)
        
        
        xGrid <- 1:n
        
        ### breaks
        incrs <- sort(incrTriangle[,-1])
        yGrid2 <- incrs[ceiling((1:(n - 1) * n * (n - 1)) / (2 * n)) + 1]
        yGrid2 <- c(-Inf, unique(yGrid2), Inf)
        
        ### states for the incremental triangle without first column
        incrTriangle1C <- incrTriangle[, -1]
        states2 <- sapply(1:(length(yGrid2) - 1), 
                          function(k){stats::median(incrTriangle1C[incrTriangle1C >= yGrid2[k] 
                                                                   & incrTriangle1C < yGrid2[k + 1]], 
                                                    na.rm = TRUE)})
        
        yGrid2 <- yGrid2[c(!is.na(states2), T)]
        states2 <- states2[!is.na(states2)]
        yGrid2[1] <- -Inf
        
        yGrid <- yGrid2 / maxAbsIncr
        states <- states2 / maxAbsIncr 
        incrTriangle1C <- incrTriangle1C / maxAbsIncr
        
        ### transition probability matrix
        P <- matrix(rep(0, length(states)^2), ncol = length(states))
        s <- 1 
        for (t in (1:(n - 2))){### time of transition
          v1 <- incrTriangle1C[1:(n - t), t + (s - 1)] ### reweighted 
          v2 <- incrTriangle1C[1:(n - t), t + 1 + (s - 1)]  ### reweighted
          P <- P + table(cut(v1,breaks=yGrid, right = FALSE),cut(v2,breaks=yGrid, right = FALSE)) 
        }
        
        P <- matrix(sweep(P,1,rowSums(P),`/`), ncol = length(states))
        P[is.na(P)] <- 0
        
        P[states == 0,] <- rep(0, length(states))
        P[states == 0, states == 0] <- 1
        ### all rows in P should sum up to one!!!
        
        ### matrix perturbation
        if (sum(P[, states == 0] != 0) == length(states)){
          P2 <- matrix(rep(0, length(states)^2), nrow = length(states))
          P2[,states == 0] <- rep(1, length(states))
          
          delta <- (sum(P[, states == 0]))/n * (10 / (length(states) - 1))
          P <- (1 - delta) * P + delta * P2
        }
        
        P_powers <- lapply(1:(n - 1), function(s) pow(P, s)) 
        
        ### Markov Chain prediction
        inState <- function(x){
          eVec <- rep(0, length(states2))
          eVec[which(levels(cut(x, breaks=yGrid2, right = FALSE)) == cut(x, breaks=yGrid2, right = FALSE))] <- 1
          return(eVec)
        }
        
        startState <- t(sapply(incrTriangle[row(incrTriangle) + col(incrTriangle) == n + 1],  inState))
        
        for (j in 0:(n - 2)){
          startState <- startState[-nrow(startState), ]
          where2go <- startState %*% P_powers[[j + 1]] %*% states2
          
          ### check for fully developed profile 
          where2go[incrTriangle[row(incrTriangle) + col(incrTriangle) == n + 1 + j][-(n - j)] == 0] <- 0
          incrTriangle[row(incrTriangle) + col(incrTriangle) == n + 2 + j] <- where2go
        }
        
        resids <- ChainLadder::incr2cum(incrTriangle)
        resids <- matrix(rev(as.vector(resids)), nrow = n, byrow = FALSE)
        
        resids <- ChainLadder::cum2incr(completed) - ChainLadder::cum2incr(resids)
        resids[!observed] <- NA 
      } else {resids <- NULL} ### no backfitted residuals for USER defined breaks and states
    } else {
      resids <- ChainLadder::cum2incr(chainLadder) - ChainLadder::cum2incr(completed)
      resids[observed] <- NA
    }
  }
  
  ### OUTPUT section
  reserveOutput <- c(sum(chainLadder[last]), sum(completed[,n]), 
                     sum(completed[,n]) - sum(completed[last]), 
                     sum(chainLadder[,n]) - sum(chainLadder[last]))
  names(reserveOutput) <- c("Paid Amount", "   Estimated Ultimate", 
                            "   Estimated Reserve", "   True Reserve")
  
  inputTriangle <- chainLadder
  inputTriangle[!observed] <- NA
  
  output <- list()
  output$reserve <- reserveOutput
  output$method <- "MACRAME method (functional profile completion)"
  output$Triangle <- ChainLadder::as.triangle(inputTriangle)
  output$FullTriangle <- ChainLadder::as.triangle(completed)
  if (all(is.na(chainLadder[!observed(n)]))){
    output$trueComplete <- NA
  } else {
    output$trueComplete <-  ChainLadder::as.triangle(chainLadder)
  }
  if (residuals == TRUE){output$residuals <- resids} else {output$residuals <- NULL}
  
  output$MarkovChain <- MarkovChain
  
  class(output) <- c('profileLadder', 'list')
  return(output)
} ### end of MACRAME function
