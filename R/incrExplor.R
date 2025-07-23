#' Exploratory Function for Run-Off Triangle Increments
#'
#' The function takes a cumulative or incremental run-off triangle (partially or 
#' completely observed) and provides some basic exploratory and graphical 
#' inspection of the observed incremental payments. The function serves as 
#' a useful tool for a user-based insight when manualy defining the states of 
#' the Markov Chain that is used to drive the reserve prediction in the MACRAME 
#' algorithm implemented in the function \code{mcReserve()}.  
#'
#' @param triangle cumulative or incremental run-off triangle (an object of the 
#' class \code{triangle} or \code{matrix}) specified in terms of a partially 
#' observed (run-off triangle) or a fully observed (completed triangle) matrix. 
#' Only the upper-left triangular part (run-off trangle) is used to provide the 
#' output analysis of the incremental payments and the underlying Markov chain 
#' setting options 
#' @param method method form \code{c("median", "mean", "max", "min")} used to 
#' summarize the run-off triangle increments within the given set of bins. Each 
#' bin with the increments  is represented by a corresponding Markov state value 
#' (obtained by the \code{method} choice with \code{median} being the DEFAULT option)
#' @param out integer value (or a vector of integers) to indicate which columns 
#' of the run-off triangle should be excluded 
#' from the exploratory analysis of the increments. By DEFAULT, the first 
#' incremental payments---i.e., the first column of the run-off triangle---are 
#' not considered (\code{out = 1}). No colums are exluded for \code{out = 0} and 
#' the whole run-off triangle is analyzed by \code{incrExplor()}. To specify 
#' multiple columns that should be excluded, one can use \code{out = c(1,2,3)} 
#' which will exlude the first three columns from the exploratory analysis
#' @param states either an integer value to indicate an explicit number of the 
#' Markov chain states to be used or  a vector of explicit Markov chain states 
#' can be provided. The DEFAULT option (\code{states = NULL}) provides a fully 
#' data-driven (automatic) definition of the Markov chain states as proposed 
#' in Maciak, Mizera, and Pešta (2022)
#' @param breaks numeric vector of explicit (unique and monotonously increasing) 
#' break points to define the bins for the run-off triangle increments.
#' If \code{states} is equal to some integer number (i.e., the explicit number 
#' of the Markov chain states is requested by \code{states}) then the value of 
#' \code{breaks} is ignored. If both \code{states} and \code{breaks} are specified 
#' (i.e., numeric vectors are provided for both) then the set of states in 
#' \code{states} must be given in a way that exactly one state value belongs to 
#' exactly one bin defined by the break points specified by \code{breaks}
#' 
#' @returns An object of the class \code{mcSetup} with the following elements:
#' \item{incrTriangle}{an object of the class \code{triangle} with the incremental 
#' run-off triangle}
#' \item{triangleType}{type of the input run-off triangle provided for the input 
#' object \code{triangle} (cumulative or incremental)}
#' \item{defaultStates}{the data-driven set of explicit states as used (by DEFAULT) 
#' by the \code{mcReserve()} function -- the MACRAME prediction algorithm}
#' \item{defaultBreaks}{the set of explicit data-driven breaks as used (by DEFAULT) 
#' by the \code{mcReserve()} function -- the MACRAME prediction algorithm}
#' \item{increments}{table with basic empirical characteristics of the increments 
#' of the input run-off triangle (without the first origin payments---the values 
#' in the first column of the run-off triangle). Two sets of increments are provided: 
#' the raw incremental payments in the first row of the table and the standardized 
#' increments (i.e., row incremental payments divided by the maximum payment within 
#' the row (while not considering the columns specified by the \code{out} parameter)}
#' \item{userDefined}{a list with all information regarding the USER modified input 
#' (numeric vector \code{increments} with the increments being analyzed; numeric 
#' value in \code{outColumns} denoting the excluded columns in the run-off triangle; 
#' \code{method} used to summarize the increments within the bins; numeric vector 
#' with the resulting Markov chain states in \code{states} and the corresponding  
#' numeric vector with the break points in \code{breaks} defining the bins for 
#' the run-off triangle increments)}
#' 
#' @seealso [mcReserve()], [permuteReserve()]
#' 
#' @examples
#' data(CameronMutual) 
#' 
#' ## default Markov Chain states with (roughly) equally occupied bins 
#' incrExplor(CameronMutual)
#' 
#' ## five Markov Chain states (with roughly equally occupied bins)
#' incrExplor(CameronMutual, states = 5)
#' 
#' ## explicitly defined breaks for five increment bins while the Markov states
#' ## are obtained as medians of the increments within each bin
#' incrExplor(CameronMutual, breaks = c(20, 500, 1000, 2000))
#' 
#' ## explicitly defined breaks for five bins and the Markov states
#' ## are given as the maximum increments within each bin
#' incrExplor(CameronMutual, breaks = c(20, 500, 1000, 2000), method = "max")
#' 
#' ## manually defined breaks for the bins and the corresponding states 
#' ## exactly one state must be within each break
#' incrExplor(CameronMutual, breaks = c(20, 500, 1000), 
#'                           states = c(10, 250, 800, 1500))
#' 
#' @references Maciak, M., Mizera, I., and Pešta, M. (2022). Functional Profile 
#' Techniques for Claims Reserving. ASTIN Bulletin, 52(2), 449-482. DOI:10.1017/asb.2022.4
#' 
#' 
#' @export
incrExplor <- function(triangle, method = c("median", "mean", "max", "min"), 
                                 out = 1, states = NULL, breaks = NULL){
  ### input data checks
  if (!(inherits(triangle, "triangle") || inherits(triangle, "matrix"))){
    stop("The input data must be of class 'triangle' or 'matrix'.")}
  if (dim(triangle)[1] != dim(triangle)[2]){
    stop("The input data do not form a run-off triangle (square matrix).")}
  
  n <- nrow(triangle) ### number of occurrence/development periods
  last <- n * (1:n) - 0:(n - 1) ### last running diagonal
  observed <- outer(1:n, 1:n, function(i, j) j <= (n + 1 - i)) ### NA layout 
  triangle[!observed] <- NA 
  
  ### cumulative or incremental
  if (sum(triangle[last]) > sum(triangle[,1])){### cumulative triangle
    triangleType <- "Cumulative"
    incrTriangle <- ChainLadder::cum2incr(triangle)
    paidAmount <- sum(triangle[last])
  } else {### incremental triangle
    triangleType <- "Incremental"
    incrTriangle <- triangle
    paidAmount <- sum(triangle[observed])
  }
  
  ##############################################################################
  ### analysis of the increments

  ### 1. DEFAULT analysis (as in Maciak, Mizera, and Pesta, 2022)
  ### maximum increment beside columns in out
  maxIncr <- max(incrTriangle[,-1], na.rm = TRUE)
  
  ### all increments except the first column
  incrs <- sort(incrTriangle[,-1])
  uniqueIncr <- unique(incrs)
  
  ### incremental triangle without first column
  incrTriangle1C <- incrTriangle[, -1]
  
  ### DEFAULT breaks (MMP2022)
  xGrid <- 1:n
  yGrid2 <- incrs[ceiling((1:(length(xGrid) - 1) * n * (n - 1)) / (2 * length(xGrid))) + 1] 
  yGrid2 <- c(-Inf, unique(yGrid2), Inf)
  
  ### DEFAULT states (MMP2022)
  states2 <- sapply(1:(length(yGrid2) - 1), function(k){
  stats::median(incrTriangle1C[incrTriangle1C >= yGrid2[k] & incrTriangle1C < yGrid2[k + 1]], na.rm = TRUE)})
  
  ### adjustment for no increments in the bin
  defaultBreaks <- yGrid2[c(!is.na(states2), TRUE)]
  defaultBreaks[1] <- -Inf
  defaultStates <- states2[!is.na(states2)]
  
  
  ### 2. USER defined parameters (parameters out/states/breaks)
  ### input checks 
  if (!all(out %in% 0:n)){stop("Parameter 'out' is out of the bounds")} 
  if (length(out) > n/2){stop("The number of deleted colums by the 'out' parameter is two high")}
  
  
  if (all(out != 0)){## some columns are deleted
    ### maximum increment beside columns in out
    out <- out[out != 0]
    maxIncrU <- max(incrTriangle[,-out], na.rm = TRUE)
  
    ### all allowed increments
    incrsU <- sort(incrTriangle[,-out])
    uniqueIncrU <- unique(incrsU)
    
    ### incremental triangle without columns in out
    incrTriangle1CU <- incrTriangle[, -out]
  } else {### no columns are deleted
    ### maximum increment beside columns in out
    maxIncrU <- max(incrTriangle, na.rm = TRUE)
    
    ### all allowed increments
    incrsU <- sort(incrTriangle)
    uniqueIncrU <- unique(incrsU)
    
    ### incremental triangle without columns in out
    incrTriangle1CU <- incrTriangle
  }
  
  if ((all(method == c("median", "mean", "max","min")) | method[1] == "median") & 
          length(out) == 1 & out[1] == 1 & is.null(states) & is.null(breaks)){
    userInput <- FALSE
    setup <- 0
  }
  else {### USER defined modifications
    userInput <- TRUE
    if (is.null(states) & is.null(breaks)){### out is modified 
      setup <- 1 ### out != 1
    } else {
      if (is.numeric(states) & length(states) == 1){### 2. states provided (breaks ignored)
        if (round(states, 0) != states){
          stop("Integer value for the number of states must be provided")}
        if (states > length(uniqueIncrU)/4){
          stop(paste("The number of provided states is too high. ",  
                     "Maximum allowed number of states is ", 
                     floor(length(uniqueIncrU)/4),".", sep= ""))}
        setup <- 2 ### states are provided, breaks are ignored
      } else {### vector of states is provided or NULL AND breaks are provided or NULL 
        if (is.numeric(states) & length(states) > 1 & is.null(breaks)){
          ### 3. explicit (vector) states provided (breaks NULL)
          if (length(states) != length(unique(states))){
            stop("The numeric vector of states must provide a set of unique states")} 
          if (length(states) > length(uniqueIncrU)/4){
            stop(paste("The number of provided states is too high. ", 
                       "Maximum allowed number of states is ", 
                       floor(length(uniqueIncrU)/4),".", sep= ""))}
          if (min(states) <= min(uniqueIncrU) | max(states) > max(uniqueIncrU)){
            stop(paste("The set of states is out of bounds. ", 
                       "Minimum observed incremenet is ", 
                       min(uniqueIncrU)," and maximum observed increment is ", 
                       max(uniqueIncrU),".", sep= ""))}
          setup <- 3 ### vector for states is provied, breaks is NULL 
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
            setup <- 4 ### states = NULL, breaks are specified
          } else {### 5. states and break are explicit numeric vectors
            if ((length(breaks) - 1) != length(states)){
              stop(paste("The number of breaks and the number of states do not correspond",  
                         "('length(states)' should be equal 'length(breaks) + 1')"), sep = " ")}
            if (sum(table(cut(states, breaks, right = F)) != 0) != length(states)){
              stop("The vector of states should represent intervals given by the 'breaks' parameter.")}
            setup <- 5 ### states and breaks provided 
          }
        }
      }
    }
      
    ### yGrid / breaks
    if (setup == 1){yGridU <- incrsU[ceiling((1:(n - 1) * length(incrsU)) / n) + 1]}
    if (setup == 2){yGridU <- incrsU[ceiling((1:(states - 1) * length(incrsU)) / states) + 1]}
    if (setup == 3){yGridU <- sort(states)[-length(states)] + diff(sort(states))/2}
    if (setup %in% 4:5){yGridU <- breaks}
    
    ### complete unique grid
    if (yGridU[1] != -Inf){yGridU <- c(-Inf, yGridU)}
    if (yGridU[length(yGridU)] != Inf){yGridU <- c(yGridU, Inf)}
    yGridU <- unique(yGridU)
    
    ### states
    if (setup %in% c(1,2,4)){
      ### definition of the states by the method 
      if (!any(method %in% c("median", "mean", "min", "max"))){
        stop("Incorrect method selected. Method must be from c('median', 'mean', 'min', 'max')")}
      ### median state
      if (any(method %in% c("median", "mean", "min", "max"))){
        statesU <- sapply(1:(length(yGridU) - 1), function(k){
          stats::median(incrTriangle1CU[incrTriangle1CU >= yGridU[k] & incrTriangle1CU < yGridU[k + 1]], 
                        na.rm = TRUE)})}
      ### mean state
      if (all(method == "mean")){
        statesU <- sapply(1:(length(yGridU) - 1), function(k){
          mean(incrTriangle1CU[incrTriangle1CU >= yGridU[k] & incrTriangle1CU < yGridU[k + 1]], 
               na.rm = TRUE)})}
      ### min state
      if (all(method == "min")){
        statesU <- sapply(1:(length(yGridU) - 1), function(k){
          min(incrTriangle1CU[incrTriangle1CU >= yGridU[k] & incrTriangle1CU < yGridU[k + 1]], 
              na.rm = TRUE)})}
      ### max state
      if (all(method == "max")){
        statesU <- sapply(1:(length(yGridU) - 1), function(k){
          max(incrTriangle1CU[incrTriangle1CU >= yGridU[k] & incrTriangle1CU < yGridU[k + 1]], 
              na.rm = TRUE)})}
    } else {
      statesU <- states
    }
    
    ### optional adjustment (if no increments in the bin)
    yGridU <- yGridU[c(!is.na(statesU), TRUE)]
    yGridU[1] <- -Inf
    statesU <- statesU[!is.na(statesU)]
  }
  
  ## summary of user defined modifications for the output
  if (userInput == TRUE){
    userDefined <- list()
    userDefined$increments <- incrsU
    userDefined$outColumns <- out
    if (all(method == c("median", "mean", "min", "max")) | method[1] == "median"){
      userDefined$method <- "DEFAULT (median)"
    } else {
      userDefined$method <- method
    }
    userDefined$states <- statesU
    userDefined$breaks <- yGridU
    userDefined$setup <- setup ### value 3 for explictely enforced set of states
  } else {
    userDefined <- NULL  
  }
  
  ### Default Markov chain details
  MarkovChain <- list()
  MarkovChain$states <- defaultStates
  MarkovChain$breaks <- defaultBreaks
  MarkovChain$increments <- incrs
  
  ### OUTPUT (overall)
  output <- list()
  output$incrTriangle <- ChainLadder::as.triangle(incrTriangle)
  output$triangleType <- triangleType
  output$MarkovChain <- MarkovChain
  output$userDefined <- userDefined
  
  class(output) <- c('mcSetup', 'list')
  return(output)
}
