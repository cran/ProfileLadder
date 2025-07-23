#' Plotting the Run-Off Triangle Increments by the MC Setting
#'
#' The function provides a graphical visualization of the results obtained from 
#' the function \code{incrExplor()}. In particular, the considered run-off triangle 
#' increments are distributed into the bins according the given Markov chain breaks 
#' or states. 
#'
#' @param x an object of the class \code{mcSetup} -- i.e., the output 
#' of the \code{incrExplor()} function
#' @param ... other graphical parameters to plot
#' 
#' @return The function returns a layout with two plots. ...
#' 
#' @seealso [incrExplor()], [mcReserve()]
#' 
#' @examples
#' ## run-off triangle increments within the default bins
#' x <- incrExplor(CameronMutual)
#' plot(x)
#' 
#' ## run-off triangle increments and user-defined number of bins
#' x <- incrExplor(CameronMutual, states = 5)
#' plot(x)
#' 
#' ## run-off triangle increments within the user-specified bins
#' x <- incrExplor(CameronMutual, breaks = c(500, 1000, 1500))
#' plot(x)
#' 
#' @export
#' @method plot mcSetup
plot.mcSetup <- function(x, ...){
  if (!inherits(x, "mcSetup")) stop("Object must be of class 'mcSetup'")
  
  dots <- list(...)
  if ("cex" %in% names(dots)){cex <- dots$cex} else {cex <- 1}
  if ("cex.main" %in% names(dots)){cex.main <- dots$cex.main} else {cex.main <- 1}
  
  ###  graphical window setting and reset on exit
  old_par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old_par))
  
  ### Detect if user has manually set a multi-frame layout
  is_multi_panel <- any(graphics::par("mfrow") != c(1, 1))
  
  ### If not in multi-panel mode, prompt for new page
  if (!is_multi_panel && interactive()) {
    grDevices::devAskNewPage(TRUE)
  } else {
    grDevices::devAskNewPage(FALSE)
  }
  
  ### OS system spec + scale
  sys_info <- Sys.info()
  scaleFac <- switch(
    sys_info["sysname"],
    "Windows" = 0.65 * cex,
    "Darwin"  = 0.75 * cex,
    0.68 * cex
  )
  
  n <- ncol(x$incrTriangle)
  if (is.null(x$userDefined)){### data-driven (default) MC setting
    incrs <- x$MarkovChain$increments
    breaks <- x$MarkovChain$breaks
    states <- x$MarkovChain$states
    
    main1 <- "(I) Histogram: Incremental triangle[ , -1]"
    main2 <- "(II) DEFAULT bins/breaks for increments and MC states"
  } else {
    incrs <- x$userDefined$increments
    breaks <- x$userDefined$breaks
    states <- x$userDefined$states
    
    main1 <- paste("(I) Histogram: Incremental triangle[ , -c(", paste(x$userDefined$outColumns, collapse = ","),")]", sep = "")
    main2 <- "(II) USER defined: method/out/states/breaks"
  }
  
  ### plot 1
  graphics::hist(incrs, breaks = n, col = "lightblue", freq = FALSE,
                 xlab = "Run-off triangle increments", main = main1, 
                 xlim = c(min(incrs), max(incrs)), cex.main = cex.main)
  graphics::lines(stats::density(incrs), col = "red", lwd = 2)
  
  ### plot 2
  plotLabels <- names(table(cut(incrs, breaks=breaks, right = FALSE, dig.lab = 6)))
  tableNum <- table(cut(incrs, breaks=breaks, right = FALSE, dig.lab = 6))
  names(tableNum) <- 1:length(tableNum)
  defaultPlot <- graphics::barplot(tableNum, xaxt='n', 
                                   xlab = paste("Run-off triangle increments (", 
                                                length(states), " disjoint bins)", sep = ""), 
                                   ylab = "Frequency of increments", main = main2, cex.main = cex.main)
  graphics::axis(side=1,at=defaultPlot[2 * 1:ceiling(length(plotLabels)/2) - 1], 
                 labels = plotLabels[2 * 1:ceiling(length(plotLabels)/2) - 1])
  graphics::text(defaultPlot, rep(max(tableNum/5),n), labels = "MC state\n value", 
                 col = "darkred", cex = scaleFac * 0.9, pos = 1)
  graphics::text(defaultPlot, rep(max(tableNum/10),n), labels = round(states, 0), 
                 col = "darkblue", cex = scaleFac * 1.4, pos = 1, font = 2)
  
  invisible(NULL)
} 









