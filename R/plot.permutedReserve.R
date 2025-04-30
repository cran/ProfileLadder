#' Plotting the output of the permutation bootstrap
#'
#' The function provides a graphical visualization of the results obtained from 
#' the permutation bootstrap (see Maciak, Mizera, and Pesta (2022) for further 
#' details) applied to the output of one of the nonparametric functional based 
#' estimation algorithm---PARALLAX or REACT implemented in the 
#' \code{parallelReserve()} function or MACRAME implemented in the 
#' \code{mcReserve()} function. 
#'
#' @param x an object of the class \code{permutedReserve} -- i.e., the output 
#' of the \code{permuteReserve()} function
#' @param ... other graphical parameters to plot
#' 
#' @return The function returns a layout for four plots. The first panel shows 
#' a simple barplot type visualization of the estimated reserve, the estimated 
#' ultimate and the true reserve (if available). The second panel provides 
#' a histogram for (permuted) bootstrapped reserves with a nonparametric estimate
#' of the corresponding density. The third panel provides a detailed inspection 
#' of the bootstrapped ultimates (with true ultimates if provided) and, finaly, 
#' the last panel shows the observed diagonal vs. simulated ones. 
#' 
#' @seealso [permuteReserve()]
#' 
#' @examples
#' ## reserve estimated by MACRAME and the corresponding visualization
#' x <- mcReserve(CameronMutual)
#' plot(permuteReserve(x, B = 100))
#' 
#' @export
#' @method plot permutedReserve
plot.permutedReserve <- function(x, ...){
    
    reserve <- x$eSummary
    pReserve <- x$pReserves
    pUltimates <- x$pUltimates
    pLatest <-x$pLatest
    trueUltimate <- x$tUltimate
    trueLatest <- x$tLatest
    
    method <- gsub("\\(", "", strsplit(x$method, " ")[[1]][3])
    B <- as.numeric(strsplit(x$info[3], " = ")[[1]][2])
    n <- as.numeric(strsplit(x$info[4], " = ")[[1]][2])
    
    ### OS type 
    sys_info <- Sys.info()
    if (sys_info["sysname"] == "Windows") {## Windows OS
      scaleFac <- 0.65
      inSet <- 0.005
      xInter <- 0.5
      yInter <- 1
    } else if (sys_info["sysname"] == "Darwin") {## macOS
      scaleFac <- 0.75
      inSet <- c(-0.05, -0.01)
      xInter <- 0.5
      yInter <- 1
    } else {## Linux
      scaleFac <- 0.65
      inSet <- 0
      xInter <- 0.5
      yInter <- 1
    }
    
    ###  graphical window setting and reset on exit
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par))
    ###
    
    graphics::par(mfrow = c(2,2), mar = c(5, 3, 5, 3), cex = scaleFac)
  
    
    ### plot 1
    offSet <- (max(pReserve) - min(pReserve))/100
    ylimits <- c(0.98 * min(c(pReserve, reserve[3], reserve[4]), na.rm = T), 
                 1.02 * max(c(pReserve, reserve[3], reserve[4]), na.rm = T))
    graphics::boxplot(pReserve, main = paste("Permutation bootstrap for ", method, " (B = ", B, ")", sep = ""), 
                      cex = 0.8,  xlab = "Permutation bootstrap reserves",
                      horizontal = T, ylim = ylimits, pch = 22, bg = "gray")
    graphics::lines(rep(mean(pReserve), 2),  c(0.7, 1.3), col = "red", lwd = 3, lty = 2)
    graphics::text(mean(pReserve) + offSet, 0.72, "Bootstrap Average", col = "red", pos = 4, cex = 0.8)
    
    if (!is.na(reserve[4])){
      if (reserve[4] > reserve[3]){
        graphics::lines(rep(reserve[4], 2), c(0.6, 1.4), col = "darkred", lwd = 4)
        graphics::text(reserve[4] + offSet, 0.62,
                       "True reserve", col = "darkred" , pos = 4, cex = 1)
        
        graphics::lines(rep(reserve[3], 2), c(0.6, 1.4) , col = "darkblue", lwd = 4, lty = 2)
        graphics::text(reserve[3] - offSet, 0.62,
                       paste(method, " reserve", sep =""), col = "darkblue" , 
                       pos = 2, cex = 1)
      } else {
        graphics::lines(rep(reserve[4], 2), c(0.6, 1.4), col = "darkred", lwd = 4)
        graphics::text(reserve[4] - offSet, 0.62, 
                       "True reserve", col = "darkred" , pos = 2, cex = 1)
        
        graphics::lines(rep(reserve[3], 2), c(0.6, 1.4), col = "darkblue", lwd = 2, lty = 4)
        graphics::text(reserve[3] + offSet, 0.62, 
                       paste(method, " reserve", sep =""), col = "darkblue" , 
                       pos = 4, cex = 1)
      }
    } else {
      graphics::lines(rep(reserve[3], 2), c(0.6, 1.4), col = "darkblue", lwd = 2, lty = 2)
      graphics::text(reserve[3] + offSet,  0.62,
                     paste(method, " reserve", sep =""), col = "darkblue" , 
                     pos = 4, cex = 0.8)
    }
      
     
    ### plot 2 
    dEst <- stats::density(pReserve)
    xLim <- c(min(dEst$x), max(dEst$x))
    yLim <- max(dEst$y)
    graphics::hist(pReserve, xlab = "Permuted (bootstrap) reserves", ylab = "",  
                   main = paste("Permutation reserve distribution (B = ",B,")", sep = ""), 
                   breaks = max(n, B/n), xlim = xLim, ylim = c(0, 1.5 * yLim), 
                   freq = F, cex = 0.8)
    graphics::box()
    graphics::polygon(dEst$x, dEst$y, 
                      col = grDevices::adjustcolor( "red", alpha.f = 0.1))
    graphics::lines(dEst, col = "red", lwd = 2)
    if (!is.na(reserve[4])){
      graphics::lines(rep(reserve[4], 2), c(0,1.1 * yLim), 
                      col = "darkred", lwd = 3)
      graphics::lines(rep(reserve[3], 2), c(0, 1.1 * yLim), 
                      col = "darkblue", lwd = 3, lty = 2)
      graphics::legend("topleft", legend = c(paste(method, "reserve", sep = " "), 
                                              "Permuted bootstrap density", "True Reserve"), pch = 22, 
                                   pt.bg = c("darkblue", "red", "darkred"), 
                                   fill = "lightgray", border = "lightgray", 
                                   inset = inSet, x.intersp = xInter, y.intersp = yInter,
                                   box.lwd = 0, box.col = "white", bg = "white", pt.cex = 1.5)
    } else {
      graphics::lines(rep(reserve[3], 2), c(0, 1.1 * yLim), 
                      col = "darkblue", lwd = 3, lty = 2)
      graphics::legend("topleft", legend = c(paste(method, "estimated reserve", sep = " "), 
                                              "Permuted bootstrap density"), pch = 22, 
                                   pt.bg = c("darkblue", "red"), 
                                   fill = "lightgray", border = "lightgray", 
                                   inset = inSet, x.intersp = xInter, y.intersp = yInter,
                                   box.lwd = 0, box.col = "white", bg = "white", pt.cex = 1.5)
    }
    
    
    ### plot 3
    graphics::boxplot(pUltimates, main = "Permuted ultimate claims\n (the last column)", 
                      ylim = c(min(pUltimates), 1.4 * max(pUltimates)),
                      xlab = "Origin period", ylab = "Ultimate claims",
                      col = "lightblue", pch = 22, bg = "gray", cex = 0.8)
      
    xVal <- t(cbind((1:n) - 0.4, (1:n + 0.4)))
    yVal <- t(matrix(rep(apply(pUltimates, 2, mean), 2), nrow = n))
    
    graphics::matlines(xVal, yVal, col = "red", lwd = 2, lty = 1)
    
    if (all(!is.na(trueUltimate))){
      graphics::points(1:n, trueUltimate, pch = 22, bg = "darkred", cex = 1.4)
      
      graphics::legend("topleft", legend = c("Permutation bootstrap", "Permuted bootstrap means", "True ultimates"), 
                       pch = 22, pt.bg = c("lightblue", "red", "darkred"), fill = "lightgray", 
                       inset = inSet, x.intersp = xInter, y.intersp = yInter,
                       border = "lightgray", box.lwd = 0, box.col = "white", bg = "white", pt.cex = 1.5)
    } else {
      graphics::legend("topleft", legend = c("Permutation bootstrap", "Permuted bootstrap means"), 
                       pch = 22, pt.bg = c("lightblue", "red"), fill = "lightgray", 
                       inset = inSet, x.intersp = xInter, y.intersp = yInter,
                       border = "lightgray", box.lwd = 0, box.col = "white", bg = "white", pt.cex = 1.5)
    }
    
    
    ### plot 4
    graphics::boxplot(pLatest, main = "Latest incremental claims\n (actual vs. simulated)", 
                      ylim = c(min(pLatest), 1.2 * max(pLatest)), 
                      xlab = "Origin period", ylab = "Latest incremental claims",
                      col = "lightblue", pch = 22, bg = "gray", cex = 0.8)
    
    xVal <- t(cbind((1:n) - 0.4, (1:n + 0.4)))
    yVal <- t(matrix(rep(apply(pLatest, 2, mean), 2), nrow = n))
    
    graphics::matlines(xVal, yVal, col = "red", lwd = 2, lty = 1)
    graphics::points(1:n, trueLatest, pch = 22, bg = "darkred", cex = 1.2)
    
    graphics::legend("topleft", legend = c("Permutation bootstrap", "Permuted bootstrap means", "Actual latest (diagonal)"), 
                     pch = 22, pt.bg = c("lightblue", "red", "darkred"), fill = "lightgray", 
                     inset = inSet, x.intersp = xInter, y.intersp = yInter,
                     border = "lightgray", box.lwd = 0, box.col = "white", bg = "white", pt.cex = 1.5)
}









