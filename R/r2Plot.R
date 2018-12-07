### Title:    R2 efficiency plotting function  
### Author:   Pavel Panko
### Created:  2016-MAY-01
### Modified: 2018-DEC-07

## Convenience function for calculating derivatives 
derivative <- function(x, y) diff(y)/diff(x)

## Main r-squared plotting function
r2Plot <- function(r2, cumulative = FALSE, ...) {
    ## Calculate non-cumulative r-squared
    if(cumulative)
        r2 <- c(r2[1], diff(r2))
    ## Calculate second derivative
    x <- seq_along(r2)
    d2 <- derivative(x[-1] - diff(x)/2, derivative(x, r2))
    ## Biggest drop-off
    elbows <- sapply(seq_along(d2), function(x) which(d2 == max(d2[x:length(d2)])))
    drop   <- elbows[which(diff(elbows) == max(diff(elbows)))]
    ## Average efficiency
    cntr <- abs(r2 - mean(r2))
    slide <- which(cntr == min(cntr))
    ## Create plot
    plot(r2, ...)
    abline(v = drop, col = "red")
    abline(h = r2[slide], col = "blue")
    ## Print report
    msg <- strwrap(sprintf("Biggest drop in variance contribution happens at
           component %i, below average contributions start after component %i", drop, slide),
           width = 81)
    return(msg)
}
