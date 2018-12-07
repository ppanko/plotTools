r2Plot <- function(r, type2 = FALSE) {

    r2 <- c(r[1], diff(r))

    shifter <- function(n, x = r2) {
        if (n == 0) -2*x
        else ret <- c(tail(x, -n), head(x, n))
    }

    deriv2Prelim <- abs(rowSums(sapply(c(-1, 1, 0), shifter)))
    deriv2 <- deriv2Prelim[-c(1, length(deriv2Prelim), which(deriv2Prelim == 0))]

    elbows <- sapply(
        1:length(deriv2), y = deriv2,
        function(x, y) which(y == max(y[x:length(y)]))
    )

    drop <- elbows[which(diff(elbows) == max(diff(elbows)))]

    r2 > mean(r2[-which(r2 == 0)])
    cntr <- abs(r2 - mean(r2))
    slide <- which(cntr == min(cntr))

    if(!type2) {
        plot(r2)
        abline(v = drop, col = "red")
        abline(v = which(deriv2 == max(deriv2)))
        abline(h = r2[slide], col = "blue")
    }

    if(type2) {
        plot(r2c)
        abline(v = c(drop, slide), col = "red")
    }
}
