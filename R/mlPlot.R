mlPlot <-
    function(y, x, nest, data, fixed.x = TRUE, save = FALSE, ...) {

    if(fixed.x == TRUE) {

        frm <- formula(paste0(y, "~ 1 + ", x, " + (1|", nest, ")"))
        
    } else if(fixed.x == FALSE) {
        
        frm <- formula(paste0(y, "~ 1 + ", x, " + (1 +", x, "|", nest, ")"))
    }

    mod <- lmer(frm, data = data, REML = FALSE)

    l2coef <- coef(mod)[[1]]
    coef1 <- coef(summary(mod))

    if(save == TRUE) png(...)

    plot(data[,x], data[,y], type = "n", ...)
    for(i in 1:nrow(l2coef)) {
        l2id <- rownames(l2coef)[i]
        l2dat <- data[data[,nest] == l2id,]
        xmin <- min(l2dat[,x])
        xmax <- max(l2dat[,x])
        ymin <- l2coef[i, 1] + l2coef[i, 2]*xmin
        ymax <- l2coef[i, 1] + l2coef[i, 2]*xmax
        lines(c(xmin, xmax), c(ymin, ymax), lwd=0.8, col="lightsteelblue2")
    }
    totalxmin <- min(data[,x])
    totalxmax <- max(data[,x])
    totalymin <- coef1[1, 1] + coef1[2, 1]*totalxmin
    totalymax <- coef1[1, 1] + coef1[2, 1]*totalxmax
    lines(c(totalxmin, totalxmax), c(totalymin, totalymax), lwd=3, col = "lightsteelblue4")

    if(save == TRUE) dev.off()

}
