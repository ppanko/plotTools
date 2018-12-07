\name{mlPlot}
\alias{mlPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mlPlot(y, x, nest, data, fixed.x = TRUE, ylb = NULL, xlb = NULL, mn = NULL, save = FALSE, svfl = NULL, wid, high, rs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{nest}{
%%     ~~Describe \code{nest} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{fixed.x}{
%%     ~~Describe \code{fixed.x} here~~
}
  \item{ylb}{
%%     ~~Describe \code{ylb} here~~
}
  \item{xlb}{
%%     ~~Describe \code{xlb} here~~
}
  \item{mn}{
%%     ~~Describe \code{mn} here~~
}
  \item{save}{
%%     ~~Describe \code{save} here~~
}
  \item{svfl}{
%%     ~~Describe \code{svfl} here~~
}
  \item{wid}{
%%     ~~Describe \code{wid} here~~
}
  \item{high}{
%%     ~~Describe \code{high} here~~
}
  \item{rs}{
%%     ~~Describe \code{rs} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (y, x, nest, data, fixed.x = TRUE, ylb = NULL, xlb = NULL, 
    mn = NULL, save = FALSE, svfl = NULL, wid, high, rs) 
{
    if (fixed.x == TRUE) {
        frm <- formula(noquote(paste0(y, "~ 1 + ", x, " + (1|", 
            nest, ")")))
    }
    else if (fixed.x == FALSE) {
        frm <- formula(noquote(paste0(y, "~ 1 + ", x, " + (1 +", 
            x, "|", nest, ")")))
    }
    mod <- lmer(frm, data = data, REML = FALSE)
    l2coef <- coef(mod)[[1]]
    coef1 <- coef(summary(mod))
    if (save == TRUE) 
        png(svfl, width = wid, height = high, res = rs)
    plot(data[, x], data[, y], type = "n", xlab = xlb, ylab = ylb, 
        main = mn)
    for (i in 1:nrow(l2coef)) {
        l2id <- rownames(l2coef)[i]
        l2dat <- data[data[, nest] == l2id, ]
        xmin <- min(l2dat[, x])
        xmax <- max(l2dat[, x])
        ymin <- l2coef[i, 1] + l2coef[i, 2] * xmin
        ymax <- l2coef[i, 1] + l2coef[i, 2] * xmax
        lines(c(xmin, xmax), c(ymin, ymax), lwd = 0.8, col = "lightsteelblue2")
    }
    totalxmin <- min(data[, x])
    totalxmax <- max(data[, x])
    totalymin <- coef1[1, 1] + coef1[2, 1] * totalxmin
    totalymax <- coef1[1, 1] + coef1[2, 1] * totalxmax
    lines(c(totalxmin, totalxmax), c(totalymin, totalymax), lwd = 3, 
        col = "lightsteelblue4")
    if (save == TRUE) 
        dev.off()
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
