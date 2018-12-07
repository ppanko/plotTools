\name{derivPlot}
\alias{derivPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
derivPlot(r, type2 = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{r}{
%%     ~~Describe \code{r} here~~
}
  \item{type2}{
%%     ~~Describe \code{type2} here~~
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
function (r, type2 = FALSE) 
{
    r2 <- c(r[1], diff(r))
    shifter <- function(n, x = r2) {
        if (n == 0) 
            -2 * x
        else ret <- c(tail(x, -n), head(x, n))
    }
    deriv2Prelim <- abs(rowSums(sapply(c(-1, 1, 0), shifter)))
    deriv2 <- deriv2Prelim[-c(1, length(deriv2Prelim), which(deriv2Prelim == 
        0))]
    elbows <- sapply(1:length(deriv2), y = deriv2, function(x, 
        y) which(y == max(y[x:length(y)])))
    drop <- elbows[which(diff(elbows) == max(diff(elbows)))]
    r2 > mean(r2[-which(r2 == 0)])
    cntr <- abs(r2 - mean(r2))
    slide <- which(cntr == min(cntr))
    if (!type2) {
        plot(r2)
        abline(v = drop, col = "red")
        abline(v = which(deriv2 == max(deriv2)))
        abline(h = r2[slide], col = "blue")
    }
    if (type2) {
        plot(r2c)
        abline(v = c(drop, slide), col = "red")
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
