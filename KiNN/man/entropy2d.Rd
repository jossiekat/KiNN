\name{entropy2d}
\alias{entropy2d}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates scaled entropy}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
entropy2d(cxy)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{cxy}{
%%     ~~Describe \code{cxy} here~~
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
function (cxy) 
{
    if (length(cxy) == 0) 
        return(0)
    t <- table(cxy)
    e <- 0
    pv <- 0
    pe <- list()
    for (i in 1:length(t)) {
        p <- t[i]/sum(t)
        e <- e + log2(1/p) * p
    }
    if (e != 0) 
        e <- e/log2(length(t))
    return(round(e, 3))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
