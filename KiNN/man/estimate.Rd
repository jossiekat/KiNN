\name{estimate}
\alias{estimate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Calculates estimator on subgraph i}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
estimate(g, i, x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{i}{
%%     ~~Describe \code{i} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
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
function (g, i, x) 
{
    simv <- mostSimilarIndices(matrix(g$vx[[i]]), matrix(x))
    est <- calculateEstimator(g$gx[[i]], simv, g$vy[[i]], 0.8)
    ev <- getEstimatorsVector(g$gx[[i]], simv, g$vy[[i]], g$cxy[[i]], 
        0.8)
    return(est)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
