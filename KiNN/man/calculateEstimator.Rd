\name{calculateEstimator}
\alias{calculateEstimator}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
calculate y^hat
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
calculateEstimator(g, simv, Y, alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{g}{
%%     ~~Describe \code{g} here~~
}
  \item{simv}{
%%     ~~Describe \code{simv} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{alpha}{
%%     ~~Describe \code{alpha} here~~
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
function (g, simv, Y, alpha) 
{
    ng <- neighborsInGraph(g, simv)
    ngy <- apply(ng, 1, function(x) (x \%*\% Y)/sum(x != 0))
    ngy[is.na(ngy)] <- (Y[simv])[is.na(ngy)]
    (1 - alpha) * ngy + alpha * t(Y[simv])
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
