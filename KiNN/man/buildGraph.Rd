\name{buildGraph}
\alias{buildGraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Builds an adjacency graph}
\description{
}
\usage{
buildGraph(X, Y, method = "det", uue = "linear")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{uue}{
%%     ~~Describe \code{uue} here~~
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
function (X, Y, method = "det", uue = "linear") 
{
    mat <- buildSimilarityMatrix(X)
    nedges = numEdgesBasedOnNoise(X, Y, uue)
    if (method == "det") 
        g <- simMatToDetermAdjGraph(mat, nedges, directed = F)
    return(g)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
