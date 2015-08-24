\name{buildGraphModel}
\alias{buildGraphModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Builds the graph model}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
buildGraphModel(X, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
%%     ~~Describe \code{X} here~~
}
  \item{Y}{
%%     ~~Describe \code{Y} here~~
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
function (X, Y) 
{
    listModel <- list()
    mX <- simClust(X)
    XY = cbind(X, Y)
    mXY <- simClust(XY)
    c2d <- mXY$classification
    gx <- list()
    vx <- list()
    vy <- list()
    cxy <- list()
    for (i in 1:mX$G) {
        id = (mX$classification == i)
        gx[[i]] = buildGraph(X[id], Y[id])
        vx[[i]] = X[id]
        vy[[i]] = Y[id]
        cxy[[i]] = c2d[id]
    }
    listModel$mX = mX
    listModel$mXY = mXY
    listModel$gx <- gx
    listModel$vx <- vx
    listModel$vy <- vy
    listModel$X <- X
    listModel$Y <- Y
    listModel$cxy <- cxy
    return(listModel)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
