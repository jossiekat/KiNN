\name{exmaple2}
\alias{exmaple2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Example 2}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
exmaple2()
}
%- maybe also 'usage' for other objects documented here.
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
function () 
{
    X <- as.matrix(c(1, 2, 3, 1, 2, 3), ncol = 1)
    Y <- as.matrix(c(1, 2, 3, 6, 5, 4), ncol = 1)
    x <- as.matrix(c(2.5, 3.8, 5), ncol = 1)
    gmodel <- buildGraphModel(X, Y)
    ygraph <- predict.graph(gmodel, x)
    plot(X, Y)
    plotGraphs(gmodel)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
