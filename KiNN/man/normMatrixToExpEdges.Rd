\name{normMatrixToExpEdges}
\alias{normMatrixToExpEdges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Normalize similarity values to match expected # of edges}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
normMatrixToExpEdges(mat, nedges, directed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
%%     ~~Describe \code{mat} here~~
}
  \item{nedges}{
%%     ~~Describe \code{nedges} here~~
}
  \item{directed}{
%%     ~~Describe \code{directed} here~~
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
function (mat, nedges, directed) 
{
    s <- sum(mat)
    proportion <- nedges/s
    if (directed == FALSE) 
        proportion <- (proportion * 2)
    mat <- proportion * mat
    print(sum(mat))
    print(mat)
    return(mat)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
