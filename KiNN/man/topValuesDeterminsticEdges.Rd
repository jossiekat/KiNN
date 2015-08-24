\name{topValuesDeterminsticEdges}
\alias{topValuesDeterminsticEdges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Top K values in similarity matrix}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
topValuesDeterminsticEdges(m, nedges, directed)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{m}{
%%     ~~Describe \code{m} here~~
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
function (m, nedges, directed) 
{
    if (directed == FALSE) 
        nedges <- nedges * 2
    r <- rank(m, ties.method = "first")
    topr <- (length(r) - nedges + 1)
    w <- which(r < topr)
    r[w] <- 0
    r[-w] <- 1
    g <- matrix(r, nrow = nrow(m))
    return(g)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
