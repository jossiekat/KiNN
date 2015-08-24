\name{kinn}
\alias{kinn}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
KiNN learner using dataframeand formula interface}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
kinn(f, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{f}{
%%     ~~Describe \code{f} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (f, data) 
{
    g <- as.formula(f)
    indep <- all.vars(g)[2]
    if (indep == ".") 
        indep = setdiff(colnames(data), lhs.vars(g))
    else indep = rhs.vars(g)
    X <- as.matrix(data[, indep])
    Y <- as.matrix(data[, lhs.vars(g)])
    graph <- buildGraphModel(X, Y)
    graph$dep <- lhs.vars(g)
    graph$indep <- indep
    return(graph)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
