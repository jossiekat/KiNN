\name{getEstimatorsVector}
\alias{getEstimatorsVector}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Checks estimation with regards to y entropy
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
getEstimatorsVector(g, simv, Y, C, alpha)
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
  \item{C}{
%%     ~~Describe \code{C} here~~
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
function (g, simv, Y, C, alpha) 
{
    y2d <- list()
    ng <- neighborsInGraph(g, simv)
    ent <- 0
    probv <- list()
    ngvy <- t(apply(ng, 1, function(x) (x * Y)))
    ngvc <- t(apply(ng, 1, function(x) (x * C)))
    for (i in 1:nrow(ngvc)) {
        line <- ngvc[i, ]
        line <- line[line > 0]
        ent[i] <- entropy2d(line)
        probv[[i]] <- prob2d(line)
    }
    for (i in 1:nrow(ngvc)) {
        for (j in 1:max(C)) {
            y2d[[j]] <- Y[ngvc[i, ] == j]
        }
        if (ent[i] > 0.5) 
            sig <- "*"
        else if (ent[i] > 0.2) 
            sig <- "."
        else sig <- " "
        ye <- vector()
        if (probv[[i]] > 0) {
            k = 1
            for (j in 1:max(ngvc[i, ])) {
                if (length(y2d[[j]]) > 0) {
                  ye[k] <- mean(y2d[[j]])
                  k = k + 1
                }
            }
            if (ent[i] == 0) {
                s <- paste0("p=", round(probv[[i]], 3), ",yhat=", 
                  round(as.numeric(ye), 3))
                cat(sig, " ", "entropy=", ent[i], s, "\n")
            }
            else {
                s <- paste0("(p=", round(probv[[i]], 3), ",yhat=", 
                  round(ye, 3), ")")
                cat(sig, " ", "entropy=", ent[i], s, "\n")
            }
        }
        else {
            sig = " "
            s <- paste0("p=", 1)
            cat(sig, " ", "entropy=", 0, s, "-no neighbors in graph.\n")
        }
    }
    return(ngvc)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
