\name{example4}
\alias{example4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Example 4}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
example4(filename)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
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
function (filename) 
{
    load(file = filename)
    plot(df[, "x"], df[, "y"])
    inTrain <- createDataPartition(y = df$y, p = 0.7, list = FALSE)
    train <- df[inTrain, ]
    test <- df[-inTrain, ]
    gmodel <- kinn("y~x", train)
    ygraph <- predict.kinn(gmodel, test)
    plotGraphs(gmodel, filename)
    message("corralation to true y : ", cor(test$y, ygraph))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
