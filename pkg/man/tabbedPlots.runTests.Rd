\name{tabbedPlots.runTests}
\alias{tabbedPlots.runTests}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tabbedPlots.runTests(tests = "ALL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{tests}{ ~~Describe \code{tests} here~~ }
}
\details{
  ~~ If necessary, more details than the description above ~~
}
\value{
  ~Describe the value returned
  If it is a LIST, use
  \item{comp1 }{Description of 'comp1'}
  \item{comp2 }{Description of 'comp2'}
  ...
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ 

 ~Make other sections like Warning with \section{Warning }{....} ~
}
\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (tests = "ALL") 
{
    if (tests == "ALL") 
        tests <- c(".testNewCloseExit", ".testLayout", ".testComplexPlot", 
            ".testComplexPlotMultiFigure1", ".testComplexPlotMultiFigure2", 
            ".testReusePar1", ".testReusePar2", ".testReusePar3", 
            ".testReusePar4", ".testReusePar5", ".testReusePar6", 
            ".testReusePar7", ".testReuseNewClose", ".testLattice", 
            ".testSaveAllDevices", ".testSaveAllExtensions")
    ow <- options(warn = 2)
    on.exit(options(ow))
    for (test in tests) {
        cat("\nRunning test", test, "\n")
        eval(call(test))
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
