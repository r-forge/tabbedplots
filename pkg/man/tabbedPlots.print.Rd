\name{tabbedPlots.print}
\alias{tabbedPlots.print}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tabbedPlots.print(warn = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{warn}{ ~~Describe \code{warn} here~~ }
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
function (warn = NULL) 
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages()) 
        return(FALSE)
    path = tempfile()
    on.exit(try(file.remove(path), silent = TRUE))
    tabbedPlots.save(path, "postscript", warn = warn)
    if (file.exists(path)) {
        ret <- system(paste("lpr", path), intern = FALSE)
        if (ret == 0) 
            return(TRUE)
        else {
            lpr = system("which lpr", intern = TRUE)
            .warning(lpr, "returned error code", ret, warn = warn)
        }
    }
    else .warning("Failed to generate Postscript file.", warn = warn)
    return(FALSE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
