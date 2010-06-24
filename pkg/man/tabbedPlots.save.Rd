\name{tabbedPlots.save}
\alias{tabbedPlots.save}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ ~~function to do ... ~~ }
\description{
  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
tabbedPlots.save(file, device = NULL, width = 7, height = 7, pointsize = 10, warn = NULL, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{file}{ ~~Describe \code{file} here~~ }
  \item{device}{ ~~Describe \code{device} here~~ }
  \item{width}{ ~~Describe \code{width} here~~ }
  \item{height}{ ~~Describe \code{height} here~~ }
  \item{pointsize}{ ~~Describe \code{pointsize} here~~ }
  \item{warn}{ ~~Describe \code{warn} here~~ }
  \item{\dots}{ ~~Describe \code{\dots} here~~ }
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
function (file, device = NULL, width = 7, height = 7, pointsize = 10, 
    warn = NULL, ...) 
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages()) 
        return(FALSE)
    extension = .getExtension(file)
    if (is.null(device)) 
        device <- .mapExtensionToDevice(extension, warn)
    if (is.null(device)) 
        return(FALSE)
    if (is.character(device)) 
        device <- get(device)
    if (!is.function(device)) {
        .warning("'device' must be either a character or function.", 
            warn)
        return(FALSE)
    }
    if (!is.null(device)) {
        deviceArgNames <- names(formals(device))
        myArgs <- c("file", "width", "height", "pointsize")
        myArgsIdx <- pmatch(myArgs, deviceArgNames, nomatch = 0)
        args <- list(device = device)
        for (i in seq(along = myArgs)) if (myArgsIdx[i] > 0) 
            args[deviceArgNames[myArgsIdx[i]]] = get(myArgs[i])
        if ("units" \%in\% deviceArgNames && "res" \%in\% deviceArgNames) {
            args["units"] <- "in"
            args["res"] <- 72
        }
        if ("onefile" \%in\% deviceArgNames) 
            args["onefile"] <- TRUE
        if (extension == "eps") {
            if ("horizontal" \%in\% deviceArgNames) 
                args["horizontal"] <- FALSE
            if ("onefile" \%in\% deviceArgNames) 
                args["onefile"] <- FALSE
            if ("paper" \%in\% deviceArgNames) 
                args["paper"] <- "special"
        }
        args <- .mergeLists(list(...), args)
        do.call("dev.copy", args)
        dev.off()
    }
    return(TRUE)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
