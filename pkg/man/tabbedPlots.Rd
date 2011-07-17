\name{tabbedPlots}
\alias{tabbedPlots}
\alias{tabbedPlots.quit}
\alias{tabbedPlots.windowTitle}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Functions to work with tabbedPlots windows }
\description{
  Functions to work with tabbedPlots windows.
}
\usage{
tabbedPlots(title = NULL)
tabbedPlots.quit()
tabbedPlots.windowTitle(title)
}
\arguments{
  \item{title}{ Title to use the window}
}
\details{
  \itemize{
    \item{tabbedPlots()}{: Create a new tabbedPlots window to hold
      multiple plots in tabs}
    \item{tabbedPlots.quit()}{Close the active tabbedPlots window (i.e.,
    the whole window, not just one tab)}
    }
}
\seealso{ \link[=tabbedPlots.tabs]{Functions to work with tabbedPlots tabs} }
\keyword{ graphics }
