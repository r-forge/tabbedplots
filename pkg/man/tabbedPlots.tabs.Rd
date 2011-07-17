\name{tabbedPlots.tabs}
\alias{tabbedPlots.tabs}
\alias{tabbedPlots.new}
\alias{tabp.new}
\alias{tabbedPlots.close}
\alias{tabbedPlots.nextTab}
\alias{tabbedPlots.prevTab}
\alias{tabbedPlots.print}
\alias{tabbedPlots.save}
\alias{tabbedPlots.copy}
\title{ Functions to work with tabbedPlots tabs }
\description{
  Functions to work with the individual tabs in a tabbedPlots window.
}
\usage{
tabbedPlots.new(label = NULL, reusePar = .get("reusePar"), warn = NULL)
tabp.new(label = NULL, reusePar = .get("reusePar"), warn = NULL)
tabbedPlots.close(warn = NULL)
tabbedPlots.nextTab(warn = NULL)
tabbedPlots.prevTab(warn = NULL)
tabbedPlots.print(warn = NULL)
tabbedPlots.save(file, device = NULL, width = 7, height = 7,
                 pointsize = 10, warn = NULL, use.dev.copy = FALSE, drawingArea = NULL, quality = 80, ...)
tabbedPlots.copy(warn = NULL)
}
\arguments{
  \item{label}{ Label for the tab }
  \item{reusePar}{ should \code{par()} values for the previous plot be reused?}
  \item{warn}{ Where should warnings go (for internal use) }
  \item{file}{ filename }
  \item{device}{ device }
  \item{width}{ width }
  \item{height}{ height }
  \item{pointsize}{ pointsize }
  \item{use.dev.copy}{ should the save be down using a pixmap or by
    redrawing using dev.copy() ?}
  \item{drawingArea}{ internal use }
  \item{quality}{ for JPEG, between 0 and 100}
  \item{...}{ additional arguments to pass to the plotting device }
}
\details{
  These funtions operate on the tabs of the currently active tabbedPlots window.
}
\value{
  Some of the functions return \code{TRUE}/\code{FALSE} to indicate
  success or failure.
}
\seealso{ \link[=tabbedPlots]{Functions to work with tabbedPlots windows} }
\keyword{ graphics }
