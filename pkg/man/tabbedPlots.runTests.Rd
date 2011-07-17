\name{tabbedPlots.runTests}
\alias{tabbedPlots.runTests}
\title{ Run the tabbedPlots test suite }
\description{
  Run through the tabbedPlots test suite.
}
\usage{
tabbedPlots.runTests(tests = "ALL", manual.new=TRUE)
}
\arguments{
  \item{tests}{ which tests to perform }
  \item{manual.new}{ see below }
}
\details{
Run through the tabbedPlots test suite.
Allows the user to verify if the tabbedPlots functionality is working correctly.
Console output explains what should be seen in the windows.

The manual.new argument provides control over testing the feature
that calls to par() and layout() made before a graphics device
is open have an effect.  This feature depends on hooks in the
functions par() and layout() (which were originally implemented
via trace(), which is ugly and fragile.)  If this feature is not
present, calls made to par() and layout() before a graphics device
is open will have no effect.  In this situation, one must call
plot.new() before calling par() and layout().  Supplying
manual.new=TRUE causes the tests to call plot.new() before they
call par() and/or layout().

}
\keyword{ graphics }
