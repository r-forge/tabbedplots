##
## Package/Namespace Hooks
##

.onLoad <- function(libname, pkgname)
{
    ## Initialize package private variables.
    .initialize(file.path(libname, pkgname))

    ## Start monitoring high level plot funcitons.
    .addHighLevelPlotFunctionHooks()
}

.onUnload <- function(libname, pkgname)
{
    ## Stop monitoring high level plot funcitons.
    .removeHighLevelPlotFunctionHooks()
}

.onAttach <- function(libname, pkgname)
{
    if (getOption("verbose"))
    {
        desc <- .packageDescription()
        message(sprintf("%s, version %s", desc$Title, desc$Version))
        message(sprintf("Type library(help = %s) to see package documentation.",
                        desc$Package))
    }
}
