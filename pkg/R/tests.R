###########################################################################
##
##  S E M I - A U T O M A T I C   T E S T S
##
##  Run these using tabbedPlots.runTests() in an interactive session.
##  Each test will be run, and then a brief description of what should
##  have appeared will be printed, and then the user can hit return to
##  continue.
##
##  The manual.new argument provides control over testing the feature
##  that calls to par() and layout() made before a graphics device
##  is open have an effect.  This feature depends on hooks in the
##  functions par() and layout() (which were originally implemented
##  via trace(), which is ugly and fragile.)  If this feature is not
##  present, calls made to par() and layout() before a graphics device
##  is open will have no effect.  In this situation, one must call
##  plot.new() before calling par() and layout().  Supplying
##  manual.new=TRUE causes the tests to call plot.new() before they
##  call par() and/or layout().
##
###########################################################################

.testNewCloseExit <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", FALSE)
    for (i in 1:30)
        tabbedPlots.new()
    readline("GUI with 30 tabs?")
    for (i in 1:30)
        tabbedPlots.close()
    readline("GUI with 0 tabs?")
    # Slower to call plot.new() than tabbedPlots.new()
    for (i in 1:10)
        plot.new()
    readline("GUI with 10 tabs?")
    tabbedPlots.quit()
    readline("No gui?")
}

.testLayout <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", FALSE)
    if (manual.new)
        plot.new()
    layout(matrix(c(1,1,0,2), 2, 2, byrow = TRUE))
    plot(1, pch="1", cex=2); plot(2, pch="2", cex=2); plot(3, pch="3", cex=2)
    readline("Two tabs, plot 1 and 2 on first and 3 on second?")
    tabbedPlots.quit()
}

.testComplexPlot <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", FALSE)
    plot(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings))
    readline("4 complex plots in 4 tabs?")
    tabbedPlots.quit()
}

.testComplexPlotMultiFigure1 <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", FALSE)
    if (manual.new)
        plot.new()
    par(mfrow = c(2, 2))
    plot(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings))
    readline("4 complex plots in 1 tab?")
    tabbedPlots.quit()
}

.testComplexPlotMultiFigure2 <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", FALSE)
    if (manual.new)
        plot.new()
    par(mfrow = c(3, 1))
    plot(lm.SR <- lm(sr ~ pop15 + pop75 + dpi + ddpi, data = LifeCycleSavings))
    readline("4 complex plots in 2 tabs? 3 on first, 1 on second?")
    tabbedPlots.quit()
}

.testReusePar1 <- function(manual.new=TRUE)
{
    ## Works if "lastPar" is applied in prePlotNew (option 1)
    ## High level plots that wrap around tabs. The key that
    ## a high level plot creates the new tab, which is supposed
    ## to reuse par form the previous tab.
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    if (manual.new)
        plot.new()
    par(mfrow = c(2, 2))
    for (i in 1:5)
        hist(rnorm(100))
    readline("2 tabs? 4 plots on first, 1 at top-left of second?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReusePar2 <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    if (manual.new)
        plot.new()
    par(mfrow = c(2, 2))
    for (i in 1:4)
        hist(rnorm(100))
    plot.new()
    hist(rnorm(100))
    readline("2 tabs? 4 plots on first, 1 at top-left on second?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReusePar3 <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    plot.new()
    par(mfrow = c(2, 2))
    hist(rnorm(100))
    readline("1 tab with 1 plot at top-left?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReusePar4 <- function(manual.new=TRUE)
{
    ## Works if "lastPar" is applied in postPlotNew (option 2)
    ## 'par' reuse is forced with user level 'plot.new' call.
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    hist(rnorm(100))
    plot.new()
    par(mfrow = c(2, 2))
    hist(rnorm(100))
    readline("2 tabs? 1 full plot on first, 1 at top-left on second?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReusePar5 <- function(manual.new=TRUE)
{
    ## Works if "lastPar" is applied in postPlotNew (option 2)
    ## 'par' reuse is forced with user level 'plot.new' call.
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    if (manual.new)
        plot.new()
    par(mfrow = c(2, 2))
    for (i in 1:12)
        hist(rnorm(100))
    readline("3 tabs with 4 plots on each?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReusePar6 <- function(manual.new=TRUE)
{
    ## Works if "lastPar" is applied in postPlotNew (option 2)
    ## 'par' reuse is forced with user level 'plot.new' call.
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    hist(rnorm(100))
    par(mfrow = c(2, 2))
    for (i in 1:12)
        hist(rnorm(100))
    readline("4 tabs. First with 1 full plot, last 3 with 4 plots on each?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReusePar7 <- function(manual.new=TRUE)
{
    ## Works if "lastPar" is applied in postPlotNew (option 2)
    ## 'par' reuse is forced with user level 'plot.new' call.
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    plot.new()
    par(mfrow = c(2, 2))
    hist(rnorm(100))
    for (i in 1:12)
        hist(rnorm(100))
    readline("4 tabs. First 3 with 4 plots each, last with 1 top-left plot?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReuseNewClose <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", TRUE)
    for (i in 1:30)
        tabbedPlots.new()
    readline("GUI with 30 tabs?")
    for (i in 1:30)
        tabbedPlots.close()
    readline("GUI with 0 tabs?")
    for (i in 1:10)
        plot.new()
    readline("GUI with 10 tabs?")
    .set("reusePar", FALSE)
    tabbedPlots.quit()
}

.testReusePlotNew <- function(manual.new=TRUE)
{
    tabbedPlots.quit()
    .set("reusePar", TRUE)

    if (manual.new)
        plot.new()
    par(bg = "red")
    hist(runif(100))
    hist(runif(100))
    plot.new()
    hist(runif(100))
    tabbedPlots.new()
    hist(runif(100))

    .set("reusePar", FALSE)

    hist(runif(100))
    hist(runif(100))
    plot.new()
    hist(runif(100))
    tabbedPlots.new()
    hist(runif(100))

    readline("First 4 tab with red plots, last 4 tab with black plots?")

    tabbedPlots.quit()
}

## To use Lattice, you have to manually manage the tabs, i.e. explicitly create
## new tabs for new plots. You can do this with either 'tabbedPlots.new' or
## 'plot.new'. You also have to 'print' the Lattice return values in order to
## get behavior like the built-in 'plot'.
.testLattice <- function(manual.new=TRUE)
{
    require(lattice)
    require(stats)

    plot.new()
    print(histogram( ~ height | voice.part, data = singer, nint = 17,
                    endpoints = c(59.5, 76.5), layout = c(2,4), aspect = 1,
                    xlab = "Height (inches)"))
    plot.new()
    print(histogram( ~ height | voice.part, data = singer,
                    xlab = "Height (inches)", type = "density",
                    panel = function(x, ...) {
                        panel.histogram(x, ...)
                        panel.mathdensity(dmath = dnorm, col = "black",
                                          args = list(mean=mean(x),sd=sd(x)))
                    } ))

    tabbedPlots.new()
    print(densityplot( ~ height | voice.part, data = singer, layout = c(2, 4),
                      xlab = "Height (inches)", bw = 5))

    tabbedPlots.new()
    print(show.settings())

    readline("4 tabs with Lattice plots?")

    tabbedPlots.quit()
}

.testSaveAllDevices <- function(manual.new=TRUE)
{
    tabbedPlots.quit()

    hist(rnorm(100))

    devices <- c("bitmap","bmp","cairo_pdf","Cairo_pdf","Cairo_png",
                 "cairo_ps","Cairo_ps","Cairo_svg","jpeg","pdf","pictex",
                 "png","postscript","svg","tiff","xfig")

    for (device in devices)
    {
        path = tempfile()
        on.exit(try(file.remove(path), silent = TRUE), add = TRUE)

        devListBefore = dev.list()
        tabbedPlots.save(path, device)

        if (!file.exists(path))
            stop("Failed to save plot with device '", device, "'")
        else
            try(file.remove(path), silent = TRUE)

        if (!identical(dev.list(), devListBefore))
            stop("dev.list() is messed up")

        tabbedPlots.save(path, get(device))

        if (!file.exists(path))
            stop("Failed to save plot with device '", device, "'")
        else
            try(file.remove(path), silent = TRUE)

        if (!identical(dev.list(), devListBefore))
            stop("dev.list() is messed up")
    }

    tabbedPlots.quit()
}


.testSaveAllExtensions <- function(manual.new=TRUE)
{
    tabbedPlots.quit()

    hist(rnorm(100))

    for (extension in .get("knownExtensions"))
    {
        path = paste(tempfile(), extension, sep = ".")
        on.exit(try(file.remove(path), silent = TRUE), add = TRUE)

        devListBefore = dev.list()
        tabbedPlots.save(path)

        if (!file.exists(path))
            stop("Failed to save plot with extension '", extension, "'")
        else
            try(file.remove(path), silent = TRUE)

        if (!identical(dev.list(), devListBefore))
            stop("dev.list() is messed up")
    }

    tabbedPlots.quit()
}


