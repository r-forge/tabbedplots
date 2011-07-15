##
## Tabbed Plots for R. Manages plot devices and shows them in a
## GUI with a device in each tab.
##
## TODO: Add support for persp, lattice, split.screen.
##


###########################################################################
##
##  P U B L I C   F U N C T I O N S
##
###########################################################################

tabbedPlots.options <- function(...)
{
    knowOptions <- c("active", "debug", "height", "reusePar", "width")

    if (length(list(...)) == 0)
    {
        options = list()

        for (i in seq(along = knowOptions))
            options[[i]] <- .get(knowOptions[[i]])

        names(options) <- knowOptions
        options
    }
    else
    {
        options <- list(...)

        for (option in names(options))
            if (is.na(match(option, knowOptions)))
                warning("unknown option ", option)
            else
                .set(option, options[[option]])
    }
}


## tabbedPlotsHook <- function()
## {
##     .in()
##
##     ## Remove the device that was just created, if tabbedPlots
##     ## GUI is not up.
##
##     if (!exists(".gui") || is.null(.gui))
##     {
##         cat("Calling dev.off\n")
##         dev.off()
##     }
##
##     ## Create GUI if necessary and add a new tab with our own device.
##     if (is.null(.gui))
##         tabbedPlots()
##     else
##         tabbedPlots.new()
##
##     ## Make sure that plot.new is called with
##     ## the new device in place.
##     setHook("plot.new", NULL, "replace")
##     plot.new()
##     setHook("plot.new", tabbedPlotsHook)
##
##     .out()
## }


tabbedPlots.prePlotNewHook <- function()
{
    if (!.get("active"))
        return()

    .in()

    .set("plotNew", FALSE)

    ## It the user called 'plot.new' he must want a new
    ## tab eventhought the last one is not done.
    highLevelPlotNew = .calledFromHighLevelPlottingFunction()

    ## Create a new tab (plot), if we need to.
    if (.get("plotDone", .get("curPage")) || !highLevelPlotNew)
    {
        if (.newTab())
        {
            ## Set 'par' form last tab. This can either be
            ## done here in 'prePlotNewHook' or in 'postPlotNewHook'.
            ## Turns out that if we are in a high level plot
            ## it *must* be done here. If the user did a
            ## 'plot.new', we *have to* do it in 'postPlotNewHook'
            ## I call this 'Option 1'.
            if (.get("reusePar") &&
                .get("plotNew") &&
                !is.null(.get("lastPar")) &&
                highLevelPlotNew)
            {
                par(.get("lastPar"))
            }
        }
    }

    .out()
}


tabbedPlots.postPlotNewHook <- function()
{
    if (!.get("active"))
        return()

    .called()

    ## See if we are on the last figure. The plot is done
    ## if we have filled out all the figures in an array
    ## of figures.
    lastFigure <- identical(par("mfg")[1:2], par("mfg")[3:4])

    ## If a high-level plotting functions like 'plot' or 'hist'
    ## called 'plot.new' we are done.
    highLevelPlotNew = .calledFromHighLevelPlottingFunction()

    ## Set 'par' form last tab. This can either be
    ## done here in 'postPlotNewHook' or in 'prePlotNewHook'.
    ## Turns out that if we are in a high level plot
    ## it *must* be done in 'prePlotNewHook'. If the user
    ## did a 'plot.new', we *have to* do it here.
    ## I call this 'Option 2'.
    if (.get("reusePar") &&
        .get("plotNew") &&
        !is.null(.get("lastPar")) &&
        !highLevelPlotNew)
    {
        par(.get("lastPar"))
    }

    .set("plotDone", highLevelPlotNew && lastFigure, .get("curPage"))
}


tabbedPlots.parHook <- function()
{
    if (!.get("active"))
        return()

    .called()

    if (!.guiReady())
    {
        ## "par" was called before the gui was put up or
        ## the gui has no pages.
        ## Create the gui and an empty tab to be filled in later.
        tabbedPlots.new()
        .set("plotDone", FALSE, .get("curPage"))
    }
}


tabbedPlots.layoutHook <- function()
{
    if (!.get("active"))
        return()

    .called()

    if (!.guiReady())
    {
        ## "layout" was called before the gui was put up.
        ## Create the gui and an empty tab to be filled in later.
        tabbedPlots.new()
        .set("plotDone", FALSE, .get("curPage"))
    }
}

## Returns TRUE if GUI was successfully created.
tabbedPlots <- function()
{
    .in()

    if (!is.null(.get("gui")))
    {
        .out()
        return(TRUE)
    }

    ## Create main GUI.
    gui <- .createGui()

    if (is.null(gui))
    {
        .errorDialog(paste("Failed to create tabbedPlots GUI!",
                           "Please check your GTK installation."))
        .set("active", FALSE)

        .out()
        return(FALSE)
    }

    ## Sync gui up with R.
    .processPendingEvents()

    .out()
    return(TRUE)
}

## New tab. Just like '.tabNew' except it
## can reuses the previous 'par'.
tabbedPlots.new <- function()
{
    if (.newTab())
    {
        ## Set 'par' form last tab.
        if (.get("reusePar") && !is.null(.get("lastPar")))
        {
            par(.get("lastPar"))
        }
    }
}


## Close current tab
tabbedPlots.close <- function()
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages())
        return(FALSE)

    pageNum <- .get("notebook")$GetCurrentPage()
    .closeTab(pageNum)
    return(TRUE)
}


## Save the current plot to a file. There is an attempt to
## figure out reasonable arguments. An output device will be select
## based on extension, if one is not specified. Know devices:
## "bitmap","bmp","cairo_pdf","Cairo_pdf","Cairo_png","cairo_ps",
## "Cairo_ps","Cairo_svg","jpeg","pdf","pictex","png","postscript",
## "svg","tiff","xfig". Device arguments are passed though with "..."
## Notice that there is a log of redundency. Here are 3 diffeerent ways
## to generate a png:
## > tabbedPlots.save("lars1.png", bitmap, type = "png256")
## > tabbedPlots.save("lars2.png", png)
## > tabbedPlots.save("lars3.png", Cairo_png)
tabbedPlots.save <- function(file, device = NULL,
                             width = 7, height = 7, pointsize = 10, warn = NULL, ...)
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages())
        return(FALSE)

    extension = .getExtension(file)

    ## Check the arguments.
    if (is.null(device))
        device <- .mapExtensionToDevice(extension, warn)

    if (is.null(device))
        return(FALSE)

    if (is.character(device))
        device <- get(device)

    if (!is.function(device))
    {
        .warning("'device' must be either a character or function.", warn)
        return(FALSE)
    }

    ## Massage the arguments to fit the various types of devices.
    ## Example; some devices has an argument called "file" others
    ## "filename".
    if (!is.null(device))
    {
        deviceArgNames <- names(formals(device))

        myArgs <- c("file", "width", "height", "pointsize")
        myArgsIdx <- pmatch(myArgs, deviceArgNames, nomatch = 0)

        args <- list(device = device)

        for (i in seq(along = myArgs))
            if (myArgsIdx[i] > 0)
                args[deviceArgNames[myArgsIdx[i]]] = get(myArgs[i])

        if ("units" %in% deviceArgNames &&
            "res" %in% deviceArgNames)
        {
            args["units"] <- "in"
            args["res"] <- 72
        }

        ## This makes xfig "happy".
        if ("onefile" %in% deviceArgNames)
            args["onefile"] <- TRUE

        ## Deal with 'ps' vs. 'eps'.
        if (extension == "eps")
        {
            if ("horizontal" %in% deviceArgNames)
                args["horizontal"] <- FALSE
            if ("onefile" %in% deviceArgNames)
                args["onefile"] <- FALSE
            if ("paper" %in% deviceArgNames)
                args["paper"] <- "special"
        }

        args <- .mergeLists(list(...), args)

        do.call("dev.copy", args)

        dev.off()
    }
    return(TRUE)
}

## Primitive print.
## TODO: Use the GTK2 print functionality. This involves drawing into
## a Cairo priter device. It would then provide cool things like
## page setup, image settings, printer settings, print preview, etc.
## See Gimp "File/Print...".
tabbedPlots.print <- function(warn = NULL)
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages())
        return(FALSE)

    path = tempfile()
    on.exit(try(file.remove(path), silent = TRUE))

    tabbedPlots.save(path, "postscript", warn = warn)

    if (file.exists(path))
    {
        ret <- system(paste("lpr", path), intern = FALSE)
        if (ret == 0)
            return(TRUE)
        else
        {
            lpr = system("which lpr", intern = TRUE)
            .warning(lpr, "returned error code", ret, warn = warn)
        }
    }
    else
        .warning("Failed to generate Postscript file.", warn = warn)

    return(FALSE)
}

tabbedPlots.copy <- function(warn = NULL)
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages())
        return(FALSE)

    notebook <- .get("notebook")
    ## This copies the tab labels + the drawing area to the clipboard
    ## Want to find a way to get just the drawing area in the child
    daa <- notebook$getAllocation()$allocation
    pixbuf <- gdkPixbufGetFromDrawable(src=notebook$window, src.x=daa$x, src.y=daa$y, dest.x=0, dest.y=0,
                                       width=daa$width, height=daa$height)
    gtkClipboardGet("CLIPBOARD")$setImage(pixbuf)

    return(FALSE)
}


tabbedPlots.quit <- function()
{
    .quit()
}


tabbedPlots.nextTab <- function()
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages())
        return(FALSE)

    notebook <- .get("notebook")
    curPage <- notebook$GetCurrentPage()
    notebook$NextPage()

    ## Sync gui up with R.
    .processPendingEvents()
    return(curPage != notebook$GetCurrentPage())
}


tabbedPlots.prevTab <- function()
{
    if (is.null(.get("gui")) || !.get("notebook")$GetNPages())
        return (FALSE)

    notebook <- .get("notebook")
    curPage <- notebook$GetCurrentPage()
    notebook$PrevPage()

    ## Sync gui up with R.
    .processPendingEvents()
    return(curPage != notebook$GetCurrentPage())
}


tabbedPlots.warning <- function()
{
    msg <- .get("warning")
    .set("warning", "")
    msg
}


tabbedPlots.runTests <- function(tests = "ALL", manual.new=TRUE)
{
    if (tests == "ALL")
        tests <- c(".testNewCloseExit", ".testLayout", ".testComplexPlot",
                   ".testComplexPlotMultiFigure1",
                   ".testComplexPlotMultiFigure2",
                   ".testReusePar1", ".testReusePar2",
                   ".testReusePar3", ".testReusePar4",
                   ".testReusePar5", ".testReusePar6",
                   ".testReusePar7", ".testReuseNewClose",
                   ".testLattice",".testSaveAllDevices",
                   ".testSaveAllExtensions")

    ow <- options(warn = 2)
    on.exit(options(ow))

    for (test in tests)
    {
        cat("\nRunning test", test, "\n")
        do.call(test, list(manual.new=manual.new))
        # eval(call(test))
    }
}
