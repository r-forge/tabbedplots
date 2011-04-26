###########################################################################
##
##  P R I V A T E   F U N C T I O N S
##
###########################################################################

.warning <- function(..., warn = NULL)
{
    if (is.null(warn))
    {
        warning(..., call. = FALSE)
        return()
    }

    if (is.character(warn))
    {
        if (warn == "gui")
            .warnDialog(...)
        else if (warn == "store")
            .set("warning", ...)
        else if (warn == "ignore")
            return()

        return()
    }

    stop("Unknown 'warn' argument: '", warn, "'.")
}


.packageDescription <- function()
{
    fieldNames <- c("Package", "Title", "Version", "Date", "Author",
                    "Maintainer", "Description", "License", "Copyright")
    descPath <- file.path(.get("path"), "DESCRIPTION")
    desc <- as.list(read.dcf(descPath, fieldNames))
    desc = lapply(desc, gsub, pattern = "\n", replacement = " ")
    names(desc) <- fieldNames
    return(desc)
}


.addHighLevelPlotFunctionHooks <- function()
{
    .called()

    ## In order for special 'lm' plot to work right, we
    ## have to make sure that 'tabbedPlots' is loaded
    ## before 'stats'.
    ## This is really a hack and demonstrates a weakness
    ## using trace. There may well be other libraries
    ## that have to be unloaded like this. We cannot
    ## possible deal with them all here. The best thing
    ## to do, it to load "tabbedPlots" in the first line
    ## of your .Rprofile.
    statsIsAlreadyLoaded <- match("stats", loadedNamespaces(),
                                  nomatch = FALSE)
    if (statsIsAlreadyLoaded)
        try(unloadNamespace("stats"), silent = TRUE)

    ## Need here because we are called from .onLoad, where
    ## it might not be attached yet. It contains the high
    ## level plot funcitons we want to track.
    require("graphics")

    setHook("before.plot.new", tabbedPlots.prePlotNewHook)
    setHook("plot.new", tabbedPlots.postPlotNewHook)
    ## Old way was using trace() for hook:
    if (F) {
        ## This is extremely subtle. The 'expression' part deferres evaluation
        ## until later. Necessary to get the tracing right. '.GlobalEnv' is
        ## needed because these high level plotting functons end up there.
        trace("plot.new",
              expression(tabbedPlots.prePlotNewHook()),
              expression(tabbedPlots.postPlotNewHook()),
              print = FALSE, where = .GlobalEnv)
        trace("par", expression(tabbedPlots.parHook()),
              print = FALSE, where = .GlobalEnv)
        trace("layout", expression(tabbedPlots.layoutHook()),
              print = FALSE, where = .GlobalEnv)
    }

    if (statsIsAlreadyLoaded)
        require("stats", quietly = TRUE)
}

.removeHighLevelPlotFunctionHooks <- function()
{
    .called()

    untrace("plot.new", where = .GlobalEnv)
    untrace("par", where = .GlobalEnv)
    untrace("layout", where = .GlobalEnv)
}

.getExtension <- function(path, tolower = TRUE)
{
  ## Extract and return the extension part of a filename

  parts <- strsplit(path, "\\.")[[1]]
  if (length(parts) > 1)
    last <- parts[length(parts)]
  else
    last <- ""

  if (tolower)
      tolower(last)
}


## Merge names lists avoiding duplicate names.
## In the precense of duplicates, the firs argument
## is used.
.mergeLists <- function(a, b)
{
    c(a, b[!(names(b) %in% names(a))])
}

## Returns TRUE if tab was successfully created.
.newTab <- function()
{
    .in()

    ## We depends on device functions like 'dev.cur;.
    require("grDevices")

    ## Make sure the tabbed plots gui is up.
    if (!tabbedPlots())
    {
        .out()
        return(FALSE)
    }

    ## Save par from current tab, if there is one.
    if (.get("reusePar") && length(.get("devList")) > 0)
        .savePar()

    nPages <- .get("notebook")$GetNPages()

    drawingArea <- gtkDrawingArea()

    hBox <- gtkHBox()
    label1 <- gtkLabel(paste("Plot", nPages + 1))

    ## The close button is made of an empty button
    ## where we set a "close" image.
    button <- gtkButton()
    gSignalConnect(button, "clicked",
                   .onButtonClicked, drawingArea)
    image <- gtkImageNewFromStock(GTK_STOCK_CLOSE, GtkIconSize[["menu"]])
    button$SetImage(image)
    button$SetRelief(GtkReliefStyle[["none"]])

    hBox$PackStart(label1, TRUE, TRUE)
    hBox$PackEnd(button, FALSE, FALSE)

    label2 <- gtkLabel(paste("Plot", nPages + 1))

    .get("notebook")$AppendPageMenu(drawingArea, hBox, label2)

    .get("notebook")$SetCurrentPage(nPages)

    asCairoDevice(drawingArea)

    ## Make sure the device is ready.
    .processPendingEvents()

    ## Add device.
    .set("devList", dev.cur(), nPages + 1)

    ## We explictly created this tab emply, so
    ## we know we are not done.
    .set("plotDone", FALSE, .get("curPage"))

    .set("plotNew", TRUE)

    .out()
    return(TRUE)
}


.closeTab <- function(pageNum)
{
    if (pageNum != -1)
    {
        .get("notebook")$RemovePage(pageNum)

        ## Update lists.
        .rm("devList", pageNum + 1)
        .rm("plotDone", pageNum + 1)

        ## Sync gui up with R.
        .processPendingEvents()

        ## We switched page.
        if (.get("notebook")$GetNPages() < 1)
        {
            .set("plotDone", TRUE)
            .switchedPage(1)
        }
        else
            .switchedPage(.get("notebook")$GetCurrentPage() + 1)
    }
}


.quit <- function()
{
    if (!is.null(.get("gui")))
        .get("guiWindow")$destroy()

    .reset()
}


## Figure out if the user called 'plot.new' or is was called
## from a high-level plotting functions like 'plot' or 'hist'.
.calledFromHighLevelPlottingFunction <- function()
{
    stack <- sapply(sys.calls(), function(call)
                    { name = call[[1]]; if (is.name(name)) as.character(name) else "" })

    plotNewIsAt <- match("plot.new", stack)

    if (plotNewIsAt > 1)
    {
        callers <- stack[seq(1, plotNewIsAt - 1)]

        packages <- sapply(callers, function(caller) { where = find(caller); if (length(where)) where else ""})

        any(packages == "package:graphics")
    }
    else
        FALSE
}


## Check to see if gui is up and has pages.
.guiReady <- function()
{
    return (!is.null(.get("gui")) && !is.null(.get("notebook")) &&
            .get("notebook")$GetNPages() != 0)
}


## Save 'par'. Probably not all parameters are worth
## saving.
.savePar <- function()
{
    par <- par(no.readonly = TRUE)

    ## Skip 'pin' (the current plot dimensions)
    ## it they are negative.
    if (any(par$pin < 0))
        par <- par[-match("pin", names(par))]

    ## Skip 'plt' (fractions of current region)
    ## it they are negative.
    if (any(par$plt < 0))
        par <- par[-match("plt", names(par))]

    ## 'new' must be FALSE!
    par$new <- FALSE

    .set("lastPar", par)
}


## > .set("nesting", -1)
## > test <- function() { .in(); .out() }
## > test2 <- function() { .in(); test(); .out() }
## > test3 <- function() { .in(); test2(); .out() }
## > test3()
## >> test3
##   >> test2
##     >> test
##     << test
##   << test2
## << test3
.in <- function()
{
    if (.get("debug"))
    {
        calls <- sys.calls()
        name <- as.character(calls[[length(calls) - 1]])
        i <- .set("nesting", .get("nesting") + 1)
        while (i > 0)
        {
            cat("  ")
            i <- i - 1
        }
        cat(">>", name, "\n")
    }
}

.out <- function()
{
    if (.get("debug"))
    {
        calls <- sys.calls()
        name <- as.character(calls[[length(calls) - 1]])
        i <- .get("nesting")
        while (i > 0)
        {
            cat("  ")
            i <- i - 1
        }
        cat("<<", name, "\n")
        .set("nesting", .get("nesting") - 1)
    }
}

.called <- function(name = NULL)
{
    if (.get("debug"))
    {
        calls <- sys.calls()
        if (is.null(name))
            name <- as.character(calls[[length(calls) - 1]])
        i <- .get("nesting")
        while (i > 0)
        {
            cat("  ")
            i <- i - 1
        }

        cat("<>", name, "\n")
    }
}


.processPendingEvents <- function()
{
    while (gtkEventsPending())
        gtkMainIteration()
}

## The page was switched. Update our
## tracking variable and current
## device.
.switchedPage <- function(pageNum)
{
    .set("curPage", pageNum)

    if (pageNum > 0 && pageNum <= length(.get("devList")))
    {
        dev <- .get("devList", pageNum)

        if (is.integer(dev))
            dev.set(dev)
    }
}

.mapExtensionToDevice <- function(extension, warn)
{
    if (nchar(extension) < 2)
    {
        .warning("File name must have a valid extension, if device is not specified.", warn)
        return(NULL)
    }

    ## The choice here is random.
    ## TODO: Optimize the choice.
    device <- switch(extension, bmp = "bmp", pdf = "pdf",
                     svg = "svg",
                     jpe = "jpeg", jpg = "jpeg", jpeg = "jpeg",
                     jfif = "jpeg", jif = "jpeg",
                     tex = "pictex", png = "png",
                     ps = "postscript", eps = "postscript",
                     tif = "tiff", tiff = "tiff", xfig = "xfig")

    if (is.null(device))
        .warning("Unknown file extension.", warn)

    device
}
