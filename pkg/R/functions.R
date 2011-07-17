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

    ## Need here because we are called from .onLoad, where
    ## it might not be attached yet. It contains the high
    ## level plot funcitons we want to track.
    require("graphics")

    setHook("before.plot.new", tabbedPlots.prePlotNewHook)
    setHook("plot.new", tabbedPlots.postPlotNewHook)
}

.removeHighLevelPlotFunctionHooks <- function()
{
    .called()

    ## Remove just the hooks we added
    hooks <- getHook("before.plot.new")
    hooks <- hooks[!sapply(hooks, identical, tabbedPlots.prePlotNewHook)]
    setHook("before.plot.new", NULL, "replace")
    for (hook in hooks) setHook("before.plot.new", hook, "append")

    hooks <- getHook("plot.new")
    hooks <- hooks[!sapply(hooks, identical, tabbedPlots.postPlotNewHook)]
    setHook("plot.new", NULL, "replace")
    for (hook in hooks) setHook("plot.new", hook, "append")
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
.newTab <- function(label=NULL, warn=NULL)
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

    nPages <- .getCurNotebook()$GetNPages()
    plotNum <- .get("plotNum") + 1
    .set("plotNum", plotNum)

    plotName <- if (!is.null(label)) label else paste("Plot", plotNum)

    drawingArea <- gtkDrawingArea()
    assign("da.new", pos=1, drawingArea)

    hBox <- gtkHBox()
    label1 <- gtkLabel(plotName)

    closeButtonOnTab <- .get("closeButtonOnTab")
    if (closeButtonOnTab) {
        ## The close button is made of an empty button
        ## where we set a "close" image.
        ## Tends to be too easy to click accidentally, so it is optional
        button <- gtkButton()
        gSignalConnect(button, "clicked", .onButtonClicked, drawingArea)
        image <- gtkImageNewFromStock(GTK_STOCK_CLOSE, GtkIconSize[["menu"]])
        button$SetImage(image)
        button$SetRelief(GtkReliefStyle[["none"]])
    }

    hBox$PackStart(label1, TRUE, TRUE)
    ## Attach the plot area as a tag so that we can easily get it when
    ## we want to print or copy.
    tag(hBox, "plotarea") <- drawingArea
    if (closeButtonOnTab)
        hBox$PackEnd(button, FALSE, FALSE)

    label2 <- gtkLabel(plotName)

    .getCurNotebook()$AppendPageMenu(drawingArea, hBox, label2)

    .getCurNotebook()$SetCurrentPage(nPages)

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
        .getCurNotebook()$RemovePage(pageNum)

        ## Update lists.
        .rm("devList", pageNum + 1)
        .rm("plotDone", pageNum + 1)

        ## Sync gui up with R.
        .processPendingEvents()

        ## We switched page.
        if (.getCurNotebook()$GetNPages() < 1)
        {
            .set("plotDone", TRUE)
            .switchedPage(1)
        }
        else
            .switchedPage(.getCurNotebook()$GetCurrentPage() + 1)
    }
}


.quit <- function()
{
    if (!is.null(.get("guiWindow")))
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
    return (!is.null(.get("guiWindow")) && !is.null(.getCurNotebook()) &&
            .getCurNotebook()$GetNPages() != 0)
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

.getCurNotebook <- function(guiWindow=NULL) {
    if (is.null(guiWindow))
        guiWindow <- .get("guiWindow")
    notebook <- tag(guiWindow, "notebook")
    if (is.null(notebook))
        stop("notebook tag on guiWindow is NULL!")
    notebook
}
