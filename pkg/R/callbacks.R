###########################################################################
##
##  S I G N A L   C A L L B A C K S
##
###########################################################################

.onNewActivate <- function(action, window)
{
    .called(".onNewActivate")
    tabbedPlots.new()
}


.onCloseActivate <- function(action, window)
{
    ## close just the active plot
    .called(".onCloseActivate")
    tabbedPlots.close()
}


.onSaveActivate <- function(action, window)
{
    .called(".onSaveActivate")

    if (length(.get("devList")) < 1)
    {
        .warnDialog("Nothing to save!")
        return()
    }

    notebook <- .getCurNotebook()
    curPage <- .get("curPage")
    drawingArea <- tag(notebook$GetTabLabel(notebook$GetNthPage(curPage-1)), "plotarea")

    ## Obtain filename to save to.
    pathOpts <- .saveFileDialog(window)
    path <- pathOpts$path
    if (is.null(path))
        return()

    if (file.exists(path))
        if (is.null(.questionDialog("The file", path,
                                    "already exists. Do you want to overwrite",
                                    "this file?")))
            return()

    ## Ask for some more information.
    if (pathOpts$dev.copy) {
        options <- .saveOptionsDialog(window, path, drawingArea=drawingArea)
        if (is.null(options))
            return()
    } else {
        options <- list()
    }

    ## Do the save.
    args <- c(list(path, warn = "gui", drawingArea=drawingArea), options)
    do.call("tabbedPlots.save", args)
}

.onPrintActivate <- function(action, window)
{
    .called(".onPrintActivate")

    if (length(.get("devList")) < 1)
    {
        .warnDialog("Nothing to print!")
        return()
    }

    tabbedPlots.print(warn = "gui")
}

.onCopyActivate <- function(action, window)
{
    .called(".onCopyActivate")

    if (length(.get("devList")) < 1)
    {
        .warnDialog("Nothing to copy!")
        return()
    }

    browser()

    tabbedPlots.copy(warn = "gui")
}

.onDevSetCurActivate <- function(action, window)
{
    .called(".onDevSetCurActivate")

    if (length(.get("devList")) < 1)
    {
        .warnDialog("No existing graphics devices!")
        return()
    }

    notebook <- .getCurNotebook()
    curPage <- .get("curPage")
    .set("plotDone", FALSE, curPage)

    devList <- .get("devList")
    dev.set(devList[curPage])
}

.onDevSetNewActivate <- function(action, window)
{
    .called(".onDevSetNewActivate")

    tabbedPlots.new(warn = "gui")
}

.onQuitActivate <- function(action, window)
{
    .called(".onQuitActivate")

    .quit()
}


.onAboutActivate <- function(action, window)
{
    .called(".onAboutActivate")

    .aboutDialog(window)
}


.onTabbedPlotsDestroy <- function(action, window)
{
    .called(".onTabbedPlotsDestroy")

    .reset()
}


.onNotebookSwitchPage <- function(action, window, pageNum)
{
    if (!.guiReady())
        return

    ## 'pageNum' is zero based!

    .called(sprintf(".onNotebookSwitchPage. pageNum: %d", pageNum))

    ## Now one based.
    pageNum <- pageNum + 1

    .switchedPage(pageNum)
}


.onButtonClicked <- function(button, data)
{
    .called(".onButtonClicked")

    pageNum <- .getCurNotebook()$PageNum(data)
    .closeTab(pageNum)
}

