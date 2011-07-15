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

    ## Obtain filename to save to.
    path <- .saveFileDialog(window)
    if (is.null(path))
        return()

    if (file.exists(path))
        if (is.null(.questionDialog("The file", path,
                                    "already exists. Do you want to overwrite",
                                    "this file?")))
            return()

    ## Ask for some more information.
    options <- .saveOptionsDialog(window, path)
    if (is.null(options))
        return()

    ## Do the save.
    args <- c(list(path, warn = "gui"), options)
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

    tabbedPlots.copy(warn = "gui")
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

    pageNum <- .get("notebook")$PageNum(data)
    .closeTab(pageNum)
}

