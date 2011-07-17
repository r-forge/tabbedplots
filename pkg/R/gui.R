.createGui <- function(title=NULL)
{
    ## Create the actions
    entries <- list(
                    ## name, stock.id (prefab icons), label (. signifies mneumonic)
                    list("FileMenu", NULL, "_File"),
                    list("HelpMenu", NULL, "_Help"), # accelerator (kb shortcut), tooltip, callback
                    list("New", "gtk-new", "_New", "<control>N", "Create a new tab", .onNewActivate),
                    list("Close", "gtk-close", "_Close", "<control>W", "Close tab", .onCloseActivate),
                    list("Set Current", "", "S_et Current", "<control>E", "Set current tab to be active device", .onDevSetCurActivate),
                    list("Next", "gtk-next", "Next tab", "<right>", "Display the next tab", .onNextActivate),
                    list("Prev", "", "Prev tab", "<left>", "Display the prev tab", .onPrevActivate),
                    list("Save", "gtk-save-as", "_Save", "<control>S", "Save current tab", .onSaveActivate),
                    list("Copy", "gtk-copy", "_Copy", "<control>C", "Copy current tab to clipboard", .onCopyActivate),
                    list("Print", "gtk-print", "_Print", "<control>P", "Print current tab", .onPrintActivate),
                    list("Quit", "gtk-quit", "_Quit", "<control>Q", "Quit", .onQuitActivate),
                    list("About", "gtk-about", "_About", "<control>A", "About", .onAboutActivate)
                    )

    nWindow <- length(.get("windows"))+1
    ## Titles do not have to be unique among windows
    if (is.null(title)) title <- paste("R Tabbed Plots [", nWindow, "]", sep="")

    ## Create a window
    toplevel <- gtkWindowNew("toplevel", show = FALSE)
    ## toplevel$setPosition("GTK_WIN_POS_CENTER_ALWAYS")
    toplevel$setTitle(title)

    ## Add menu and notebook.
    ## topbar will contain menu and buttons
    vbox <- gtkVBoxNew(FALSE)
    topbar <- gtkHBoxNew(FALSE)
    toplevel$add(vbox)

    agroup <- gtkActionGroupNew("AppWindowActions")

    ## Add actions to group with the window widget passed to callbacks
    agroup$addActions(entries, toplevel)

    ## Create a UI manager to read in menus specified in XML
    manager <- gtkUIManagerNew()

    toplevel$setData("ui-manager", manager)
    manager$insertActionGroup(agroup, 0)

    toplevel$addAccelGroup(manager$getAccelGroup())

    ## Define some XML
    uistr <- paste(
                   "<ui>",
                   "  <menubar name='MenuBar'>",
                   "    <menu action='FileMenu'>",
                   "      <menuitem action='New'/>",
                   "      <menuitem action='Close'/>",
                   "      <separator/>",
                   "      <menuitem action='Save'/>",
                   "      <menuitem action='Copy'/>",
                   "      <menuitem action='Print'/>",
                   "      <separator/>",
                   "      <menuitem action='Set Current'/>",
                   "      <menuitem action='Next'/>",
                   "      <menuitem action='Prev'/>",
                   "      <separator/>",
                   "      <menuitem action='Quit'/>",
                   "    </menu>",
                   "    <menu action='HelpMenu'>",
                   "      <menuitem action='About'/>",
                   "    </menu>",
                   "  </menubar>",
                   "</ui>", sep="\n")

    manager$addUiFromString(uistr)
    menubar <- manager$getWidget("/MenuBar")
    # vbox$packStart(menubar, FALSE, TRUE, 0)
    topbar$packStart(menubar, FALSE, FALSE, 0)

    # Set up a copy button next to the menu
    close.button <- gtkButton()
    gSignalConnect(close.button, "clicked", .onCloseActivate, toplevel)
    close.button$SetImage(gtkImageNewFromStock(GTK_STOCK_CLOSE, GtkIconSize[["menu"]]))
    # close.button$SetText("Close")
    close.button$SetRelief(GtkReliefStyle[["none"]])
    close.button$SetTooltipText("Close active plot")
    topbar$PackEnd(close.button, FALSE, FALSE)

    copy.button <- gtkButton("Copy")
    gSignalConnect(copy.button, "clicked", .onCopyActivate, toplevel)
    # copy.button$SetImage(gtkImageNewFromStock(GTK_STOCK_COPY, GtkIconSize[["menu"]]))
    copy.button$SetRelief(GtkReliefStyle[["none"]])
    copy.button$SetTooltipText("Copy active plot to clipboard")
    topbar$PackEnd(copy.button, FALSE, FALSE)

    setcur.button <- gtkButton("Set")
    gSignalConnect(setcur.button, "clicked", .onDevSetCurActivate, toplevel)
    setcur.button$SetRelief(GtkReliefStyle[["none"]])
    setcur.button$SetTooltipText("Make the current tab the active graphics device")
    topbar$PackEnd(setcur.button, FALSE, FALSE)

    setnew.button <- gtkButton("New")
    gSignalConnect(setnew.button, "clicked", .onDevSetNewActivate, toplevel)
    setnew.button$SetRelief(GtkReliefStyle[["none"]])
    setnew.button$SetTooltipText("Make a new tab the active graphics device")
    topbar$PackEnd(setnew.button, FALSE, FALSE)

    vbox$packStart(topbar, FALSE, FALSE, 0)

    notebook <- gtkNotebook()
    notebook$setScrollable(TRUE)
    notebook$popupEnable()
    vbox$packStart(notebook, TRUE, TRUE, 0)

    ## More actions.
    gSignalConnect(notebook, "switch_page", .onNotebookSwitchPage)
    gSignalConnect(toplevel, "destroy", .onTabbedPlotsDestroy)

    toplevel$setDefaultSize(.get("width"), .get("height"))
    toplevel$showAll()
    tag(toplevel, "windowNum") <- nWindow
    tag(toplevel, "notebook") <- notebook

    .set("guiWindow", toplevel)
    ## the 'notebook' var should no longer be needed because it is a tag in guiWindow
    .set("notebook", notebook)
    .set("windows", toplevel, nWindow)

    toplevel
}

