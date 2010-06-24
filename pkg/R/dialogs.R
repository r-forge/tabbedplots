###########################################################################
##
##  C O M M O N   D I A L O G S
##
###########################################################################

.warnDialog <- function(..., parent = .get("guiWindow"))
{
    dialog <- gtkMessageDialogNew(parent, "modal", "warn", "close",
                                  ...)
    connectSignal(dialog, "response", gtkWidgetDestroy)

    dialog$run()
}


.errorDialog <- function(..., parent = .get("guiWindow"))
{
    dialog <- gtkMessageDialogNew(parent, "modal", "error", "close",
                                  ...)
    connectSignal(dialog, "response", gtkWidgetDestroy)

    dialog$run()
}


.questionDialog <- function(..., parent = .get("guiWindow"))
{
    dialog <- gtkMessageDialogNew(parent, "modal", "question",
                                  "yes-no",
                                  ...)
    result <- dialog$run()

    dialog$destroy()
    if (result == GtkResponseType["yes"])
        return("yes")
    else
        return(NULL)
}


.aboutDialog <- function(parent = .get("guiWindow"))
{
    dialog <- gtkAboutDialog(show = FALSE)
    gtkWindowSetTransientFor(dialog, parent)

    desc <- .packageDescription()

    dialog$setProgramName(desc$Package)
    dialog$setVersion(desc$Version)
    dialog$setCopyright(desc$Copyright)
    dialog$setLicense("GPL")
    dialog$setComments(paste(desc$Title, desc$Description,
                             sep = ". "))
    dialog$setAuthors(desc$Maintainer)

    result <- dialog$run()

    dialog$destroy()
    if (result == GtkResponseType["yes"])
        return("yes")
    else
        return(NULL)
}

.saveFileDialog <- function(parent = .get("guiWindow"))
{
    dialog <- gtkFileChooserDialog("Save As...", parent, "save",
                                   "gtk-cancel", GtkResponseType["cancel"],
                                   "gtk-save", GtkResponseType["accept"])

    extensions <- list(c("BMP\t- Windows Bitmap", "bmp"),
                       c("EPS\t- Encapsulated PostScript", "eps"),
                       c("JPEG\t- Joint Photographic Experts Group",
                         "jpg", "jpeg", "jpe", "jfif", "jif"),
                       c("PDF\t- Portable Document Format", "pdf"),
                       c("PNG\t- Portable Network Graphics", "png"),
                       c("PS\t- PostScript document", "ps"),
                       c("SVG\t- Scalable Vector Graphics", "svg"),
                       c("TEX\t- PicTeX", "tex"),
                       c("TIFF\t- Tagged Image File Format", "tif","tiff"),
                       c("XFIG\t- X Facility for Interactive Generation of figures", "xfig"))

    for (ext in extensions)
    {
        ff <- gtkFileFilterNew()
        ff$setName(paste(ext[[1]], " (",
                         paste(paste("*.", ext[seq(2, length(ext))], sep = ""),
                               collapse = ", "),
                         ") ", sep = ""))
        for (pat in ext[seq(2, length(ext))])
            ff$addPattern(paste("*.", pat, sep = ""))
        dialog$addFilter(ff)
    }

    done <- FALSE
    path <- NULL

    while (!done)
    {
        if (dialog$run() == GtkResponseType["accept"])
        {
            path <- dialog$getFilename()

            ext = .getExtension(path)

            if (!(ext %in% .get("knownExtensions")))
                .warnDialog(paste("The given file name does not",
                                  "have any known file extension.",
                                  "Please enter a known file extension",
                                  "(see file type filter)."),
                            parent = dialog)
            else
            {
                dialog$destroy()
                done <- TRUE
            }
        }
        else
        {
            dialog$destroy()
            return(NULL)
        }
    }

    path
}

.saveOptionsDialog <- function(parent = .get("guiWindow"), path)
{
    ## Attempt to figure out what options we can use.
    useDPI <- FALSE

    extension = .getExtension(path)

    if (!is.null(extension))
    {
        device <- .mapExtensionToDevice(extension, warn = "ignore")
        if (!is.null(device))
            useDPI <- "res" %in% names(formals(device))
    }

    ## Create dialog.
    dialog <- gtkDialog("Save Options", parent, "modal",
                        GTK_STOCK_OK, GtkResponseType["accept"],
                        GTK_STOCK_CANCEL, GtkResponseType["reject"],
                        show = FALSE)
    dialog$setDefaultResponse(GtkResponseType["accept"])

    ## Width
    width <- gtkHBox(show = TRUE)
    widthLabel <- gtkLabel("_Width: ")
    widthLabel$setWidthChars(9)
    widthLabel$setUseUnderline(TRUE)
    widthLabel$setAlignment(1, 0.5)
    width$packStart(widthLabel, FALSE, FALSE, 0)
    widthAdj <- gtkAdjustment(0, 0.01, 100, 0.01, 1, 1)
    widthSpin <- gtkSpinButton(widthAdj, 1, 2, show = TRUE)
    widthSpin$value <- .get("save.width")
    width$packStart(widthSpin, TRUE, TRUE, 0)
    dialog$vbox$add(width)

    ## Height
    height <- gtkHBox(show = TRUE)
    heightLabel <- gtkLabel("_Height: ")
    heightLabel$setWidthChars(9)
    heightLabel$setUseUnderline(TRUE)
    heightLabel$setAlignment(1, 0.5)
    height$packStart(heightLabel, FALSE, FALSE, 0)
    heightAdj <- gtkAdjustment(0, 0.01, 100, 0.01, 1, 1)
    heightSpin <- gtkSpinButton(heightAdj, 1, 2, show = TRUE)
    heightSpin$value <- .get("save.height")
    height$packStart(heightSpin, TRUE, TRUE, 0)
    dialog$vbox$add(height)

    ## Pointsize
    pointsize <- gtkHBox(show = TRUE)
    pointsizeLabel <- gtkLabel("_Pointsize: ")
    pointsizeLabel$setWidthChars(9)
    pointsizeLabel$setUseUnderline(TRUE)
    pointsizeLabel$setAlignment(1, 0.5)
    pointsize$packStart(pointsizeLabel, FALSE, FALSE, 0)
    pointsizeAdj <- gtkAdjustment(0, 1, 100, 1, 10, 10)
    pointsizeSpin <- gtkSpinButton(pointsizeAdj, 1, 0, show = TRUE)
    pointsizeSpin$value <- .get("save.pointsize")
    pointsize$packStart(pointsizeSpin, TRUE, TRUE, 0)
    dialog$vbox$add(pointsize)

    if (useDPI)
    {
        ## Dpi
        dpi <- gtkHBox(show = TRUE)
        dpiLabel <- gtkLabel("_DPI: ")
        dpiLabel$setWidthChars(9)
        dpiLabel$setUseUnderline(TRUE)
        dpiLabel$setAlignment(1, 0.5)
        dpi$packStart(dpiLabel, FALSE, FALSE, 0)
        dpiAdj <- gtkAdjustment(0, 10, 1000, 1, 10, 10)
        dpiSpin <- gtkSpinButton(dpiAdj, 1, 0, show = TRUE)
        dpiSpin$value <- .get("save.dpi")
        dpi$packStart(dpiSpin, TRUE, TRUE, 0)
        dialog$vbox$add(dpi)
    }

    ## Show
    dialog$show()

    result <- dialog$run()
    options <- NULL

    if (result == GtkResponseType["accept"])
    {
        options <- list(width = widthSpin$value,
                        height = heightSpin$value,
                        pointsize = pointsizeSpin$value)
        if (useDPI)
            options[["res"]] = dpiSpin$value
    }

    dialog$destroy()
    options
}
