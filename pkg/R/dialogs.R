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

    ## Method: dev.copy vs pixbuf
    method <- gtkHBox(show = TRUE)
    dev.copy.check <- gtkCheckButton("Use dev.copy() ?")
    method$packStart(dev.copy.check, TRUE, TRUE, 0)
    dialog$setExtraWidget(method)

    ## Quality
    quality <- gtkHBox(show = TRUE)
    qualityLabel <- gtkLabel("_Quality (jpeg only): ")
    qualityLabel$setWidthChars(9)
    qualityLabel$setUseUnderline(TRUE)
    qualityLabel$setAlignment(1, 0.5)
    quality$packStart(qualityLabel, FALSE, FALSE, 0)
    qualityAdj <- gtkAdjustment(0, 10, 100, 10, 30, 30)
    qualitySpin <- gtkSpinButton(qualityAdj, 1, 0, show = TRUE)
    qualitySpin$value <- .get("save.quality")
    quality$packStart(qualitySpin, TRUE, TRUE, 0)
    dialog$vbox$add(quality)

    while (!done)
    {
        if (dialog$run() == GtkResponseType["accept"])
        {
            path <- dialog$getFilename()
            ext <- .getExtension(path)
            use.dev.copy <- dev.copy.check$active
            quality <- qualitySpin$value

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

    list(path=path, dev.copy=use.dev.copy, quality=quality)
}

.saveOptionsDialog <- function(parent = .get("guiWindow"), path, quality=NULL, drawingArea=NULL)
{
    ## Attempt to figure out what options we can use.
    useDPI <- FALSE
    useQuality <- FALSE

    extension = .getExtension(path)

    if (!is.null(extension))
    {
        device <- .mapExtensionToDevice(extension, warn = "ignore")
        if (!is.null(device))
            useDPI <- "res" %in% names(formals(device))

        if (extension == "jpg")
            useQuality <- TRUE
    }

    da.alloc <- drawingArea$getAllocation()$allocation

    ## Create dialog.
    dialog <- gtkDialog("Save Options", parent, "modal",
                        GTK_STOCK_OK, GtkResponseType["accept"],
                        GTK_STOCK_CANCEL, GtkResponseType["reject"],
                        show = FALSE)
    dialog$setDefaultResponse(GtkResponseType["accept"])

    ## Method: dev.copy vs pixbuf
    method <- gtkHBox(show = TRUE)
    dev.copy.check <- gtkCheckButton("Use dev.copy() ?")
    method$packStart(dev.copy.check, TRUE, TRUE, 0)
    dialog$vbox$add(method)

    ## Units
    units <- gtkHBox(show = TRUE)
    unitsLabel <- gtkLabel("_Units: ")
    unitsLabel$setWidthChars(9)
    unitsLabel$setUseUnderline(TRUE)
    unitsLabel$setAlignment(1, 0.5)
    units$packStart(unitsLabel, FALSE, FALSE, 0)
    unitsCombo <- gtkComboBoxNewText()
    unitsChoices <- c("pixels"="px", "inches"="in", "cm"="cm", "mm"="mm")
    for (u in names(unitsChoices)) unitsCombo$appendText(u)
    unitsCombo$setActive(0)
    units$packStart(unitsCombo, TRUE, TRUE, 0)
    dialog$vbox$add(units)

    ## Width
    width <- gtkHBox(show = TRUE)
    widthLabel <- gtkLabel("_Width: ")
    widthLabel$setWidthChars(9)
    widthLabel$setUseUnderline(TRUE)
    widthLabel$setAlignment(1, 0.5)
    width$packStart(widthLabel, FALSE, FALSE, 0)
    # widthAdj <- gtkAdjustment(0, 0.01, 100, 0.01, 1, 1) # adjustment for inches
    widthAdj <- gtkAdjustment(0, 1, 10000, 1, 100, 100)
    widthSpin <- gtkSpinButton(widthAdj, 1, 2, show = TRUE)
    widthSpin$value <- da.alloc$width # .get("save.width")
    width$packStart(widthSpin, TRUE, TRUE, 0)
    dialog$vbox$add(width)

    ## Height
    height <- gtkHBox(show = TRUE)
    heightLabel <- gtkLabel("_Height: ")
    heightLabel$setWidthChars(9)
    heightLabel$setUseUnderline(TRUE)
    heightLabel$setAlignment(1, 0.5)
    height$packStart(heightLabel, FALSE, FALSE, 0)
    heightAdj <- gtkAdjustment(0, 1, 10000, 1, 100, 100)
    heightSpin <- gtkSpinButton(heightAdj, 1, 2, show = TRUE)
    heightSpin$value <- da.alloc$height # .get("save.height")
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

    if (useQuality)
    {
        ## Quality
        quality <- gtkHBox(show = TRUE)
        qualityLabel <- gtkLabel("_Quality: ")
        qualityLabel$setWidthChars(9)
        qualityLabel$setUseUnderline(TRUE)
        qualityLabel$setAlignment(1, 0.5)
        quality$packStart(qualityLabel, FALSE, FALSE, 0)
        qualityAdj <- gtkAdjustment(0, 10, 100, 10, 30, 30)
        qualitySpin <- gtkSpinButton(qualityAdj, 1, 0, show = TRUE)
        qualitySpin$value <- if (!is.null(quality)) quality else .get("save.quality")
        quality$packStart(qualitySpin, TRUE, TRUE, 0)
        dialog$vbox$add(quality)
    }

    ## Show
    dialog$show()

    result <- dialog$run()
    options <- NULL

    if (result == GtkResponseType["accept"])
    {
        options <- list(use.dev.copy = dev.copy.check$active,
                        units = unitsChoices[unitsCombo$active+1],
                        width = widthSpin$value,
                        height = heightSpin$value,
                        pointsize = pointsizeSpin$value,
                        drawingArea = drawingArea)
        if (useDPI)
            options[["res"]] = dpiSpin$value
        if (useQuality)
            options[["quality"]] = qualitySpin$value
    }

    dialog$destroy()
    if (class(dialog)!="<invalid>") Sys.sleep(1)
    if (class(dialog)!="<invalid>") Sys.sleep(1)
    options
}
