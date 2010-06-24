###########################################################################
##
##  P R I V A T E   V A R I A B L E S
##
###########################################################################

## The package private environment.
.env <- NULL

## Initialize private variables.
.initialize <- function(packagePath)
{
    .env <<- new.env()

    ## Path to package.
    .set("path", packagePath)
    
    ## These variables have package lifetime.
    ## If TRUE, tabbed plots are turned on.
    .set("active", TRUE)

    ## These variables have package lifetime.
    ## If TRUE, copy par from last plot into new plot.
    .set("reusePar", FALSE)
    
    ## If TRUE, print debugging information.
    .set("debug", FALSE)

    ## Initial width of plot.
    .set("width", 660)
    
    ## Initial height of plot.
    .set("height", 730)
    
    ## Default plot save DPI.
    .set("save.dpi", 72)
    
    ## Default plot save height.
    .set("save.height", 7)
    
    ## Default plot save width.
    .set("save.width", 7)
    
    ## Default plot save width.
    .set("save.pointsize", 10)
    
    ## TODO: This is a subset of what is available in R. The 'bitmap'
    ## device provides a bunch more.
    .set("knownExtensions",
            c("bmp", "eps", "jpg", "jpeg", "jpe", "jfif",  "jif",  "pdf",
              "png", "ps", "svg", "tex", "tif", "tiff", "xfig"))

    ## These variables have GUI lifetime.
    .reset()
}


## Initialize private variables with 
## GUI lifetime.
.reset <- function()
{
    .set("gui", NULL)
    .set("guiWindow", NULL)
    .set("notebook", NULL)

    ## Keep track of function call nesting. This is
    ## to make function call tracing output more
    ## readable.
    .set("nesting", -1)
    
    ## Keep track plot frames used by 'mfcol', 'mfrow' and 'layout'.
    ## We will assume that the plot is done when the last frame
    ## in the layout has been used. This will be true if frames are
    ## used in sequential order and the last one is used.
    ## TODO: We can probably keep better track of the frames.
    ## TODO: Support 'split.screen'.
    .set("plotDone", TRUE)
        
    ## Association of (one based) tab numbers to R device numbers.
    .set("devList", integer())

    ## Keep track of current page. One based.
    .set("curPage", 1)

    ## The plot is not finished yet. This can be because
    ## we have not gotten to the last figure in a multi-
    ## figure plot, or it was create with 'plot.new'.
    .set("plotNew", TRUE)
    
    ## Used when "reusePar" is TRUE to store 'par'
    ## from last tab. 
    .set("lastPar", NULL)

    ## Place to store the warnings for later retrieval.
    ## When some functions return FALSE, it is worth
    ## checking 'warning' for information on what
    ## went wrong.
    .set("warning", "")
}


