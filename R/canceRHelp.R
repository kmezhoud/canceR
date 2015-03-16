`canceRHelp` <-
    function() {
        tkgrab.release(window)
        helpIndex <- file.path(system.file(package="canceR"),"canceR.html")
        browseURL(helpIndex)
    }
