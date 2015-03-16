`canceR_Vignette` <-
    function() {
        tkgrab.release(window)
        ViggIndex <- file.path(system.file("doc",package="canceR"),"canceR.pdf")
        openPDF (ViggIndex) # opens the pdf
    }