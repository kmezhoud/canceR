#' canceR Help
#' @usage canceRHelp()
#' @return html file  with tutorial
#' @export
#'
#' @examples
#' canceRHelp()
#'


`canceRHelp` <-
    function() {
        tkgrab.release(window)
        helpIndex <- file.path(system.file(package="canceR"),"canceR.html")
        browseURL(helpIndex)
    }
