#' canceR Help
#' @usage canceRHelp()
#' @return html file  with tutorial
#' @export
#'
#' @examples
#' \dontrun{
#' canceRHelp()
#'}
#'


`canceRHelp` <-
    function() {
        tkgrab.release(window)
        helpIndex <- file.path(system.file(package="canceR"),"canceR.html")
        if (interactive()) browseURL(helpIndex)
    }
