#' open pdf vignette
#' @usage
#' canceR_Vignette()
#'
#' @return open pdf vignette
#' @export
#'
#' @examples
#' \dontrun{
#' canceR_Vignette()
#'}
#'
#'@importFrom Biobase openPDF
#'
`canceR_Vignette` <-
    function() {
        tkgrab.release(window)
        ViggIndex <- file.path(system.file("extdata",package="canceR"),"canceR.pdf")
        Biobase::openPDF (ViggIndex) # opens the pdf
    }