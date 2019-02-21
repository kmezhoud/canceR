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
#'@importFrom utils vignette
#'
`canceR_Vignette` <-
    function() {
        tkgrab.release(window)
        #ViggIndex <- file.path(system.file("extdata",package="canceR"),"canceR.pdf")
        #Biobase::openPDF (ViggIndex) # opens the pdf
        browseURL('http://www.bioconductor.org/packages/3.9/bioc/vignettes/canceR/inst/doc/canceR.pdf')
        ## Morgan
        #utils::vignette(package="canceR", "canceR.Rnw")
    }