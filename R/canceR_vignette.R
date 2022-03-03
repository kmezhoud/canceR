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
        #ViggIndex <- file.path(system.file("extdata/canceR.Rmd",package="canceR"),"canceR.pdf")
        #Biobase::openPDF (ViggIndex) # opens the pdf
        browseURL('https://bioconductor.org/packages/3.14/bioc/vignettes/canceR/inst/doc/canceR.html')
        #rmarkdown::render(input = system.file("extdata/canceR.Rmd",package="canceR"), 'pdf_document')
        ## Morgan
        #utils::vignette(package="canceR", "canceR.Rmd")
    }


#' canceR Report Issue
#' @usage canceR_Issue()
#' @return link to github issues
#' @export
#'
#' @examples
#' \dontrun{
#' canceR_Issue()
#'}
#'


`canceR_Issue` <-
    function() {
        tkgrab.release(window)
        browseURL('https://github.com/kmezhoud/canceR/issues')
        
    }