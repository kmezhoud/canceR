#' Dialog Box to  Select MSigDB Files from drive 
#' @usage
#' getMSigDBfile()
#' 
#' @return A path of MSigDB file
#' @export
#'
#' @examples
#' f <- 5+2
#' \dontrun{
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/prad_michPhenoTest1021.rds", sep=""))
#' geteSet()
#' getMSigDBfile()
#' }
getMSigDBfile <- function(){
    
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    myGlobalEnv$MSigDBPath <- tclvalue(tkgetOpenFile(filetypes = "{{gmt Files} {.gmt}} {{All files} *}", title="Select MSigDB with Symbol Gene")) 
    
    if (!nchar(myGlobalEnv$MSigDBPath)) {
        tkmessageBox(message = "No file was selected!")
    } else {
        Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        myGlobalEnv$mSigDB<-readLines(myGlobalEnv$MSigDBPath)
        tkmessageBox(message = paste("The selected file was", basename(myGlobalEnv$MSigDBPath)))
    }
    
}