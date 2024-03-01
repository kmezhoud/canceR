#' get matrix with clinical from file
#' @usage getClinicalDataMatrix()
#' @export
#' @return dataframe of clinicaldata
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' \dontrun{
#' getClinicalDataMatrix()
#' }
getClinicalDataMatrix <- function(){
 
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(exists("ClinicalDataMatrixfile", envir = ENV)){
        rm(ENV$ClinicalDataMatrixfile)
    }
    ENV$ClinicalDataMatrixfile <- tclvalue(tkgetOpenFile(filetypes = "{{txt Files} {.txt}} {{All files} *}", 
                                                         title="Choose Gene Expression Matrix from File")) 
    
    if (!nchar(ENV$ClinicalDataMatrixfile)) {
        tkmessageBox(message = "No file was selected!")
    } else {
        Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        ENV$ClinicalData<-read.table(ENV$ClinicalDataMatrixfile)
       
        tkmessageBox(message = paste("The file selected was", ENV$ClinicalDataMatrixfile))
        tkfocus(ttMain)
    }
    
}