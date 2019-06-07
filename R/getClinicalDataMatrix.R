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
    if(exists("ClinicalDataMatrixfile", envir = myGlobalEnv)){
        rm(myGlobalEnv$ClinicalDataMatrixfile)
    }
    myGlobalEnv$ClinicalDataMatrixfile <- tclvalue(tkgetOpenFile(filetypes = "{{txt Files} {.txt}} {{All files} *}", title="Choose Gene Expression Matrix from File")) 
    
    if (!nchar(myGlobalEnv$ClinicalDataMatrixfile)) {
        tkmessageBox(message = "No file was selected!")
    } else {
        Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        myGlobalEnv$ClinicalData<-read.table(myGlobalEnv$ClinicalDataMatrixfile)
       
        tkmessageBox(message = paste("The file selected was", myGlobalEnv$ClinicalDataMatrixfile))
        tkfocus(ttMain)
    }
    
}