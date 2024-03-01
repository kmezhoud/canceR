#' User needs to specify which gene is interesting to get genomic cancer data. The gene must be with Symbol and one gene by line.
#' @usage getGeneList()
#' @return Gene list path of file
#' @export
#' @examples
#' ENV <- new.env(parent = emptyenv())
#' \dontrun{
#' getGeneList()
#'  }
getGeneList <- function(){
   if(exists("GeneListMSigDB", envir=ENV)){
       rm("GeneListMSigDB", envir=ENV)
   }
       
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(exists("Genelistfile", envir = ENV)){
        rm(ENV$GeneList)
        rm(ENV$GeneListfile)
    }
    ENV$GeneListfile <- tclvalue(tkgetOpenFile(filetypes = "{{txt Files} {.txt}} {{All files} *}", title="Choose Gene List from File")) 
    print(ENV$GeneListfile)
    if (!nchar(ENV$GeneListfile)) {
        tkmessageBox(message = "No file was selected!")
        tkfocus(ENV$ttCasesGenProfs)
    } else {
        #Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        ENV$GeneList<-unique(read.table(ENV$GeneListfile))
        
       
        ENV$GeneList <- t(ENV$GeneList)
        tkmessageBox(message = paste("The file selected is", basename(ENV$GeneListfile),"with", length(t(ENV$GeneList))," genes"),icon="info")
        tkfocus(ENV$ttCasesGenProfs)
       
    }
    
}