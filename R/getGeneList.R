getGeneList <- function(){
   
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(exists("Genelistfile", envir = myGlobalEnv)){
        rm(myGlobalEnv$GeneList)
        rm(myGlobalEnv$GeneListfile)
    }
    myGlobalEnv$GeneListfile <- tclvalue(tkgetOpenFile(filetypes = "{{txt Files} {.txt}} {{All files} *}", title="Choose Gene List from File")) 
    print(myGlobalEnv$GeneListfile)
    if (!nchar(myGlobalEnv$GeneListfile)) {
        tkmessageBox(message = "No file was selected!")
        tkfocus(myGlobalEnv$ttCasesGenProfs)
    } else {
        Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        myGlobalEnv$GeneList<-unique(read.table(myGlobalEnv$GeneListfile))
        
       
        myGlobalEnv$GeneList <- t(myGlobalEnv$GeneList)
        tkmessageBox(message = paste("The file selected is", basename(myGlobalEnv$GeneListfile),"with", length(t(myGlobalEnv$GeneList))," genes"),icon="info")
        tkfocus(myGlobalEnv$ttCasesGenProfs)
       
    }
    
}