#' Testing checked appropriate Cases for appropriate Genetic profiles.
#' @usage testCheckedCaseGenProf()
#' @return dialog box with warning message
#' @export
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' \dontrun{
#' testCheckedCaseGenProf()
#' }
testCheckedCaseGenProf <- function(){
    
if(!exists("curselectCases", envir = myGlobalEnv)){
    msgNoCaseChoice= paste("Select at least ONE Case and ONE Genetic Profile")
    tkmessageBox(message= msgNoCaseChoice)
    stop(msgNoCaseChoice)
    
} else if (!exists("curselectGenProfs", envir = myGlobalEnv)){
    
    msgNoGenProfChoice= paste("Select at least ONE Genetic Profile")
    tkmessageBox(message= msgNoGenProfChoice)
    stop(msgNoGenProfChoice)
    
    
    
} else if (!exists("GeneList", envir = myGlobalEnv)){
    msgNoGeneList= paste("Load Gene List (HUGO)")
    tkmessageBox(message= msgNoGeneList, icon="info")
    stop(msgNoGeneList)
} else {
    
    Lchecked_Studies <- myGlobalEnv$lchecked_Studies_forCases
    Lchecked_Cases <- length(myGlobalEnv$curselectCases)
    Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
    
    ######## Verify the number of checked cases and checked GenProfs.
    #If they not have the same length, take this message:
    if(Lchecked_Cases != Lchecked_GenProf)
    {
        msgNotEqual<-"Select EQUAL number of Cases and Genetic Profiles for every Study!"
        tkmessageBox(message=msgNotEqual, icon="warning")
        tkfocus(ttCasesGenProfs)
        stop("Select EQUAL number of Cases and Genetic Profiles for every Study!")
        
    } 
    
    ######### Test if all cases were corresponded to appropriate Gen profs (same Study)
    for (i in 1:Lchecked_Cases){
        if(myGlobalEnv$StudyRefCase[i]!= myGlobalEnv$StudyRefGenProf[i]){
            
            msgBadChoice="Correpond the Genetic Profile to the Case for the same Study"
            tkmessageBox(message=msgBadChoice, icon="warning")
            tkfocus(myGlobalEnv$ttCasesGenProfs)
            stop("Correspond the Genetic Profile to the Case for the same Study")
            
        } 
        
    }
    
}
}