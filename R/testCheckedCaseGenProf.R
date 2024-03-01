#' Testing checked appropriate Cases for appropriate Genetic profiles.
#' @param singleGene specify if the check for querying genetic profile for a specific gene or not (0,1).
#' @usage testCheckedCaseGenProf(singleGene=0)
#' @return dialog box with warning message
#' 
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' \dontrun{
#' testCheckedCaseGenProf(singleGene=0)
#' }
#' @export
testCheckedCaseGenProf <- function(singleGene=0){
    
if(!exists("curselectCases", envir = ENV)){
    msgNoCaseChoice= paste("Select at least ONE Case and ONE Genetic Profile")
    tkmessageBox(message= msgNoCaseChoice)
    stop(msgNoCaseChoice)
    
} else if (!exists("curselectGenProfs", envir = ENV)){
    
    msgNoGenProfChoice= paste("Select at least ONE Genetic Profile")
    tkmessageBox(message= msgNoGenProfChoice)
    stop(msgNoGenProfChoice)
    
} else if (!exists("GeneList", envir = ENV) && singleGene==0){
    msgNoGeneList= paste("Load Gene List (HUGO)")
    tkmessageBox(message= msgNoGeneList, icon="info")
    stop(msgNoGeneList)
    
} else {
    ######## Verify the number of checked cases and checked GenProfs.
    #If they not have the same length, take this message:
    if(length(ENV$curselectCases) != length(ENV$curselectGenProfs))
    {
        msgNotEqual<-"Select EQUAL number of Cases and Genetic Profiles for every Study!"
        tkmessageBox(message=msgNotEqual, icon="warning")
        tkfocus(ENV$ttCasesGenProfs)
        stop("Select EQUAL number of Cases and Genetic Profiles for every Study!")
    } 
    ######### Test if all cases were corresponded to appropriate Gen profs (same Study)
    for (i in seq(ENV$curselectCases)){
        if(ENV$StudyRefCase[i]!= ENV$StudyRefGenProf[i]){
            
            msgBadChoice="Correspond the Genetic Profile to the Case for the same Study"
            tkmessageBox(message=msgBadChoice, icon="warning")
            tkfocus(ENV$ttCasesGenProfs)
            stop("Correspond the Genetic Profile to the Case for the same Study")
        } 
    }
}
}