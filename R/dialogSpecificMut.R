#' dialog box to Specify Mutation using Regular Expression. Search specific mutation using regular expression.
#' @usage
#' getSpecificMut()
#'
#' @return a a dataframe with specific mutation informations
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' getSpecificMut()
#' }
dialogSpecificMut <- function(MutData, c) {
    
    Word <- dialogMut(myGlobalEnv$StudyRefCase[c], "AA change", "G438C|G...C|G.*.")
    if (Word == "ID_CANCEL") return()
    
    
    MutIndex <- grep(Word, MutData$amino_acid_change)
    if(length(MutIndex)==0){
        msgNoMut=paste ("No Mutation found with genes list for",myGlobalEnv$StudyChoice[c])
        tkmessageBox(message=msgNoMut, icon="info")
        
    } else { 
        
        
        MutList<-cbind(MutData$gene_symbol[MutIndex],MutData$amino_acid_change[MutIndex])
        colnames(MutList) <- c("Gene", "Mutation")
        
        title=paste(myGlobalEnv$StudyRefCase[c],myGlobalEnv$CasesStudies[myGlobalEnv$curselectCases[c]+1], myGlobalEnv$GenProfsStudies[myGlobalEnv$curselectGenProfs[c]+1], sep=": ")
        getInTable(MutList, title=title)
        
        
    }
    
}