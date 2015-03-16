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