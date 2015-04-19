getMegaProfData <- function(MegaGeneList,k)
{
    
    if(is.integer(length(MegaGeneList)/500)){
        
        G <- lenght(MegaGeneList)/500
    }else{
        G <- as.integer(length(MegaGeneList)/500) + 1
        
    }
    
    GenProf<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[k]]
    
    Case<-myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[k]]
    
    
    MegaProfData <- 0
    SubMegaGeneList <- 0
    for(g in 1: G){
        
        if (length(MegaGeneList) - length(SubMegaGeneList) > 500){
            SubMegaGeneList <- MegaGeneList[(((g-1)*500)+1):((g)*500)]
        } else{
            SubMegaGeneList <- MegaGeneList[((g*500)+1):(length(MegaGeneList) - length(SubMegaGeneList))]
        }
        
        
        print(paste("Getting Profile Data of Genes from: ", (((g-1)*500)+1), "to",((g)*500), sep= " "))
        
        ProfData<-getProfileData(myGlobalEnv$cgds,SubMegaGeneList, GenProf,Case)
        MegaProfData <- cbind(MegaProfData, ProfData)
    }
    MegaProfData <- MegaProfData[,-1]
    
    
    return(MegaProfData)
    
    
}