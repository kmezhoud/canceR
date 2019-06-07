#' get Methylation data for multiple genes
#' @usage
#' getMetDataMultipleGenes()
#'
#' @return a a dataframe with mean and median of methylation rate (threshold of silencing gene)
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' getMetDataMultipleGenes()
#' }
getMetDataMultipleGenes <-function(){
    
    tclRequire("BWidget")
    tclRequire("Tktable")
    
    testCheckedCaseGenProf()
    
    
    Lchecked_Studies <- myGlobalEnv$lchecked_Studies_forCases
    Lchecked_Cases <- length(myGlobalEnv$curselectCases)
    Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
    ######### Test if all cases were corresponded to appropriate Gen profs (same Study)
    for (i in 1:Lchecked_Cases){
        if(myGlobalEnv$StudyRefCase[i]!=myGlobalEnv$StudyRefGenProf[i]){
            
            msgBadChoice="Correpond the Genetic Profile to the Case for the same Study"
            tkmessageBox(message=msgBadChoice, icon="warning")
            tkfocus(myGlobalEnv$ttCasesGenProfs)
            stop("Correpond the Genetic Profile to the Case for the same Study")
            
        }
    }
    
    ProfDataAll=0
    ProfData=0
    LengthGenProfs=0
    LengthCases=0
    for (i in 1:Lchecked_Studies){
        Si =myGlobalEnv$checked_StudyIndex[i]
        progressBar_ProfilesData <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                                  max = Lchecked_GenProf, width = 400)
        
        #tkfocus(progressBar_ProfilesData)
        LastLengthGenProfs = LengthGenProfs
        LengthGenProfs = LengthGenProfs + myGlobalEnv$LGenProfs[i]+1
        LastLengthCases = LengthCases
        LengthCases= LengthCases + myGlobalEnv$LCases[i]+1
        
        for (k in 1:Lchecked_GenProf){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_ProfilesData, k, label=paste( round(k/Lchecked_GenProf*100, 0),
                                                                       "% of Methylation Data"))
            
            if (myGlobalEnv$curselectGenProfs[k] <= LengthGenProfs && myGlobalEnv$curselectGenProfs[k]>LastLengthGenProfs){    
                
                GenProf<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[k]]
                
                if (length(grep("methylation", GenProf))==0){
                    msgNoMeth <- "Select Methylation data from Genetics Profiles"
                    tkmessageBox(message = msgNoMeth, icon='info')
                    break
                }
                
                Case<-myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[k]]
                
                if (length(grep("methylation", Case))==0){
                    msgNoMeth <- "Select Methylation data from Cases"
                    tkmessageBox(message = msgNoMeth, icon='info')
                    break
                }
                
                
                ProfData<-getProfileData(myGlobalEnv$cgds,myGlobalEnv$GeneList, GenProf,Case)
                
                ##convert data frame to numeric structure
                #if( !is.numeric(ProfData[1,1])){
                #    for(i in 1:ncol(ProfData)){
                    
                 #       ProfData[,i] <- as.numeric(ProfData[,i])
                  #  }
                    
                #}
                ##More facter
                cidx <- !(sapply(ProfData, is.numeric) )
                ProfData[cidx] <- lapply(ProfData[cidx], as.numeric)
                
                ProfData <- round(ProfData, digits=3)
                
                dialogMetOption(ProfData,k)
                
            }
        } 
        close(progressBar_ProfilesData)
    }
    
}