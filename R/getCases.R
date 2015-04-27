getCases <- function(){
    
    
    
    #get Study Index 
    StudiesRef <- getCancerStudies(myGlobalEnv$cgds)[,1]
    #checked_StudyIndex
    checked_StudyIndex_forCases <- myGlobalEnv$checked_StudyIndex
    myGlobalEnv$checked_StudyIndex_forCases <- checked_StudyIndex_forCases
    
    ## and we need the cases list of every study
    checked_Studies_forCases <- myGlobalEnv$checked_Studies
    myGlobalEnv$checked_Studies_forCases <- checked_Studies_forCases
    lchecked_Studies_forCases <- length(checked_Studies_forCases)
    myGlobalEnv$lchecked_Studies_forCases <- lchecked_Studies_forCases
    
    
    police <- tkfont.create(family="arial", size=11)
    tkconfigure(myGlobalEnv$tc, foreground="black", font=police)
    
    
    
    CasesStudies = 0
    CasesRefStudies=0
    LCases=0
    CasesRefStudy=0
    
    
    
    for (i in 1:myGlobalEnv$lchecked_Studies_forCases){
        
        
        
        Si <- myGlobalEnv$checked_StudyIndex[i]
        
        
        tkinsert(myGlobalEnv$tc,"end",paste("***** Study ", Si ," : ", myGlobalEnv$Studies[Si], "*****"))
        
        LCases[i] <- length(getCaseLists(myGlobalEnv$cgds, StudiesRef[Si])[,1])
        
        myGlobalEnv$LCases[i] <- LCases[i]
        print(paste("There are", myGlobalEnv$LCases[i], "Cases in",myGlobalEnv$Studies[Si] ,sep=" "))
        
        # create progress bar
        progressBar_Cases <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                           max = LCases[i], width = 400)
        
        j=0
        CasesStudy = 0
        CaseRefStudy =0
        CasesRefStudy = 0
        for (j in 1:LCases[i]){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_Cases, j, label=paste( round(j/LCases[i]*100, 0),
                                                                "% of Cases"))
            
            CaseStudy <- getCaseLists(myGlobalEnv$cgds,checked_Studies_forCases[i])[,3][j]
            CaseRefStudy <- getCaseLists(myGlobalEnv$cgds, checked_Studies_forCases[i])[,1][j]
            
            tkinsert(myGlobalEnv$tc,"end",paste(j,":",CaseStudy))
            
            CasesStudy <- cbind(CasesStudy, CaseStudy)
            CasesRefStudy <- cbind(CasesRefStudy,CaseRefStudy)
            
        }
        ##Close ProgressBar_Cases
        close(progressBar_Cases)
        
        
        CasesStudies <- cbind(CasesStudies, CasesStudy)
        CasesRefStudies <- cbind(CasesRefStudies, CasesRefStudy)
        
    }
    
    CasesRefStudies <- CasesRefStudies[-1]
    myGlobalEnv$CasesRefStudies <- CasesRefStudies
    myGlobalEnv$CasesStudies <- CasesStudies
    
}