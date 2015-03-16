getGenProfs <- function(){
    
    #get Study Index 
    StudiesRef <- getCancerStudies(myGlobalEnv$cgds)[,1]
    
    
    ## and we need the cases list of every study
    myGlobalEnv$checked_Studies_forGenProf <- myGlobalEnv$checked_Studies
    myGlobalEnv$lchecked_Studies_forGenProf <- length(myGlobalEnv$checked_Studies_forGenProf)
    
    
    police <- tkfont.create(family="arial", size=11)
    tkconfigure(myGlobalEnv$tl, foreground="black", font=police)
    
    
    GenProfsStudies = 0
    GenProfsRefStudies =0
    LGenProfs=0
    for (i in 1:(myGlobalEnv$lchecked_Studies_forGenProf)){
        
        Si = myGlobalEnv$checked_StudyIndex[i]
        
        tkinsert(myGlobalEnv$tl,"end",paste("***** Study ", Si ," : ", myGlobalEnv$Studies[Si],"******"))
        
        LGenProfs[i]<- length(getGeneticProfiles.CGDS(myGlobalEnv$cgds, StudiesRef[Si])[,1])
        
        myGlobalEnv$LGenProfs[i] <- LGenProfs[i]
        print(paste("There are",myGlobalEnv$LGenProfs[i],"Genetics Profiles in", myGlobalEnv$Studies[Si],sep=" "))
        
        # create progress bar
        progressBar_GenProfs <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                              max = LGenProfs[i], width = 400)
        
        
        GenProfRefStudy =0    
        GenProfsStudy = 0
        GenProfsRefStudy =0  
        j=0
        for (j in 1:LGenProfs[i]){
            
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_GenProfs, j, 
                             label=paste( round(j/LGenProfs[i]*100, 0),"% of Genetic Profiles"))
            
            
            
            GenProfStudy <- getGeneticProfiles.CGDS(myGlobalEnv$cgds, myGlobalEnv$checked_Studies_forGenProf[i])[,2][j]
            GenProfRefStudy <- getGeneticProfiles.CGDS(myGlobalEnv$cgds, myGlobalEnv$checked_Studies_forGenProf[i])[,1][j]
            
            tkinsert(myGlobalEnv$tl,"end",paste(j,":",GenProfStudy))
            GenProfsStudy <- cbind(GenProfsStudy, GenProfStudy)
            GenProfsRefStudy <- cbind(GenProfsRefStudy, GenProfRefStudy)
        }
        close(progressBar_GenProfs)
        
        GenProfsStudies <- cbind(GenProfsStudies, GenProfsStudy)
        GenProfsRefStudies <- cbind(GenProfsRefStudies, GenProfsRefStudy)
    }
    
    
    GenProfsRefStudies <- GenProfsRefStudies[-1]
    myGlobalEnv$GenProfsRefStudies <- GenProfsRefStudies
    myGlobalEnv$GenProfsStudies <-GenProfsStudies
    
    
}