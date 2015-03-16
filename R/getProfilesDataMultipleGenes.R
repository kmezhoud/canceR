getProfilesDataMultipleGenes <-function(getSummaryGSEAExists){
    
    tclRequire("BWidget")
    tclRequire("Tktable")
    
    
    testCheckedCaseGenProf()
    
    ####Create a new directory "Results" for All Profiles Data
    ##Set Results as tempary work directory
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(!file.exists("Results/")){
        dir.create(file.path(paste(getwd(), "/Results", sep="")), showWarnings = FALSE)
        dir.create(file.path(paste(getwd(), "/Results/ProfileData", sep="")), showWarnings = FALSE)
    } else if (!file.exists("Results/ProfileData/")){
        dir.create(file.path(paste(getwd(), "/Results/ProfileData", sep="")), showWarnings = FALSE)
    }
    
    ############################
    if(exists("ttProfData")){
        tkdestroy(ttProfData)
    }
    
    ttProfData <- tktoplevel()
    tkwm.title(ttProfData,"The list of Profiles Data")
    tkwm.geometry(ttProfData, "580x470")
    
    xScr       <- tkscrollbar(ttProfData,command=function(...)tkxview(treeWidget,...),orient="horizontal")
    yScr       <- tkscrollbar(ttProfData,command=function(...)tkyview(treeWidget,...))
    police <- tkfont.create(family="arial", size=11)
    treeWidget <- tkwidget(ttProfData,"Tree",xscrollcommand=function(...)tkset(xScr,...),
                           yscrollcommand=function(...)tkset(yScr,...),width=68,height=25, bg="white")
    
    
    tkgrid(treeWidget,yScr)
    tkgrid.configure(yScr,stick="nsw")
    tkgrid(xScr)
    tkgrid.configure(xScr,stick="new")
    
    
    ##############################
    Lchecked_Studies <- myGlobalEnv$lchecked_Studies_forCases
    Lchecked_Cases <- length(myGlobalEnv$curselectCases)
    Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
    
    
    ProfDataAll=0
    ProfData=0
    LengthGenProfs=0
    LengthCases=0
    for (i in 1:Lchecked_Studies){
        Si = myGlobalEnv$checked_StudyIndex[i]
        progressBar_ProfilesData <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                                  max = Lchecked_GenProf, width = 400)
        
        #tkfocus(progressBar_ProfilesData)
        LastLengthGenProfs <- LengthGenProfs
        LengthGenProfs <- LengthGenProfs + myGlobalEnv$LGenProfs[i]+1
        LastLengthCases <- LengthCases
        LengthCases= LengthCases + myGlobalEnv$LCases[i]+1
        
        #print("Studies"); print(i)
        
        ####Tree ROOT==Studies
        StudyiChoice <- paste("Study",i,"Choice", sep="")
        
        tkinsert(treeWidget, "end", "root", StudyiChoice,text= myGlobalEnv$StudyChoice[i])
        
        
        for (k in 1:Lchecked_GenProf){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_ProfilesData, k, label=paste( round(k/Lchecked_GenProf*100, 0),
                                                                       "% of Profiles Data"))
            
            if (myGlobalEnv$curselectGenProfs[k] <= LengthGenProfs && myGlobalEnv$curselectGenProfs[k]>LastLengthGenProfs){    
                
                GenProf<- myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[k]]
                #print("GenProf"); print(k)
                Case<- myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[k]]
                #print("Case");print(k)
                
                if(length(myGlobalEnv$GeneList)>500){
                    ProfData <- getMegaProfData(myGlobalEnv$GeneList,k )
                } else{
                    ProfData<-getProfileData(myGlobalEnv$cgds,myGlobalEnv$GeneList, GenProf,Case)
                    print(ncol(ProfData))
                }
                ##Convert data frame to numeric structure
#                 print("converting data frame of Profile data to numeric stucture...")
#                 cidx <- !(sapply(ProfData, is.numeric))
#                 ProfData[cidx] <- lapply(ProfData[cidx], as.numeric)
                #for(p in 1:ncol(ProfData)){
                    
                 #   ProfData[,p] <- as.numeric(ProfData[,p])
                #}
                
                
                
                print(paste("Genes number:",ncol(ProfData), sep=" "))
                ##cleaning data.framme from NaN and NA
                ## substitute "NaN" by "NA"
                
                if(length(grep("NaN",ProfData))!=0 && getSummaryGSEAExists==1){
                    
                    print("This step takes a while to remove NaN string and convert them to NA (12000 genes takes about 10 min)")
                    for (j in 1:ncol(ProfData)){
                        #ProfData[,i]<- as.numeric(gsub("\\[Not Available\\]","na", ProfData[,i]))
                        #ProfData <- sapply(ProfData, function(x) gsub("NA", "na", x))
                        ProfData[,j]<- as.numeric(gsub("NaN",NA, ProfData[,j]))
                        #ProfData[,i]<- as.numeric(gsub(NA,"na", ProfData[,i]))
                        
                    }
                }
                print("Removing genes without data... ")
                ##remove all NAs rows
                print(paste("Genes number:",ncol(ProfData), sep=" "))
            
                if(ncol(ProfData)==0){
                    tkmessageBox(message=paste("There is no",myGlobalEnv$GenProfChoice[k] ," for used gene list",icon="info"))
                    break(paste("There is no", myGlobalEnv$GenProfChoice[k]," for used gene list"))
                    
                }else{
                    ProfData<- ProfData[which( apply( !( apply(ProfData,1,is.na) ),2,sum)!=0 ),]
                    print(paste("dimension of Profile Data:"))
                    print(dim(ProfData))
                }
                
                
                
                
                ## When matrix has negative values,  simple space are generated before every positive value. 
                ## This space cause deleting all columns from matrix when we try to remove the columns without values NAs
                if(min(ProfData, na.rm = TRUE) < 0){
                    for (j in 1:ncol(ProfData)){
                        
                        ProfData[,j]<- as.numeric(gsub(" ","", ProfData[,j]))
                        
                    }
                }
                
                
                print(paste("dimension of Profile Data:"))
                print(dim(ProfData))
                print("end cleaning...")
                
                if(ncol(ProfData)==0){
                    tkmessageBox(message=paste("There is no",myGlobalEnv$GenProfChoice[k]," for used gene list",icon="info"))
                    #tkdestroy(ttProfData)
                    break 
                    
                }
                print(paste("dimension of Profile Data:"))
                print(dim(ProfData))
                
                ProfData <- round(ProfData, digits=3)
                print("rounding Profile Data ...")
                fileName<-paste("SD_",myGlobalEnv$checked_StudyIndex[i],"_","GenProf_",myGlobalEnv$curselectGenProfs_forStudy[k],"_","CASE_",myGlobalEnv$curselectCases_forStudy[k],".txt")
                
                workspace <- getwd()
                Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                setwd(paste(getwd(),"/Results/ProfileData", sep=""))
                
                write.table(ProfData,file=fileName, col.names=TRUE, quote=FALSE, row.names=TRUE, sep="\t")
                
                setwd(workspace)
                
                
                
                ###Tree suit
                GenkProfChoice <- paste("Gen",k,"Prof", sep="")
                CasekChoice <- paste("Case", k, "Choice", sep="")
                ProfkData <- paste("Prof", k, "Data", sep="")
                
                tkinsert(treeWidget, "end", StudyiChoice, GenkProfChoice, text = paste("Genetic Profile: ",myGlobalEnv$GenProfChoice[k], sep=" "))
                tkinsert(treeWidget, "end", GenkProfChoice,CasekChoice, text = paste("Case: ",myGlobalEnv$CaseChoice[k], sep=" "))
                tkinsert(treeWidget, "end",CasekChoice, ProfkData,text= paste("Profile Data file: ", fileName, sep=" "))
                
                
                print("END!")
                
                
                
            }
        } 
        close(progressBar_ProfilesData) 
    }
    
    OnOK <- function()
    { 
        ###view WIDGETOOL
        file <- tclvalue(tkgetOpenFile()) 
        if (!nchar(file)) {
            tkmessageBox(message = "No file was selected!")
        } else {
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            #w<-2
            if(exists("ProfDataTable")){
                LastProfDataTable<-  ProfDataTable 
            }
            ProfDataTable<-read.table(file, header=TRUE, row.names=1)
            assign("ProfDataTable", ProfDataTable, envir = myGlobalEnv)
            
            
            getInTable(ProfDataTable, file)
            
        }
        
    }
    OnCancel <- function(){
        tkdestroy(ttProfData)
    }
    
    OK.but <-tkbutton(ttProfData,text="   OK   ", command= OnOK)
    tkgrid(OK.but,  row=2,column=0, sticky="e")
    
    Cancel.but <- tkbutton(ttProfData, text="Cancel", command=OnCancel )
    # Place the two buttons on the same row in their assigned window (ttProfData)
    tkgrid(Cancel.but,row=2, column=0, sticky="w")
    tkfocus(ttProfData)
    
}