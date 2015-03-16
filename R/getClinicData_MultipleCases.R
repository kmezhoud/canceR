getClinicData_MultipleCases<- function(getSummaryGSEAExists){
    
    ##getSummaryGSEAExists is an argument for the function getSummaryGSEA(). this function accept only one clinical dats.
    if(getSummaryGSEAExists ==1 && length(myGlobalEnv$curselectCases)>1){
        
        tkmessageBox(message=paste("Multiple Cases are loaded. To get Summary of GSEA Results, Select only the Clinical data of study on which are you working."), icon="warning")
        stop("Multiple Cases are loaded. To get Summary of GSEA Results, Select only the Clinical data of study on which are you working.")
    }
    
    
    ClinicalData<-0
    ClinicalData_All<-NULL
    ClinicalDataSub <-NULL
    ClinicalDataSub_All <- NULL
    
    if(length(grep("curselectCases",ls(myGlobalEnv)))==0){
        msgNoCases <- "Select at less one Case"
        tkmessageBox(message=msgNoCases)
        stop(msgNoCases)
    }
    
    for(c in 1:length(myGlobalEnv$curselectCases)){
        Case<- myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[c]]
        
        ClinicalData<-getClinicalData(myGlobalEnv$cgds,Case)
        
        
        if(length(ClinicalData[1,])==0){
            msgNoClinData=paste("No Clinical Data are Available for\n", myGlobalEnv$CasesStudies[myGlobalEnv$curselectCases[c]+1])
            tkmessageBox(message=msgNoClinData, title= paste("Study: ",myGlobalEnv$StudyRefCase[c]))
        } else{
            
            ttClin<-tktoplevel()
            tktitle(ttClin) <- paste("Clinical Data of", myGlobalEnv$CaseChoice[c], sep=" ")
            tkwm.geometry(ttClin, "430x420")
            yscr1 <- tkscrollbar(ttClin, repeatinterval=2,
                                 command=function(...)tkyview(ttc,...))
            xscr1 <- tkscrollbar(ttClin, repeatinterval=2,orient="horizontal",
                                 command=function(...)tkxview(ttc,...))
            ttc<-tklistbox(ttClin,height=20, width= 50 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")
            
            tkgrid(ttc,yscr1, columnspan=1)
            tkgrid.configure(yscr1,rowspan=20, columnspan=1,sticky="nsw")
            tkgrid(xscr1,columnspan=2)
            tkgrid.configure(xscr1,rowspan=2,columnspan=2,sticky="ew")
            
            tkinsert(ttc,"end","All")
            cbIValue=0
            for(j in 1: length(names(ClinicalData))){
                tkinsert(ttc,"end",names(ClinicalData)[j])
                
            }
            
            OnOK <- function(){
                myGlobalEnv$curselect <- as.numeric(tkcurselection(ttc))+1
                
                if(myGlobalEnv$curselect[1]=="1"){
                    myGlobalEnv$ClinicalData <- ClinicalData
                    
                    title=paste(myGlobalEnv$StudyRefCase[c],myGlobalEnv$CasesStudies[myGlobalEnv$curselectCases[c]+1], sep=": ")
                    getInTable(myGlobalEnv$ClinicalData, title)
                    tkdestroy(ttClin)
                } else{
                    #myGlobalEnv$ClinicalData <- ClinicalData[!(is.na(ClinicalData[,myGlobalEnv$curselect]) | ClinicalData[,myGlobalEnv$curselect]==""), ] 
                    myGlobalEnv$ClinicalData <- ClinicalData[myGlobalEnv$curselect-1]
                    
                    
                    title=paste(myGlobalEnv$StudyRefCase[c],myGlobalEnv$CasesStudies[myGlobalEnv$curselectCases[c]+1], sep=": ")
                    getInTable(myGlobalEnv$ClinicalData, title)
                    
                    tkdestroy(ttClin)
                    
                }
                
                
            }
            OK.but <- tkbutton(ttClin,text="OK",command=OnOK)
            tkgrid(OK.but,columnspan=2)
            
            ##Waiting to checkbox before to access to the next clinical data
            tkwait.window(ttClin)
        }
        
        
        
        ##getSummaryGSEA funtion needs return(ClinicalSub_All)
        if(getSummaryGSEAExists==1){
            return(myGlobalEnv$ClinicalData)
        }
    }
    
    
}