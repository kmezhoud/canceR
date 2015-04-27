getGenesTree_MultipleCases <- function(entryWidth = 10){
    
             testCheckedCaseGenProf()
            
            ##Checking Genes list, Cases GeneProf
            if(length(myGlobalEnv$curselectCases)==1||length(myGlobalEnv$curselectGenProfs)==1){
                msgNoOneStudy = "Select more than one Case/Genetic Profile or use single Case function"
                tkmessageBox(message=msgNoOneStudy, icon="warning")
                stop(msgNoOneStudy)
                
            }
            
            Lchecked_Studies <- myGlobalEnv$lchecked_Studies_forCases
            Lchecked_Cases <- length(myGlobalEnv$curselectCases)
            Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
            ###Starting function of dialoGenesTree
            d <- 0
            ProfDataAll<-0
            ProfData<-0
            LengthGenProfs<-0
            LengthCases<-0
            for (i in 1:length(myGlobalEnv$CaseChoice)){
                Si <- myGlobalEnv$checked_StudyIndex[i]
                progressBar_ProfilesData <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                                          max = Lchecked_GenProf, width = 400)
                
                if(exists('ttGeneTree', envir = myGlobalEnv)){
                    tkdestroy(myGlobalEnv$ttGeneTree)
                    
                }
                
                
                LastLengthGenProfs <- LengthGenProfs
                LengthGenProfs <- LengthGenProfs + myGlobalEnv$LGenProfs[i]+1
                LastLengthCases <- LengthCases
                LengthCases <- LengthCases + myGlobalEnv$LCases[i]+1
                
                Sys.sleep(0.1)
                setTkProgressBar(progressBar_ProfilesData, i, label=paste( round(i/Lchecked_GenProf*100, 0),
                                                                           "% of Expression Set"))
                ##########
                ttGeneTree <- tktoplevel()
                #tkwm.geometry(ttGeneTree,"180x250")
                tktitle(ttGeneTree) <- paste(myGlobalEnv$Studies[Si],": Classify genes by variable")
                
                ##Image Horizontal scale option
                textEntryHscale <- tclVar("2")
                textEntryWidget <- tkentry(ttGeneTree, width = paste(entryWidth),
                                           textvariable = textEntryHscale)
                
                txtHscale <- tklabel(ttGeneTree, text = "Horizontal Scale of the plot")
                tkgrid(txtHscale)
                tkgrid.configure(txtHscale, column=1, row=1, sticky="w")
                tkgrid(textEntryWidget)
                tkgrid.configure(textEntryWidget, column=1, row=1, sticky="ne")
                
                ##Image Vertical scale option
                textEntryVscale <- tclVar("1")
                textEntryWidgetV <- tkentry(ttGeneTree, width = paste(entryWidth),
                                            textvariable = textEntryVscale)
                
                txtVscale <- tklabel(ttGeneTree, text = "Vertical Scale of the plot")
                tkgrid(txtVscale)
                tkgrid.configure(txtVscale, column=1, row=2, sticky="w")
                tkgrid(textEntryWidgetV)
                tkgrid.configure(textEntryWidgetV, column=1, row=2, sticky="ne")
                
                
                
                ##Clinical Data list
                
                label1 <- tklabel(ttGeneTree, text= "Clinical Data")
                
                yscr1 <- tkscrollbar(ttGeneTree, repeatinterval=2,
                                     command=function(...)tkyview(tl1,...))
                xscr1 <- tkscrollbar(ttGeneTree, repeatinterval=2,orient="horizontal",
                                     command=function(...)tkxview(tl1,...))
                
                xscr1Info <- tkscrollbar(ttGeneTree, repeatinterval=2,orient="horizontal",
                                         command=function(...)tkxview(tl1info,...))
                
                
                tl1<-tklistbox(ttGeneTree,height=10, width= 40 ,selectmode="single",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")
                tl1info<-tklistbox(ttGeneTree,height=1, width= 40,selectmode="single",xscrollcommand=function(...)tkset(xscr1Info,...),background="white")
                
                
                #getClinicData_SingleCase()
                Case<-myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[i]]
                ClinicalData<-getClinicalData(myGlobalEnv$cgds,Case)
                
                
                loadVariable <- function()
                {  
                    curselectVariable <- as.numeric(tkcurselection(tl1))+1
                    lcurselectVariable <- length(curselectVariable)
                    
                    myGlobalEnv$variable <-  names(ClinicalData)[curselectVariable]
                    tkdelete(tl1info,0,1)
                    tkinsert(tl1info,"end",myGlobalEnv$variable)
                    
                }
                
                
                Variable.but <-tkbutton(ttGeneTree,text="select",command=loadVariable)
                
                tkgrid(label1,tl1,yscr1)
                tkgrid.configure(yscr1,rowspan=20, columnspan=2,sticky="nsw")
                tkgrid(xscr1)
                tkgrid.configure(xscr1,rowspan=2, column=1,sticky="we")
                tkgrid(Variable.but, tl1info, columnspan=1)
                tkgrid(xscr1Info)
                tkgrid.configure(xscr1Info,rowspan=4, column=1,sticky="we")
                
                print(paste("testing which Genenic Profile: ", myGlobalEnv$curselectGenProfs[i],"<=", LengthGenProfs))
                print(paste("testing last Genenic Profile: ",myGlobalEnv$curselectGenProfs[i],">",LastLengthGenProfs))
                if (myGlobalEnv$curselectGenProfs[i] <= LengthGenProfs && myGlobalEnv$curselectGenProfs[i]>LastLengthGenProfs){    
                    
                    GenProf<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[i]]
                    
                    ProfData<-getProfileData(myGlobalEnv$cgds,myGlobalEnv$GeneList, GenProf,Case)
                    
                    ##Convert data frame to numeric structure
#                     print("converting data frame of Profile data to numeric stucture...")
#                     
#                     for(i in 1:ncol(ProfData)){
#                         
#                         ProfData[,i] <- as.numeric(ProfData[,i])
#                     }
                    
                    ## for loop is faster than apply fonction
                    #rnames <- rownames(ProfData)
                    #ProfData <- as.data.frame(apply(ProfData,2 ,function(x) as.numeric(x)))
                    #rownames(ProfData) <- rnames
                    
                   #test if is there a clinical data
                    if(length(ClinicalData[1,])==0){
                        tkdestroy(ttGeneTree)
                        msgNoClinData=paste("No Clinical Data are Available for\n", CaseChoice)
                        tkmessageBox(message=msgNoClinData, title= CaseChoice, icon="info")
                        break
                    }
                    print('Case has Clinical Data...')
                    ## Select only Cases (rownames) that exist in ClinicalDataSub and ProfData
                    merge <- merge(ClinicalData, ProfData, by="row.names")
                    print("merging Samples from Profile and Clinical Data")
                    ClinicalData<- merge[,2:(length(ClinicalData)+1)]
                    
                    
                    ProfData<-merge[,!(merge %in% ClinicalData)]
                    
                    
                    for (j in 1:length(names(ClinicalData))){
                        
                        tkinsert(tl1,"end",names(ClinicalData)[j])
                    }
                    
                    Methods <- c("class","anova","poisson")
                    # Default selections for the two combo boxes
                    defaultMethod <- tclVar("class")
                    favMethod <- tclVar("class")
                    
                    comboBox <- ttkcombobox(ttGeneTree, values=Methods, textvariable=favMethod, state="readonly")                        
                    
                    text <- tklabel(ttGeneTree,text="Select Method:")
                    tkgrid(text, comboBox)  
                    
                    
                    d<-d+1
                    onOK <- function(){
                        if(exists("variable", envir = myGlobalEnv)){
                            print(paste("d",d))
                            HorScale <- as.numeric(tclvalue(textEntryHscale))
                            VerScale <- as.numeric(tclvalue(textEntryVscale))
                            myGlobalEnv$ProfData <- cbind(ClinicalData[,myGlobalEnv$variable], ProfData[,-1])
                            
                            colnames(myGlobalEnv$ProfData)[1] <- myGlobalEnv$variable
                            frmla <- paste0(myGlobalEnv$variable, "~.", sep="")
                            myGlobalEnv$frmla <- as.formula(frmla)
                            print(myGlobalEnv$frmla)
                            
                            
                            ##selected mathod
                            selectedMethod <- tclvalue(favMethod)
                            print(paste("Selected Method:", selectedMethod))
                            
                            plotCommand<- function(){
                                
                                
                                fit <- rpart::rpart(myGlobalEnv$frmla, method=selectedMethod, data=myGlobalEnv$ProfData)
                                plot(fit, uniform=TRUE, compress=TRUE,main= paste(myGlobalEnv$StudyChoice[d],"\n ",myGlobalEnv$GenProfChoice[d],"vs",myGlobalEnv$variable ))
                                text(fit, use.n=TRUE, all=TRUE, cex=0.6, fancy=FALSE)
                                ##capture print(fit) for editing
                                summary <- capture.output(print(fit))
                                ## Edit summary fit
                                getTextWin(paste(summary,collapse="\n"))
                            }
                            plotModel(plotCommand, title=paste(myGlobalEnv$checked_Studies[d],":",myGlobalEnv$CaseChoice[d],"vs" ,myGlobalEnv$variable, sep=""), vscale=VerScale, hscale=HorScale)
                            tkdestroy(ttGeneTree)
                        }else{
                            msgNoFrmla <- "Select one variable"
                            tkmessageBox(message= msgNoFrmla, icon="info")
                        }
                        
                    }
                    
                    
                    Ok.but <-tkbutton(ttGeneTree,text=" OK ",command=onOK)
                    tkgrid(Ok.but)
                    tkgrid.configure(Ok.but,rowspan=4, column=1,sticky="n")
                    
                    tkwait.window(ttGeneTree)
                    
                    
                } else {
                    tkdestroy(ttGeneTree)
                    close(progressBar_ProfilesData)
                    msgBadCheck <- paste("Choose only one Case/Genetic Profile by Study.")
                    tkmessageBox(message=msgBadCheck, icon="warning")
                }
                
                
                
                close(progressBar_ProfilesData)
            } 
}