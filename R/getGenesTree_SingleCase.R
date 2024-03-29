#' classify genes in tree for two phenotypes in the same case(disease).
#' @usage getGenesTree_SingleCase()
#' @return tree plot
#' @export
#'
#'@examples
#'readRDS(paste(path.package("canceR"),"/extdata/rdata/prad_michPhenoTest1021.rds", sep=""))
#' \dontrun{
#' getGenesTree_SingleCase()
#' }
getGenesTree_SingleCase <- function(){
   
    if(length(ENV$curselectCases)!=1||length(ENV$curselectGenProfs)!=1){
        msgOneStudy = "Select only one Case/Genetic Profile or use multiple Cases"
        tkmessageBox(message=msgOneStudy, icon="warning")
        
    } else {
        LengthCases <- 0
        d <- 0
        for (i in 1:length(ENV$checked_Studies)){
            
            LengthCases <- LengthCases + ENV$LCases[i]+1
            d <- d +1 
            if(ENV$curselectCases < LengthCases){
                
                entryWidth <- 10
                ttGeneTree <- tktoplevel()
                #tkwm.geometry(ttGeneTree,"180x250")
                
              
                tktitle(ttGeneTree) <- paste(ENV$StudyChoice[i],": Classifying genes by variable")
                
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
                
                
                Case<-ENV$CasesRefStudies[ENV$curselectCases]
                #getClinicData_SingleCase()
                ClinicalData<-getClinicalData(ENV$cgds,Case)
                
                
                loadVariable <- function()
                {  
                    curselectVariable <- as.numeric(tkcurselection(tl1))+1
                    lcurselectVariable <- length(curselectVariable)
                    
                    ENV$variable <-  names(ClinicalData)[curselectVariable]
                    tkdelete(tl1info,0,1)
                    tkinsert(tl1info,"end",ENV$variable)
                    
                }
                
                
                Variable.but <-tkbutton(ttGeneTree,text="select",command=loadVariable)
                
                tkgrid(label1,tl1,yscr1)
                tkgrid.configure(yscr1,rowspan=20, columnspan=2,sticky="nsw")
                tkgrid(xscr1)
                tkgrid.configure(xscr1,rowspan=2, column=1,sticky="we")
                tkgrid(Variable.but, tl1info, columnspan=1)
                tkgrid(xscr1Info)
                tkgrid.configure(xscr1Info,rowspan=4, column=1,sticky="we")
                
                
                GenProf<-ENV$GenProfsRefStudies[ENV$curselectGenProfs]
                
                
                ProfData<-getProfileData(ENV$cgds,ENV$GeneList, GenProf,Case)
                
                
                ##Convert data frame to numeric structure
#                 print("converting data frame of Profile data to numeric stucture...")
#                 
#                 for(i in 1:ncol(ProfData)){
#                     
#                     ProfData[,i] <- as.numeric(ProfData[,i])
#                 }
                
                ##for loop is faster than apply fonction
                #rnames <- rownames(ProfData)
                #ProfData <- as.data.frame(apply(ProfData,2 ,function(x) as.numeric(x)))
                #rownames(ProfData) <- rnames
                
                #test if is there a clinical data
                if(length(ClinicalData[1,])==0){
                    msgNoClinData=paste("No Clinical Data are Available for\n", ENV$CaseChoice)
                    tkmessageBox(message=msgNoClinData, title= ENV$CaseChoice, icon="info")
                    stop()
                }
                print('Clinical Data exists...')
                ## Select only Cases (rownames) that exist in ClinicalDataSub and ProfData
                merge <- merge(ClinicalData, ProfData, by="row.names")
                print("merging samples from Clinical and Profile Data...")
                ClinicalData<- merge[,2:(length(ClinicalData)+1)]
                
                ProfData<-merge[,!(merge %in% ClinicalData)]
                
                
                for (i in 1:length(names(ClinicalData))){
                    
                    tkinsert(tl1,"end",names(ClinicalData[i]))
                }
                
                Methods <- c("class","anova","poisson")
                # Default selections for the two combo boxes
                defaultMethod <- tclVar("class")
                favMethod <- tclVar("class")
                
                comboBox <- ttkcombobox(ttGeneTree, values=Methods, textvariable=favMethod, state="readonly")                        
                
                text <- tklabel(ttGeneTree,text="Select Method:")
                tkgrid(text, comboBox)              
                
                
                onOK <- function(){
                    if(exists("variable", envir = ENV)&&exists("Methods")){
                        
                        HorScale <- as.numeric(tclvalue(textEntryHscale))
                        VerScale <- as.numeric(tclvalue(textEntryVscale))
                        ENV$ProfData <- cbind(ClinicalData[,ENV$variable], ProfData[,-1])
                        
                        colnames(ENV$ProfData)[1] <- ENV$variable
                        
                        
                        frmla <- paste0(ENV$variable, "~.", sep="")
                        ENV$frmla <- as.formula(frmla)
                        
                        print(paste("Selected formula: ", ENV$frmla))
                        
                        
                        ##selected mathod
                        selectedMethod <- tclvalue(favMethod)
                        print(paste("Selected Method:", selectedMethod))
                        
                        plotCommand<- function(){
                            
                            #img <- tree(frmla, data=ProfData)
                            ENV$fit <- rpart::rpart(ENV$frmla, method=selectedMethod, data=ENV$ProfData)
                            plot(ENV$fit, uniform=TRUE, compress=TRUE,margin=0,main= paste(ENV$StudyChoice[d],"\n ",ENV$GenProfChoice," vs ",ENV$variable ))
                            text(ENV$fit, use.n=TRUE, all=TRUE, cex=0.6, fancy=FALSE)
                            
                        
                            
                        }
                        plotModel(plotCommand, title=paste(ENV$checked_Studies[d],":",ENV$CaseChoice,"vs" ,ENV$variable, sep=""), vscale=VerScale, hscale=HorScale)
                        ##capture print(fit) for editing
                        summary <- capture.output(print(ENV$fit))
                        ## Edit summary fitqqddcdsc
                        getTextWin(paste(summary,collapse="\n"))
                        
                        tkdestroy(ttGeneTree)
                    }else{
                        msgNoFrmla <- "Select one variable"
                        tkmessageBox(message= msgNoFrmla, icon="info")
                    }
                    
                    
                }
                
                Ok.but <-tkbutton(ttGeneTree,text=" OK ",command=onOK)
                tkgrid(Ok.but)
                tkgrid.configure(Ok.but,rowspan=4, column=1,sticky="n")
                ## break loop to avoid getting next studies without case GenProf.
                break()
            }else{
                print(paste("Skip Study:", ENV$checked_Studies[i]))
            }
        }
    }
}