#' get Profiles Data  for a Single Gene.
#' @usage getProfilesDataSingleGene()
#' @return dataframe with profiles data for a single gene
#' @export
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' ## Select Case from Breast Cancer
#'  myGlobalEnv <- new.env(parent = emptyenv())
#' myGlobalEnv$curselectCases <- 9
#' ##Select Genetic Profile from Breast Cancer
#' myGlobalEnv$curselectGenProfs <- 4
#' ## get Specific Mutation data for 73 Genes list
#' \dontrun{
#' getProfilesDataSingleGene()
#' }
getProfilesDataSingleGene <-function(){
    
    tclRequire("BWidget")
    tclRequire("Tktable")
    
    testCheckedCaseGenProf()
    

    Lchecked_Studies <- myGlobalEnv$lchecked_Studies_forCases
    Lchecked_Cases <- length(myGlobalEnv$curselectCases)
    Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
    
    ############################
    ProfDataAll<-0
    ProfData<-0
    LengthGenProfs<-0
    LengthCases<-0
    
    for (s in 1:Lchecked_Studies){
        Si =myGlobalEnv$checked_StudyIndex[s]
        GenProfS=0
        GenProfS<- getGeneticProfiles.CGDS(myGlobalEnv$cgds, myGlobalEnv$checked_Studies_forGenProf[s])[,1]
        
        ## Wich Cases are checked and for any study and Genetic Profiles? 
        LastLengthCases <- LengthCases
        LengthCases <- LengthCases + myGlobalEnv$LCases[s]+1
        for(c in 1: Lchecked_Cases){
            if(myGlobalEnv$curselectCases[c] <= LengthCases && myGlobalEnv$curselectCases[c]>LastLengthCases){
                print(paste("Case",myGlobalEnv$curselectCases[c],"<",LengthCases,myGlobalEnv$curselectCases[c],">",LastLengthCases ))   
                
                CaseS<- myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[c]]
                
                launchDialog <- function(){
                    
                    Dialog_Title<- paste("STUDY:", Si,"CASE:", myGlobalEnv$curselectCases_forStudy[c], sep=" ")
                    GENE <- modalDialog(Dialog_Title, "Enter HUGO Gene Symbol", "MDM4")
                    if (GENE == "ID_CANCEL") return()
                    
                    ProfDataS<-getProfileData(myGlobalEnv$cgds,GENE, GenProfS,CaseS)
                    
                    
                    ttProfData_cb <- tktoplevel()
                    tktitle(ttProfData_cb) <- paste(myGlobalEnv$StudyRefCase[c],myGlobalEnv$CaseChoice[c], sep=": ")
                    #tkwm.geometry(ttProfData_cb,"300x300")
                    
                    cbAll <- tkcheckbutton(ttProfData_cb)
                    cbAllValue <- tclVar("0")
                    tkconfigure(cbAll,variable=cbAllValue)
                    labelAll<- tklabel(ttProfData_cb,text= "All")
                    tkgrid(labelAll, cbAll)
                    
                    cbIValue=0
                    for(i in 1: length(names(ProfDataS))){
                        
                        cbi <- paste ("cb", i, sep="")  
                        cbi <- tkcheckbutton(ttProfData_cb)
                        cbiValue <- paste("cb", i, "Value", sep="")
                        cbIValue[i] <- cbiValue
                        cbIValue[i] <- tclVar("0")
                        
                        tkconfigure(cbi,variable=cbiValue)
                        labeli <- paste ("label", i , sep="") 
                        labelI <- labeli
                        labelI <- tklabel(ttProfData_cb,text= names(ProfDataS[i]))
                        tkgrid(labelI,cbi)
                        
                    }
                    ProfDataSSub<-0
                    OnOK <- function(){
                        
                        cbAllVal <- as.character(tclvalue(cbAllValue))
                        if(cbAllVal =="1"){
                            
                            for (i in 1:length(names(ProfDataS))){
                                ProfDataS[,i]<- gsub("\\[Not Available\\]","NA", ProfDataS[,i])
                            }
                            
                            ProfDataS <- t(t(ProfDataS))
                            title<-paste(myGlobalEnv$StudyRefCase[c],myGlobalEnv$CaseChoice[c], sep=": ")
                            getInTable(ProfDataS, title)
                            
                            
                        } else{
                            
                            for (i in 1: length(names(ProfDataS))){
                                cbiValue <- paste("cb", i, "Value", sep="")
                                cbIValue <- cbiValue
                                cbiVal <- paste("cb", i, "Val", sep="")
                                cbIVal<-cbiVal
                                
                                cbIVal[i] <- as.character(tclvalue(cbIValue))
                                #tkdestroy(ttProfData_cb)
                                if (cbIVal[i]=="1"){
                                    
                                    ## convert metacharacter "[""]" not supported by tclarray()
                                    ProfDataS[,i]= gsub("\\[Not Available\\]","NA", ProfDataS[,i])
                                    
                                    ProfDataSSub<- cbind(ProfDataSSub,ProfDataS[i])
                                    
                                }
                                
                            }
                            ProfDataSSub<-ProfDataSSub[-1]
                            
                            if(length(ProfDataSSub)==0){
                                tkmessageBox(message= paste("Select at least one data type"), icon="warning")
                                stop("Select at least one data type")
                            }
                           
                            ProfDataSSub <- t(t(ProfDataSSub))
                            title=paste(myGlobalEnv$StudyRefCase[c],myGlobalEnv$CaseChoice[c], sep=": ")
                            getInTable(ProfDataSSub, title)
                            
                        }
                        tkdestroy(ttProfData_cb)
                    }
                    
                    OK.but <- tkbutton(ttProfData_cb,text="OK",command=OnOK)
                    tkgrid(OK.but)
                    tkfocus(ttProfData_cb)
                    
                    ##Waiting to checkbox before to access to the next clinical data
                    tkwait.window(ttProfData_cb)
                    
                    
                    
                }
                launchDialog()
                
            }
        }   
        
    }
}