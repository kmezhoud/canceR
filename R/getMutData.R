#' get Mutation data for multiple genes
#' @usage
#' getMutData()
#'
#' @return a a dataframe with mutation informations
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' getMutData()
#' }
#' 
getMutData <- function(){
    
    tclRequire("BWidget")
    tclRequire("Tktable")
    
    testCheckedCaseGenProf()
    
    ProfData = 0
    i <- 0
    for (s in ENV$checked_Studies_id){
        i<-i+1
        Si =ENV$checked_StudyIndex[i]
        progressBar_ProfilesData <- tkProgressBar(title = ENV$Studies$name[Si], min = 0,
                                                  max = length(ENV$curselectGenProfs), width = 400)
        
        study_desc_position_in_genProfs <- 0
        for(c in 1:length(ENV$curselectCases)){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_ProfilesData, c, 
                             label=paste( round(c/length(ENV$curselectGenProfs)*100, 0),
                                          "% of Mutation Data"))
            
            # Avoid to select study description  
            if (ENV$curselectGenProfs[c] <= ENV$n_GenProfs[i] && 
                ENV$curselectGenProfs[c] > study_desc_position_in_genProfs){ 
                
                study_desc_position_in_genProfs <- study_desc_position_in_genProfs + ENV$n_GenProfs[i]
                
                GenProf<-ENV$GenProfsRefStudies[ENV$curselectGenProfs[c]]
                
                if (length(grep("mutation", GenProf))==0){
                    msgNoMut <- "Select Mutation data from Genetics Profiles"
                    tkmessageBox(message = msgNoMut, icon='info')
                    break
                }
                
                
                # MutData <- getMutationData(ENV$cgds,Study_id, GenProf, ENV$GeneList)
                
                MutData <- getDataByGenes(
                    api = ENV$cgds,
                    studyId = s,
                    genes = ENV$GeneList,
                    by = "hugoGeneSymbol",
                    molecularProfileIds = GenProf) |>
                    unname() |>
                    as.data.frame() |>
                    select(-c("uniqueSampleKey", "uniquePatientKey", "molecularProfileId", "sampleId", "studyId"))
                
                
                if(length(MutData[,1])==0){
                    msgNoMutData=paste("No Mutation Data are Available for\n",
                                       ENV$CasesStudies[ENV$curselectCases[c]+2])
                    tkmessageBox(message=msgNoMutData, 
                                 title= paste(ENV$StudyRefCase[c],ENV$CasesStudies[ENV$curselectCases[c]+2], 
                                              ENV$GenProfsStudies[ENV$curselectGenProfs[c]+1], sep=": "))
                    
                    
                } else{
                    ttMutData_cb <- tktoplevel()
                    tktitle(ttMutData_cb) <- paste(ENV$StudyRefCase[c],ENV$CasesStudies[ENV$curselectCases[c]+2],
                                                   ENV$GenProfsStudies[ENV$curselectGenProfs[c]+2], sep=": ")
                    
                    cbAll <- tkcheckbutton(ttMutData_cb)
                    cbAllValue <- tclVar("0")
                    tkconfigure(cbAll,variable=cbAllValue)
                    labelAll<- tklabel(ttMutData_cb,text= "All")
                    tkgrid(labelAll, cbAll)
                    
                    cbIValue=0
                    for(i in 1: length(names(MutData))){
                        
                        cbi <- paste ("cb", i, sep="")  
                        cbi <- tkcheckbutton(ttMutData_cb)
                        cbiValue <- paste("cb", i, "Value", sep="")
                        cbIValue[i] <- cbiValue
                        cbIValue[i] <- tclVar("0")
                        
                        tkconfigure(cbi,variable=cbiValue)
                        labeli <- paste ("label", i , sep="") 
                        labelI <- labeli
                        labelI <- tklabel(ttMutData_cb,text= names(MutData[i]))
                        tkgrid(labelI,cbi)
                    }
                    
                    OnOK <- function(){
                        
                        cbAllVal <- as.character(tclvalue(cbAllValue))
                        if(cbAllVal =="1"){
                            
                            title=paste(ENV$StudyRefCase[c],ENV$CasesStudies[ENV$curselectCases[c]+2], 
                                        ENV$GenProfsStudies[ENV$curselectGenProfs[c]+2], sep=": ")
                            getInTable(MutData, title=title)
                            
                        } else{
                            
                            
                            for (i in 1: length(names(MutData))){
                                cbiValue <- paste("cb", i, "Value", sep="")
                                cbIValue <- cbiValue
                                cbiVal <- paste("cb", i, "Val", sep="")
                                cbIVal<-cbiVal
                                
                                cbIVal[i] <- as.character(tclvalue(cbIValue))
                                if (cbIVal[i]=="1"){
                                    
                                    ## convert metacharacter "[""]" not supported by tclarray()
                                    #MutData_All[i]<- gsub("\\[Not Available\\]","NA", MutData[i])
                                    MutDataSub <- cbind(MutDataSub, MutData[i])
                                    
                                }
                                
                            }
                            MutDataSub<-MutDataSub[-1]
                            #MutDataSub_All[[c]]<<- MutDataSub
                            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                            #MutDataSub_All[[c]]<<- as.data.frame.array(MutDataSub, envir=.GlobalEnv)
                            #ENV$MutDataSub_All[[c]] <- as.list.data.frame(MutDataSub, envir=ENV)
                            
                            
                            name <- paste("MutDataSub", c, sep="")
                            #Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                            assign(name, MutDataSub, envir=ENV)
                            
                            title=paste(ENV$StudyRefCase[c],ENV$CasesStudies[ENV$curselectCases[c]+1],
                                        ENV$GenProfsStudies[ENV$curselectGenProfs[c]+1], sep=": ")
                            getInTable(MutDataSub, title=title)
                            
                        }
                        
                        tkdestroy(ttMutData_cb)
                    }         
                    
                    OK.but <- tkbutton(ttMutData_cb,text="OK",command=OnOK)
                    tkgrid(OK.but)
                    
                    ##Waiting to checkbox before to access to the next clinical data
                    tkwait.window(ttMutData_cb)
                }
            } 
        }
        close(progressBar_ProfilesData)
    }
}