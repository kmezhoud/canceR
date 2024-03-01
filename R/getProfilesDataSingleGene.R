#' get Profiles Data  for a Single Gene.
#' @usage getProfilesDataSingleGene()
#' @return dataframe with profiles data for a single gene
#' @export
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' ## Select Case from Breast Cancer
#'  ENV <- new.env(parent = emptyenv())
#' ENV$curselectCases <- 9
#' ##Select Genetic Profile from Breast Cancer
#' ENV$curselectGenProfs <- 4
#' ## get Specific Mutation data for 73 Genes list
#' \dontrun{
#' getProfilesDataSingleGene()
#' }
getProfilesDataSingleGene <-function(){
    
    tclRequire("BWidget")
    tclRequire("Tktable")
    
    testCheckedCaseGenProf(singleGene=1)

    
    for (s in ENV$checked_Studies_id){
        
        for(c in seq(length(ENV$curselectGenProfs))){

                
                launchDialog <- function(){
                    
                    Dialog_Title<- paste("STUDY:", s ,"CASE:", ENV$curselectCases_forStudy[c], sep=" ")
                    GENE <- modalDialog(Dialog_Title, "Enter HUGO Gene Symbol", "MDM4")
                    if (GENE == "ID_CANCEL") return()
                    
                    #ProfDataS<-getProfileData(ENV$cgds, GENE, GenProfS, Study_id)
                    print(ENV$GenProfsRefStudies[ENV$curselectGenProfs[c]])
                    print(s)
                    ProfDataS <- cBioPortalData::getDataByGenes(api =  ENV$cgds,
                                                                studyId = s,
                                                                genes = GENE,
                                                                by = "hugoGeneSymbol", 
                                                                molecularProfileIds = ENV$GenProfsRefStudies[ENV$curselectGenProfs[c]]) |>
                        unname() |>
                        as.data.frame() |>
                        select("hugoGeneSymbol","sampleId", "value") |>
                        tidyr::spread("hugoGeneSymbol", "value") 
                        #data.frame(row.names = 1)
                    
                    
                    ttProfData_cb <- tktoplevel()
                    tktitle(ttProfData_cb) <- paste(ENV$StudyRefCase[c],ENV$CaseChoice[c], sep=": ")
                    #tkwm.geometry(ttProfData_cb,"300x300")
                    
                    cbAll <- tkcheckbutton(ttProfData_cb)
                    cbAllValue <- tclVar("0")
                    tkconfigure(cbAll, variable = cbAllValue)
                    labelAll<- tklabel(ttProfData_cb, text= "All")
                    tkgrid(labelAll, cbAll)
                    
                    cbIValue=0
                    for(i in seq(ncol(ProfDataS))){
                        
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
                            
                            #ProfDataS <- t(t(ProfDataS))
                            getInTable(ProfDataS,
                                       title= paste0(ENV$StudyRefCase[c],": ",
                                                    ENV$CaseChoice[c]))
 
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
                                    ProfDataS[,i] <- gsub("\\[Not Available\\]","NA", ProfDataS[,i])
                                    
                                    ProfDataSSub <- cbind(ProfDataSSub, ProfDataS[i])
                                }
                            }
                            ProfDataSSub <- ProfDataSSub[-1]
                            
                            if(length(ProfDataSSub)==0){
                                tkmessageBox(message= paste("Select at least one data type"), icon="warning")
                                stop("Select at least one data type")
                            }
                            
                            #ProfDataSSub <- t(t(ProfDataSSub))
                            getInTable(ProfDataSSub, 
                                       title= paste0(ENV$StudyRefCase[c],": ",
                                                     ENV$CaseChoice[c]))
                            
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
                
#            }
        }   
        
    }
}