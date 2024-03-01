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
    
    ######### Test if all cases were corresponded to appropriate Gen profs (same Study)
    # for (i in 1:length(ENV$curselectCases)){
    #     if(ENV$StudyRefCase[i]!=ENV$StudyRefGenProf[i]){
    #         
    #         msgBadChoice="Correspond the Genetic Profile to the Case for the same Study"
    #         tkmessageBox(message=msgBadChoice, icon="warning")
    #         tkfocus(ENV$ttCasesGenProfs)
    #         stop("Correspond the Genetic Profile to the Case for the same Study")
    #         
    #     }
    # }
    
    #ProfDataAll =0
    ProfData = 0
    i <- 0
    for (s in ENV$checked_Studies_id){
        i<-i+1
        Si =ENV$checked_StudyIndex[i]
        progressBar_ProfilesData <- tkProgressBar(title = ENV$Studies$name[Si], min = 0,
                                                  max = length(ENV$curselectGenProfs), width = 400)
        
        #tkfocus(progressBar_ProfilesData)
        
        study_desc_position_in_genProfs <- 0
        for (k in 1:length(ENV$curselectGenProfs)){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_ProfilesData, k, 
                             label=paste( round(k/length(ENV$curselectGenProfs)*100, 0),
                                                                       "% of Methylation Data"))
            
            
            # Avoid to select study description  
            if (ENV$curselectGenProfs[k] <= ENV$n_GenProfs[i] && 
                ENV$curselectGenProfs[k] > study_desc_position_in_genProfs){ 
                
                study_desc_position_in_genProfs <- study_desc_position_in_genProfs + ENV$n_GenProfs[i]
                
                GenProf <- ENV$GenProfsRefStudies[ENV$curselectGenProfs[k]]
                
                if (length(grep("methylation", GenProf))==0){
                    msgNoMeth <- "Select Methylation data from Genetics Profiles"
                    tkmessageBox(message = msgNoMeth, icon='info')
                    break
                }
                
                # if (length(grep("methylation", s ))==0){
                #     msgNoMeth <- "Select Methylation data from Cases"
                #     tkmessageBox(message = msgNoMeth, icon='info')
                #     break
                # }
                
                
                #ProfData <- getProfileData(ENV$cgds,ENV$GeneList, GenProf,Study_id)
                ProfData <-  cBioPortalData::getDataByGenes(api = ENV$cgds,
                                               studyId = s,
                                               genes = ENV$GeneList,
                                               by = "hugoGeneSymbol",
                                               molecularProfileIds = GenProf) |>
                    unname() |>
                    as.data.frame()|>
                    select("sampleId", "hugoGeneSymbol", "value") |>
                    tidyr::spread("hugoGeneSymbol", "value") |>
                    data.frame(row.names = 1)
                   #tibble::column_to_rownames("sampleId")

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