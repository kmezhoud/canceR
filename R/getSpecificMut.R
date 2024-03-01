#' get specific Mutation data for multiple genes
#' @usage
#' getSpecificMut()
#'
#' @return a a dataframe with specific mutation informations
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata//ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' getSpecificMut()
#' }
#' 
getSpecificMut <- function(){
    
    
    tclRequire("BWidget")
    tclRequire("Tktable")
    
    
        testCheckedCaseGenProf()
    
    
        
        Lchecked_Studies <- ENV$lchecked_Studies
        Lchecked_Cases <- length(ENV$curselectCases)
        Lchecked_GenProf <- length(ENV$curselectGenProfs)
        ###########################################################
        MutData=0
        MutData_All <-NULL
        MutDataSub<-0
        MutDataSub_All <- NULL
        
        for(c in 1:Lchecked_Cases){
            
            GenProf <- ENV$GenProfsRefStudies[ENV$curselectGenProfs[c]]
            Study_id <- ENV$CasesRefStudies[ENV$curselectCases[c]]
            
            MutData <- getDataByGenes(
                api = ENV$cgds,
                studyId = Study_id,
                genes = ENV$GeneList,
                by = "hugoGeneSymbol",
                molecularProfileIds = GenProf) |>
                unname() |>
                as.data.frame() |>
                select(-c("uniqueSampleKey", "uniquePatientKey", "molecularProfileId", "sampleId", "studyId"))
            
            if(length(MutData[,1])==0){
                msgNoMutData=paste("No Mutation Data are Available for\n", 
                                   ENV$CasesStudies[ENV$curselectCases[c]+1])
                tkmessageBox(message=msgNoMutData, 
                             title= paste(ENV$StudyRefCase[c],
                             ENV$CasesStudies[ENV$curselectCases[c]+1], 
                             ENV$GenProfsStudies[ENV$curselectCases[c]+1], sep=": "))
                
                
            } else{
                
                dialogSpecificMut(MutData, c)
                
                
                
            }
            
        }
    
}