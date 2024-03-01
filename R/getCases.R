#' Get cases for selected Studies. The Cases are the descrption of the samples from patients. The samples can be subdivided by the type of assays as, sequencing, CNA, Mutation, Methylation.
#' @usage 
#' getCases()
#' @export
#' @return a dataframe with cases
#' 
#' @examples
#' 
#' cgds <- cBioPortal(
#' hostname = "www.cbioportal.org",
#' protocol = "https",
#' api = "/api/v2/api-docs"
#' )
#' \dontrun{
#' getDataByGenes( api =  cgds,
#' studyId = "gbm_tcga_pub",
#' genes = c("NF1", "TP53", "ABL1"),
#' by = "hugoGeneSymbol",
#' molecularProfileIds = "gbm_tcga_pub_mrna"
#' )
#'}
#'   
getCases <- function(){

    #get Study Index 
    #StudiesRef <- ENV$Studies |> pull("studyId")
    #checked_StudyIndex
    #ENV$checked_StudyIndex_forCases <- ENV$checked_StudyIndex
    
    ## and we need the cases list of every study
    #checked_Studies_forCases <- ENV$checked_Studies_id
    #ENV$checked_Studies_forCases <- checked_Studies_forCases
    #lchecked_Studies_forCases <- length(checked_Studies_forCases)
    #ENV$lchecked_Studies_forCases <- lchecked_Studies_forCases
    
    
    police <- tkfont.create(family="arial", size=11)
    tkconfigure(ENV$tc, foreground="black", font=police)
    
    CasesStudies <- 0
    CasesRefStudies <- NULL
    #ENV$Cases <- NULL
    for (i in seq(length(ENV$checked_StudyIndex))){
        
        Si <- ENV$checked_StudyIndex[i]
        
        tkinsert(ENV$tc,"end", paste("***** Study ", Si ," : ", ENV$Studies$name[Si], "*****"))
        
        ENV$Cases[[i]] <- cBioPortalData::sampleLists(ENV$cgds, ENV$checked_Studies_id[i])
        
        ENV$n_Cases[i] <- nrow(ENV$Cases[[i]])
        
        print(paste("There are", ENV$n_Cases[i], "Cases in", ENV$checked_Studies_id[i], sep=" "))
        
        # create progress bar
        progressBar_Cases <- tkProgressBar(title = ENV$Studies$name[Si], min = 0,
                                           max = ENV$n_Cases[i], width = 400)
        
        j <- 0
        CasesStudy <- 0
        CaseRefStudy <- NULL
        CasesRefStudy <-  NULL
        for (j in seq(ENV$n_Cases[i])){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_Cases, j, 
                             label=paste(round(j/ENV$n_Cases[i]*100, 0),
                                                                "% of Cases"))
            CaseStudy <- ENV$Cases[[i]][["description"]][j]
            #print(CaseStudy)
            #CaseStudy <- getCaseLists(ENV$cgds,checked_Studies_id[i])[,3][j]
            CaseRefStudy <- ENV$Cases[[i]][["studyId"]][j]
            #print(CaseRefStudy)
            #CaseRefStudy <- getCaseLists(ENV$cgds, checked_Studies_id[i])[,1][j]
    
            tkinsert(ENV$tc,"end", paste(j,":", CaseStudy))
            
            CasesStudy <- c(CasesStudy, CaseStudy)
            CasesRefStudy <- c(CasesRefStudy,CaseRefStudy)
        }
        ##Close ProgressBar_Cases
        close(progressBar_Cases)
        CasesStudies <- c(CasesStudies, CasesStudy)
        CasesRefStudies <- c(CasesRefStudies, CasesRefStudy)
        
    }
    
    ENV$CasesRefStudies <- CasesRefStudies
    ENV$CasesStudies <- CasesStudies
    
}