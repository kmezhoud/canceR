#' Get Genetic Profile from selected Studies
#' @usage getGenProfs()
#' @return dataframe with genetic profil
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
#' @export
getGenProfs <- function(){
    
    #get Study Index
    #StudiesRef <- ENV$Studies |> pull("studyId")

    ## and we need the cases list of every study
    #ENV$checked_Studies_forGenProf <- ENV$checked_Studies
    #ENV$lchecked_Studies_forGenProf <- length(ENV$checked_Studies_forGenProf)

   police <- tkfont.create(family="arial", size=11)
   tkconfigure(ENV$tl, foreground="black", font=police)

    GenProfsStudies <- 0
    GenProfsRefStudies <- NULL
    for (i in seq(length(ENV$checked_StudyIndex))){

        Si = ENV$checked_StudyIndex[i]

        tkinsert(ENV$tl,"end", paste("***** Study ", Si ," : ", ENV$Studies$name[Si],"******"))
        
        ENV$GenProfs[[i]] <- cBioPortalData::molecularProfiles(ENV$cgds, ENV$checked_Studies_id[i])
        
        ENV$n_GenProfs[i] <- nrow(ENV$GenProfs[[i]])

        print(paste0("There are ", ENV$n_GenProfs[i], " Genetics Profiles in ", ENV$checked_Studies_id[i]))

        # create progress bar
        progressBar_GenProfs <- tkProgressBar(title = ENV$Studies$name[Si], min = 0,
                                              max = ENV$n_GenProfs[i], width = 400)
        j=0
        GenProfsStudy <- 0
        GenProfRefStudy <- NULL
        GenProfsRefStudy <- NULL
        for(j in seq(ENV$n_GenProfs[i])){

            Sys.sleep(0.1)
            setTkProgressBar(progressBar_GenProfs, j,
                             label=paste(round(j/ENV$n_GenProfs[i]*100, 0),
                                          "% of Genetic Profiles"))

            GenProfStudy <- ENV$GenProfs[[i]][["description"]][j]
            #GenProfStudy <- getGeneticProfiles.CGDS(ENV$cgds, ENV$checked_Studies_forGenProf[i])[,2][j]
            GenProfRefStudy <- ENV$GenProfs[[i]][["molecularProfileId"]][j]
            #GenProfRefStudy <- getGeneticProfiles.CGDS(ENV$cgds, ENV$checked_Studies_forGenProf[i])[,1][j]

            tkinsert(ENV$tl,"end", paste(j,":", GenProfStudy))
            
            GenProfsStudy <- cbind(GenProfsStudy, GenProfStudy)
            GenProfsRefStudy <- cbind(GenProfsRefStudy, GenProfRefStudy)
        }
        close(progressBar_GenProfs)

        GenProfsStudies <- cbind(GenProfsStudies, GenProfsStudy)
        GenProfsRefStudies <- cbind(GenProfsRefStudies, GenProfsRefStudy)
    }
    ENV$GenProfsRefStudies <- GenProfsRefStudies
    ENV$GenProfsStudies <- GenProfsStudies
}