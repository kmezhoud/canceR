#' Search and get genetic profiles (CNA,mRNA, Methylation, Mutation...)
#'
#' @details See \url{https://github.com/kmezhoud/bioCancer/wiki}
#'
#' @return A data frame with Genetic profile
#'
#' @usage getProfData(study,genProf, listGenProf, GeneList, Mut)
#' @param study  Study ID
#' @param genProf Genetic Profile id (cancer_study_id_[mutations, cna, methylation, mrna ]).
#' @param listGenProf A list of Genetic Profiles for one study.
#' @param GeneList A list of genes
#' @param Mut Condition to set if the genetic profile is mutation or not (0,1)
#'
#' @examples
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
#'@export
getProfData<-function(study, genProf, listGenProf, GeneList, Mut){
    
    
    if(length(grep(genProf, listGenProf))!= 0){
        
        ProfData_X <- cBioPortalData::getDataByGenes(api =  ENV$cgds,
                                                     studyId = study,
                                                     genes = GeneList,
                                                     by = "hugoGeneSymbol",
                                                     molecularProfileIds = genProf) |>
            unname() |>
            as.data.frame()
        
        # avoid error if no profile data exist for geneList
        if(nrow(ProfData_X)<1){
            #all(dim(ProfData_X)==c(0,1) ||
            #nrow(ProfData_X)==0)== TRUE){ #length(ProfData_X)== 0 ||
            
            #print(paste0("No Profile Data available for:", genProf))
            
            ## built empty data frame with gene Symbol in colnames
            if(Mut==0){
                
                ProfData_X <- as.data.frame(setNames(replicate(length(GeneList),numeric(1),
                                                               simplify = FALSE),
                                                     GeneList[order(GeneList)]))
                return(ProfData_X)
                
            }else if (Mut==1){
                ## built emty data.frame as the same form of MutData
                hugoGeneSymbol <- as.vector(GeneList)
                proteinChange <- rep(character(1), length(GeneList))
                variantType <- rep(character(1), length(GeneList))
                MutData <- data.frame(hugoGeneSymbol, proteinChange, variantType)
                
                return(MutData)
            }
            
        }else if(Mut== 0){
            
            ProfData_X <- ProfData_X |>
                #.[[1]] |>
                select("sampleId", "hugoGeneSymbol", "value") |>
                tidyr::spread("hugoGeneSymbol", "value") |>
                data.frame(row.names = 1)
            ##  check the order of geneList and add gene with empty data NA.
            missing <- setdiff(GeneList[order(GeneList)], names(ProfData_X))
            ProfData_X[missing] <- NA
            ProfData_X |> select(GeneList[order(GeneList)])
            
            return(ProfData_X)
            
        } else if(Mut==1){
            
            #print(paste0("Getting Mutation Data of ", study ," ..."))
            
            MutData <- ProfData_X  |>
                #.[[1]] |>
                select("hugoGeneSymbol", "proteinChange", "variantType")
            
            return(MutData)
            
        }
    }else{
        
        msgNoGenProf= paste("There is no genetic Profiles: ",
                                     genProf )
        
        tkmessageBox(message= msgNoGenProf)
        #stop(msgNoGenProf)
        
        print(paste("There is no genetic Profiles: ", genProf ))
        
        ProfData_X <- as.data.frame(setNames(replicate(
            length(GeneList),numeric(1), simplify = FALSE),
            GeneList[order(GeneList)]))
        
        return(ProfData_X)
    }
}



#' Unify row names in data frame with the same order of gene list.
#' @usage UnifyRowNames(x,geneList)
#' @param x data frame with gene symbol in the row name
#' @param geneList a gene list
#'
#' @return a data frame having the gene in row name ordered as in gene list.
#'
#' @examples
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
UnifyRowNames <- function(x, geneList){
    ## compute the ratio of mutation
    df_MutData <-as.data.frame(table(x$hugoGeneSymbol) /sum(table(x$hugoGeneSymbol))*100)
    ## compute le sum of mutation using table function.
    #df_MutData <-as.data.frame(table(x$hugoGeneSymbol))
    rownames(df_MutData) <- df_MutData$Var1  #tibble::column_to_rownames("sampleId")
    ## ordering genes in MutData as in GeneList
    
    df_GeneList <- as.data.frame(t(geneList))
    #df_GeneList <- as.data.frame(GeneList)
    rownames(df_GeneList) <- df_GeneList[,1]
    df_merge <- merge(df_GeneList, df_MutData, by="row.names",all.x=TRUE)
    Freq_Mut <- df_merge[,c(-2,-3)]
    return(Freq_Mut)
}


#' get mutation frequency
#'
#' @usage getFreqMutData(list, GeneList)
#'
#' @param list a list of data frame with mutation data. Each data frame is for one study
#' @param GeneList file name of GeneList examples: "73"
#'
#' @return a data frame with mutation frequency. gene is in rows and study is in column
#' @export
#'
#' @examples
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
getFreqMutData <- function(list, GeneList){
    
    #GeneList <- whichGeneList(GeneListLabel)
    
    if(is.null(list)){stop("Select a less one Study.")}
    
    Freq_ListMutData <- lapply(list,
                               function(x) UnifyRowNames(x, GeneList))
    
    ## convert the list of correlation matrices to Array
    Freq_ArrayMutData <- array(unlist(Freq_ListMutData),
                               dim = c(nrow(Freq_ListMutData[[1]]),
                                       ncol( Freq_ListMutData[[1]]),
                                       length(Freq_ListMutData)))
    
    if (inherits(try(dimnames(Freq_ArrayMutData) <-
                     list(Freq_ListMutData[[1]][,1],
                          colnames(Freq_ListMutData[[1]]),
                          names(Freq_ListMutData)),
                     silent=TRUE),"try-error")){
        p("There is a Study without Mutation Data.
      Use Mutation Panel to verify mutations data for selected studies.",
          align="center", style = "color:blue")
    }else{
        dimnames(Freq_ArrayMutData) <-
            list(Freq_ListMutData[[1]][,1],
                 colnames(Freq_ListMutData[[1]]),
                 names(Freq_ListMutData))
    }
    #   ?getListProfData(Genes= empty)
    if(dim(Freq_ArrayMutData)[3]==1){
        Freq_DfMutData <- as.numeric(Freq_ArrayMutData[,2,])
        names(Freq_DfMutData) <- names(Freq_ArrayMutData[,2,])
        ## ordering gene list as in GeneList from MSigDB:
        ## grouping genes with the same biological process or gene Sets
        Freq_DfMutData <- Freq_DfMutData[GeneList]
        Freq_DfMutData <- data.frame(round(Freq_DfMutData,digits=2))
        names(Freq_DfMutData) <- names(Freq_ListMutData)
    }else{
        Freq_DfMutData <- apply(Freq_ArrayMutData[,2,],2,as.numeric)
        rownames(Freq_DfMutData) <- rownames(Freq_ArrayMutData[,2,])
        ## ordering gene list as in GeneList from MSigDB:
        ## grouping genes with the same biological process or gene Sets
        Freq_DfMutData <- Freq_DfMutData[GeneList,,drop=FALSE]
        Freq_DfMutData <- data.frame(round(Freq_DfMutData,digits=2))
    }
    return(Freq_DfMutData)
}



#' Get list of data frame with profiles data (CNA,mRNA, Methylation, Mutation...)
#'
#' @usage getListProfData(checked_Studies, geneList)
#'
#' @param checked_Studies checked studies in corresponding panel (input$StudiesIDCircos, input$StudiesIDReactome).
#' @param geneList  GeneList with Hugo Symbol
#'
#' @return A LIST of profiles data (CNA, mRNA, Methylation, Mutation, miRNA, RPPA).
#'         Each dimension content a list of studies.
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' \dontrun{
#' getListProfData()
#' head(ENV$ProfData$Expression)
#' }
#' 
getListProfData <- function(checked_Studies, geneList){

# if(exists("ListProfData", envir = ENV)){
#     rm("ListProfData", envir=ENV)
# }
# if(exists("ListMetData", envir = ENV)){
#     rm("ListMetData", envir=ENV)
# }
# if(exists("ListMutData", envir = ENV)){
#     rm("ListMutData", envir=ENV)
# }
    
    ENV$ListProfData <- NULL
    ENV$ListMetData <- NULL
    ENV$ListMutData <- NULL
    

    #Lchecked_Studies <- ENV$lchecked_Studies

    #get Study references
    #StudiesRef <- getCancerStudies(ENV$cgds)[,1]


    #LengthGenProfs <- 0
    #LengthCases <- 0
    i <- 0
    for (s in checked_Studies){
        i <- i+1
        #Si = ENV$checked_StudyIndex[i]
        progressBar_ProfilesData <- tkProgressBar(title = paste(s,":"), 
                                                  min = 0,
                                                  max = length(checked_Studies), 
                                                  width = 400)

        Sys.sleep(0.1)
        setTkProgressBar(progressBar_ProfilesData, i, 
                         label=paste(round(i/length(checked_Studies)*100, 0),
                                     "% of Profiles Data"))

        ### get Cases and Genetic Profiles  with cgdsr references
        GenProf_CNA<- paste(s,"_gistic", sep="")
        #Case_CNA   <- paste(s,"_cna", sep="")

        GenProf_Exp<- paste(s,"_rna_seq_v2_mrna", sep="")
        #Case_Exp   <- paste(s,"_rna_seq_v2_mrna", sep="")

        GenProf_Met_HM450<- paste(s,"_methylation_hm450", sep="")
        #Case_Met_HM450   <- paste(s,"_methylation_hm450", sep="")

        GenProf_Met_HM27<- paste(s,"_methylation_hm27", sep="")
        #Case_Met_HM27   <- paste(s,"_methylation_hm27", sep="")

        GenProf_RPPA<- paste(s,"_RPPA_protein_level", sep="")
        #Case_RPPA   <- paste(s,"_rppa", sep="")

        GenProf_miRNA<- paste(s,"_mirna", sep="")
        #Case_miRNA   <- paste(s,"_microrna", sep="")

        GenProf_Mut<- paste(s,"_mutations", sep="")
        #Case_Mut   <- paste(s,"_sequenced", sep="")
        
        
        ## get Genetics Profiles for selected each Study
        GenProfsRefStudies <- unname(unlist(apply(
            as.data.frame(s), 1,
            function(x) molecularProfiles(api = ENV$cgds, studyId = x)[,"molecularProfileId"])))

        if (length(geneList) > 0){
            print(GenProf_Exp)
            
            ProfData_CNA <- getProfData(s, GenProf_CNA,
                                        GenProfsRefStudies, geneList, Mut=0)
            
            ProfData_Exp <- getProfData(s, GenProf_Exp,
                                        GenProfsRefStudies, geneList, Mut=0)
            
            ProfData_Met_HM450 <- getProfData(s, GenProf_Met_HM450,
                                              GenProfsRefStudies, geneList, Mut=0)
            
            ProfData_Met_HM27 <- getProfData(s, GenProf_Met_HM27,
                                             GenProfsRefStudies, geneList, Mut=0)
            
            ProfData_RPPA <- getProfData(s, GenProf_RPPA,
                                         GenProfsRefStudies, geneList, Mut=0)
            
            ProfData_miRNA <- getProfData(s, GenProf_miRNA,
                                          GenProfsRefStudies, geneList, Mut=0)
            
            MutData <- getProfData(s,GenProf_Mut,
                                   GenProfsRefStudies, ENV$GeneList, Mut=1)
            

            # ProfData_CNA<- grepRef(Case_CNA, ENV$CasesRefStudies, GenProf_CNA,
            #                        ENV$GenProfsRefStudies, ENV$GeneList, Mut=0)
            # ProfData_Exp<- grepRef(Case_Exp, ENV$CasesRefStudies, GenProf_Exp,
            #                        ENV$GenProfsRefStudies, ENV$GeneList, Mut=0)
            # ProfData_Met_HM450 <- grepRef(Case_Met_HM450, ENV$CasesRefStudies,  GenProf_Met_HM450,
            #                               ENV$GenProfsRefStudies, ENV$GeneList, Mut=0)
            # ProfData_Met_HM27 <- grepRef(Case_Met_HM27, ENV$CasesRefStudies, 
            #                              GenProf_Met_HM27, ENV$GenProfsRefStudies, ENV$GeneList,Mut=0)
            # ProfData_RPPA<- grepRef(Case_RPPA, ENV$CasesRefStudies,
            #                         GenProf_RPPA, ENV$GenProfsRefStudies, ENV$GeneList,Mut=0)
            # ProfData_miRNA<- grepRef(Case_miRNA, ENV$CasesRefStudies,
            #                          GenProf_miRNA, ENV$GenProfsRefStudies, ENV$GeneList,Mut=0)
            # MutData <- grepRef(Case_Mut,ENV$CasesRefStudies ,GenProf_Mut,
            #                    ENV$GenProfsRefStudies,ENV$GeneList, Mut=1)

        } else {
            tkmessageBox(message= "Load gene List", icon="warning")
            close(progressBar_ProfilesData)
            stop("Load Gene List")
        }

        ENV$ListProfData$CNA[[s]] <- ProfData_CNA
        ENV$ListProfData$Expression[[s]] <- ProfData_Exp
        ENV$ListMetData$HM450[[s]] <- ProfData_Met_HM450
        ENV$ListMetData$HM27[[s]] <- ProfData_Met_HM27
        ENV$ListProfData$RPPA[[s]] <- ProfData_RPPA
        ENV$ListProfData$miRNA[[s]] <- ProfData_miRNA
        ENV$ListMutData[[s]] <- MutData

        print(" End Getting Profiles Data... ")
        close(progressBar_ProfilesData)
    }

    print("Start getting Frequency of Mutation ...")
    
    ENV$Freq_DfMutData <- getFreqMutData(ENV$ListMutData, geneList)

    print("End getting Mutation Frequency...")

}
