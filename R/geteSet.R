#' Built Expression Set (eSet) from profile data.
#' @usage geteSet()
#' @export
#' @return ExpressionSet
#' @examples 
#'  f <- 9
#'  \dontrun{
#'  readRDS(paste(path.package("canceR"),"/extdata/rdata/prad_michPhenoTest1021.rds", sep=""))
#'  geteSet()
#'  }
#' 
geteSet <- function(){
    
    #function to replace blanks with missing
    
    blank2na <- function(x){ 
        z <- gsub("\\s+", "", x)  #make sure it's "" and not " " etc
        x[z==""] <- NA 
        return(x)
    } 
        ##Test checked Cases and Genetic Profiles
        testCheckedCaseGenProf()
       
        
        ProfData=0
        i<-0
        for (s in ENV$checked_Studies_id){
            i<-i+1
            Si = ENV$checked_StudyIndex[i]
            progressBar_ProfilesData <- tkProgressBar(title = ENV$Studies$name[Si], min = 0,
                                                      max = length(ENV$curselectGenProfs), width = 400)
            
            
            study_desc_position_in_genProfs <- 0
            for (k in 1:length(ENV$curselectCases)){
                
                Sys.sleep(0.1)
                setTkProgressBar(progressBar_ProfilesData, k, 
                                 label=paste(round(k/length(ENV$curselectGenProfs)*100, 0),
                                             "% of Profiles Data"))
    
                
                # Avoid to select study description  
                if (ENV$curselectGenProfs[k] <= ENV$n_GenProfs[i] && 
                    ENV$curselectGenProfs[k] > study_desc_position_in_genProfs){    
                    
                    study_desc_position_in_genProfs <- study_desc_position_in_genProfs + ENV$n_GenProfs[i]
                    
                    GenProf <- ENV$GenProfsRefStudies[ENV$curselectGenProfs[k]]
            
                    print(GenProf)
                    print(s)
                    ## verify if GenProf has expression data
                    if (length(grep("mrna", GenProf, ignore.case = TRUE))==0 && 
                        length(grep("gistic", GenProf, ignore.case = TRUE))==0 &&
                        length(grep("CNA", GenProf, ignore.case = TRUE))==0){
                        msgNoExp <- "Select Expression data from Genetics Profiles"
                        tkmessageBox(message = msgNoExp, icon='info')
                        break
                    }  

                    ProfData <- getDataByGenes(
                        api =  ENV$cgds,
                        studyId = s,
                        genes = ENV$GeneList,  #c("NF1", "TP53", "ABL1")
                        by = "hugoGeneSymbol",
                        molecularProfileIds = GenProf)  |>
                        unname() |>
                        as.data.frame() |>
                        select("hugoGeneSymbol","sampleId", "value") |>
                        tidyr::spread("hugoGeneSymbol", "value") |>
                        mutate(across(-c("sampleId"), \(x)round(x, digits = 3)))
                        #data.frame(row.names = 1)

                    ProfData <<- ProfData 
                    print("getting Profile Data and removing all NAs rows...")
                    ##remove all NAs rows
                    ProfData <- ProfData[which( apply(!( apply(ProfData,1,is.na) ),2,sum)!=0 ),]

                    ## Display AssyData with Tcl Table
                    title <- paste0(ENV$StudyRefGenProf[k],": ", ENV$GenProfChoice[k])
                    getInTable(ProfData, title)
                    
                    #####nicData_MultipleCases function
                    #Case<- ENV$CasesRefStudies[ENV$curselectCases[k]]
                    
                    ClinicalData <- cBioPortalData::clinicalData(ENV$cgds, s) |>
                        select("sampleId", dplyr::everything()) 
                        #data.frame(row.names = 1)
                   
                    #ClinicalData <<- ClinicalData
                    
                    if(nrow(ClinicalData)==0){
                        msgNoClinData=paste("No Clinical Data are Available for\n", CasesStudies[curselectCases[k]+1])
                        tkmessageBox(message=msgNoClinData, title= paste("Study: ",ENV$StudyRefCase[k]))
                        close(progressBar_ProfilesData)
                        break
                    }else{
                        ## getClinicalData generate CHARACTER class if is there "NA" value in any column
                        ## Convert character value to numeric if grep [0-9] != 0
                        # for(i in 1:ncol(ClinicalData)){
                        #     ## substitute  NO digital value to NA
                        #     ClinicalData[,i]<- gsub("^\\D.*",NA, ClinicalData[,i], ignore.case=TRUE)
                        #     ## substitte "NA" charcter to NA 
                        #     ClinicalData[,i]<- gsub("NA",NA, ClinicalData[,i], ignore.case=TRUE)
                        #     ## substiture space by NA
                        #     ClinicalData[,i]<-gsub("\\s+", NA,  ClinicalData[,i], ignore.case=TRUE) 
                        #     if(length(grep("-?[0-9]*\\.[0-9]*",ClinicalData[,i]))!=0){
                        #         ClinicalData[,i] <- as.numeric(ClinicalData[,i])
                        #     }
                        # }
                        
                        title <- paste(ENV$StudyRefCase[k],": ", ENV$CaseChoice[k])
                        getInTable(ClinicalData,title)
                    }       
                    
                    
                    
                    names_Clinical_Data <- colnames(ClinicalData)
                    names_ProfData <- colnames(ProfData)
                    
                    clinical_profData <- ClinicalData |>
                        left_join(ProfData, by="sampleId")
                    
                    
                    AssayData <- clinical_profData |>
                        select(all_of(names_ProfData)) |>
                        data.frame(row.names = 1) |>
                        t() |>
                        as.matrix()
                    
                    ClinicalData <- ClinicalData |>
                                    data.frame(row.names = 1)


                    # matrix <-rbind(colnames(ClinicalData), ClinicalData)
                    # rnames <- rownames(ClinicalData)
                    # cnames <- colnames(ClinicalData)
                    
                    #apply blank2na function
                    # ClinicalData <- data.frame(lapply(ClinicalData,  blank2na))
                    # rownames(ClinicalData) <- rnames
                    # names(ClinicalData) <- cnames
                  
                    ## Select only Cases (rownames) that exist in ClinicalDataSub and ProfData
                    #merge <- merge(ClinicalData, ProfData, by="row.names")
                    #print("merge Clinical and Profile Data")
                    #ClinicalData <- merge[,1:(length(ClinicalData)+1)]
                    
                    # rownames(ClinicalData) <- ClinicalData[,1]
                    # ClinicalData <- ClinicalData[-1]
                    # ProfData <- merge[,!(merge %in% ClinicalData)]
        
                    # AssayData<- t(ProfData)
                    # colnames(AssayData) <- AssayData[1,]
                    # AssayData <- AssayData[-1,]
                    # rnames <- rownames(AssayData) 
                    # AssayData <- as.matrix(apply(t(ProfData),2 ,function(x) as.numeric(x)))
                    # rownames(AssayData) <- rnames
                    
                    
                    ##Convert column with digital values from factor to numeric
                    for(i in 1:ncol(ClinicalData)){
                        ClinicalData[,i] <- sapply(ClinicalData[,i], 
                                                   function(x) if(length(grep("[a-z'-'+A-Z'/'' ']", 
                                as.character(ClinicalData[,i])))==0) { as.numeric(as.character(x)) } else {x})
                    }
                    
                
                    ENV$ClinicalData <- ClinicalData
                    ENV$ProfData <- t(AssayData)
                    ENV$AssayData <- AssayData
                    
                    
                    #Test if the same length cases for phenoData and AssayData
                    if (all(rownames(ClinicalData)==colnames(AssayData))){

                        ## create labelDescription for columns of phenoData. 
                        ## labeldescription is used by Biobase packages
                        ## In our case labelDescription is Equal to column names
                        metaData <- data.frame(labelDescription= colnames(ClinicalData), row.names=colnames(ClinicalData))        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame   
                        ##that conveniently stores and manipulates 
                        ##the phenotypic data and its metadata in a coordinated fashion. 
                        phenoData <- new("AnnotatedDataFrame", data=ClinicalData, varMetadata=metaData)    
                        
                        ##Assembling an ExpressionSet  
                        ENV$eSet<-Biobase::ExpressionSet(assayData=AssayData, phenoData=phenoData, annotation="GO") 
                        print(paste("End of building eSet..."))
                        
                        #             for (i in 1:length(names(pData(eSet)))){
                        #             pData(eSet)[i] <- as.matrix(na.omit(pData(eSet)[i]))
                        #             }
                    }else {tkmessageBox( message= "The expression Gene Set and the Clinical Data do not have the same samples", icon="warning")}
                } 
            } 
            close(progressBar_ProfilesData)
        } 
        
}