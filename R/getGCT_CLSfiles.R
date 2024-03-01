#' get Profile (GCT file) and Phenotype (CLS file) Data from Disease.
#' @usage getGCT_CLSfiles()
#' @return GCT and CLS files paths
#' @export
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' getGCT_CLSfiles()
#' }
getGCT_CLSfiles <- function(){
    
    tclRequire("BWidget")
    tclRequire("Tktable")
    
    ## verify the rightness of checked Case Gene. Prof.
    testCheckedCaseGenProf()
    
    ####Create a new directory "Results" for All Profiles Data
    ##Set Results as tempary work directory
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(!file.exists("Results/")){
        dir.create(file.path(paste(getwd(), "/Results", sep="")), showWarnings = FALSE)
        dir.create(file.path(paste(getwd(), "/Results/gct_cls", sep="")), showWarnings = FALSE)
    } else if (!file.exists("Results/gct_cls/")){
        dir.create(file.path(paste(getwd(), "/Results/gct_cls", sep="")), showWarnings = FALSE)
    }
    
    ProfData=0
    i<-0
    for (s in ENV$checked_Studies_id){
        i <- i+1
        Si =ENV$checked_StudyIndex[i]
        progressBar_ProfilesData <- tkProgressBar(title = ENV$Studies$name[Si], min = 0,
                                                  max = length(ENV$curselectGenProfs), width = 400)
        
        ####Tree ROOT==Studies
        StudyiChoice <- paste("Study",i,"Choice", sep="")
        study_desc_position_in_genProfs <- 0
        
        for (k in 1:length(ENV$curselectGenProfs)){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_ProfilesData, k, 
                             label=paste(round(k/length(ENV$curselectGenProfs)*100, 0),
                                         "% of Profiles Data"))
            
            # Avoid to select study description  
            if (ENV$curselectGenProfs[k] <= ENV$n_GenProfs[i] && 
                ENV$curselectGenProfs[k] > study_desc_position_in_genProfs){    
                
                study_desc_position_in_genProfs <- study_desc_position_in_genProfs + ENV$n_GenProfs[i]   
                
                GenProf <- ENV$GenProfsRefStudies[ENV$curselectGenProfs[k]]
                
                ReturnDialogSamplingGSEA <- dialogSamplingGSEA(length(ENV$curselectGenProfs))
                
                if(ReturnDialogSamplingGSEA[1] == "ID_CANCEL"){
                    stop()
                }else if(length(ENV$GeneList) > 1000){ 
                    
                    
                    ProfData <- getDataByGenes(
                        api =  ENV$cgds,
                        studyId = s,
                        genes = ENV$GeneList,  #c("NF1", "TP53", "ABL1")
                        by = "hugoGeneSymbol",
                        molecularProfileIds = GenProf)  |>
                        unname() |>
                        as.data.frame() |>
                        select("hugoGeneSymbol","sampleId", "value") |>
                        tidyr::spread("hugoGeneSymbol", "value") 
                        #data.frame(row.names = 1)
                    
                    ProfData <<- ProfData
                    ##Convert data frame to numeric structure
                    #                     print("converting data frame of Profile data to numeric stucture...")
                    #                     for(i in 1:ncol(ProfData)){
                    #                         
                    #                         ProfData[,i] <- as.numeric(ProfData[,i])
                    #                     }
                    
                    ## substitute "NaN" by "NA"
                    if(length(grep("NaN", ProfData))!=0){
                        
                        print("This step takes a while to remove NaN string and
                              convert them to NA (12000 genes takes about 10 min)")
                        for (j in 1:ncol(ProfData)){
                            #ProfData[,i]<- as.numeric(gsub("\\[Not Available\\]","na", ProfData[,i]))
                            #ProfData <- sapply(ProfData, function(x) gsub("NA", "na", x))
                            ProfData[,j]<- as.numeric(gsub("NaN",NA, ProfData[,j]))
                            #ProfData[,i]<- as.numeric(gsub(NA,"na", ProfData[,i]))
                        }
                    }
                    print("Removing genes without data... ")
                    ##remove all NAs rows
                    ProfData <- ProfData[which( apply( !( apply(ProfData,1,is.na) ),2,sum)!=0 ),]
                    print(paste("dimension of Profile Data:"))
                    print(dim(ProfData))
                    ## When matrix has negative values,  simple space are generated before every positive value. 
                    ## This space cause deleting all columns from matrix when we try to remove the columns without values NAs
                    # if(min(ProfData, na.rm = TRUE) < 0){
                    #     for (j in 1:ncol(ProfData)){
                    #         
                    #         ProfData[,j]<- as.numeric(gsub(" ","", ProfData[,j]))
                    #         
                    #     }
                    # }
                    
                    ##remove all NAs columns only for CNA profile Data
                    #if(length(grep("cna",ENV$CaseChoice[k], ignore.case = TRUE))!=0){
                    #ProfData<- ProfData[,-which( apply( !( apply(ProfData,1,is.na) ),1,sum)==0 )]
                    #}
                    
                    if(nrow(ProfData) < ReturnDialogSamplingGSEA){
                        tkmessageBox(message= paste("After cleaning data frame, it remains: ", 
                                                    nrow(ProfData), "samples < ", ReturnDialogSamplingGSEA,
                                                    ". Select",nrow(ProfData), 
                                                    "as the size of sampling and repeat the request." ), icon="info")
                        close(progressBar_ProfilesData)
                        stop(paste("After cleaning data frame, it remains: ",
                                   nrow(ProfData), "samples < ", ReturnDialogSamplingGSEA,
                                   ". Select", nrow(ProfData), "as the size of sampling and repeat the request." ))
                        
                    }
                    ##Sampling Profile Data
                    set.seed(1234)
                    SamplingProfData <- 
                        apply(ProfData, 2,function(x) sample(x, ReturnDialogSamplingGSEA)) |>
                        as.data.frame()
        
                    
                    SamplingProfData <<- SamplingProfData 
                    print("Getting  clinical data...")
                    
                    ClinicalData <- getClinicData_MultipleCases(getSummaryGSEAExists=1)
                    ClinicalData <<- ClinicalData
                    # ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="NA")
                    # ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="[Unknown]")
                    # ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="[Not Available]")
                    # ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="")
                    
                    # if(ncol(ClinicalData)>1){
                    #     tkmessageBox(message="Select only ONE clinical Data with only TWO classes")
                    #     stop("Select only ONE clinical Data with only TWO classes")
                    # }
                    
                    print("merging profile data and clinical data tables...")
                    ## MERGE Data.frames: ClinincalData and ProfData: 
                    ## Select only Cases (rownames) that exist in ProfData
                    names_Clinical_Data <- colnames(ClinicalData)
                    names_SamplingProfData <- colnames(SamplingProfData)

                    clinical_profData <- ClinicalData |>
                        left_join(SamplingProfData, by="sampleId")


                    AssayData <- clinical_profData |>
                        select(all_of(names_SamplingProfData)) |>
                        data.frame(row.names = 1) |>
                        t() 
                        #as.matrix()
                    
                    ClinicalData <- ClinicalData |>
                        data.frame(row.names = 1)
                    ClinicalData <<- ClinicalData
                    
                    # merge <- merge(ClinicalData, SamplingProfData, by="row.names") |> 
                    #     data.frame(row.names = 1)
                    # ##Ordering by classes in selected variable in ClinicalData
                    # 
                    # merge <- merge[order(merge[,1]),]
                    # merge <- subset(merge, !merge[,1]=="")
                    # print("Merging Clinical and Profile data...")
                     PhenoData <- ClinicalData[["OS_STATUS"]]
                    # 
                    # 
                    # AssayData<-merge[,-1]
                    # print("AssayData")
                    # AssayData <- round(AssayData, digits=2)
                    # print("round Assaydata")
                    ## GSEA-R does no accept negative value
                    ## Translate matrix with minimum negative value
                    
                    if(min(AssayData, na.rm=TRUE) < 0){
                        print("There are negative values. Translating values by adding the absolute 
                              of minimum value to all matrix")                  
                        AssayData <- AssayData +(abs(min(AssayData, na.rm=TRUE)))
                    }
                    
                    
                    Table_Title <- paste("SD_", ENV$StudyRefCase[k],"_","GenProf_",
                                         ENV$curselectGenProfs[k],"_","CASE_",
                                         ENV$curselectCases[k],".gct")
                    getInTable(AssayData, Table_Title)

                    ###Save AssayData to GCT file                            
                    #AssayData <- t(AssayData)
                    ncol <- ncol(AssayData)
                    nrow <- nrow(AssayData)
                    
                    tmp<- cbind(rownames(AssayData), rep("na", nrow))
                    tmp <- rbind("",tmp)
                    tmp[1,1:2] <- c("NAME", "Description")
                    
                    
                    print("Saving GCT et CLS files ...")
                    
                    rownames(AssayData) <- NULL
                    AssayData <-rbind(colnames(AssayData),AssayData)
                    colnames(AssayData) <- NULL
                    AssayData <- cbind(tmp, AssayData)
                    
                    
                    AssayData <- rbind("","", AssayData)
                    AssayData[1,1] <- "#1.2"
                    AssayData[2,1] <- nrow
                    AssayData[2,2] <- ncol
                    fileName <- paste("SD_",ENV$StudyRefCase[k],"_","GenProf_",
                                    ENV$curselectGenProfs[k],"_","CASE_",ENV$curselectCases[k],
                                    "_",gsub(".txt","",basename(ENV$GeneListfile)) ,".gct", sep="")
                    
                    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                    setwd(paste(getwd(),"/Results/gct_cls", sep=""))
                    
                    
                    write.table(AssayData,file=fileName, col.names=FALSE, 
                                quote=FALSE, row.names=FALSE, sep="\t")
                    
                    ## Built and Save CLS file from PhenoData frame
                    if(sum(grep("[A-Za-z]",PhenoData))==0){
                        
                        
                        fileName<-paste("SD_",ENV$StudyRefCase[k],"_","GenProf_",
                                        ENV$curselectGenProfs[k],"_","CASE_",ENV$curselectCases[k],
                                        "_","OS_STATUS","_",gsub(".txt","",basename(ENV$GeneListfile)) ,".cls", sep="")
                        
                        sink(fileName)
                        cat("#numeric")
                        cat("\n")
                        cat("#","OS_STATUS", sep="")
                        cat("\n")
                        cat(PhenoData)
                        cat("\n")
                        sink()
                        
                        setwd("../../")
                        tkmessageBox(message="cls continuous file was generated but is seems not
                                     working with the present version of GSEA.1.0.R")
                    }else{ 
                        
                        nbrOfSample <-length(PhenoData)
                        nbrOfClasses <-length(table(PhenoData))
                        if(nbrOfClasses!= 2){
                            tkmessageBox(message="Select Variable with only two classes")
                            stop("Select Variable with only two classes")
                            close(progressBar_ProfilesData)
                        }else{
                            
                            classes<- 0
                            for(i in 1:nbrOfClasses){
                                
                                classes <- cbind(classes,names(table(PhenoData))[i])
                            }
                            classes[1] <- "#"
                            classes <- sub(" ", "", classes)

                            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                            
                            fileName<-paste("SD_",ENV$StudyRefCase[k],"_","GenProf_",
                                            ENV$curselectGenProfs[k],"_","CASE_",
                                            ENV$curselectCases[k],"_","OS_STATUS",
                                            "_",gsub(".txt","",basename(ENV$GeneListfile)),".cls", sep="")
                            
                            sink(fileName)
                            cat(paste(nbrOfSample, nbrOfClasses, "1", sep=" "))
                            cat("\n")
                            cat(classes)
                            cat("\n")
                            cat(PhenoData)
                            cat("\n")
                            sink()
                            
                            setwd("../../")
                            
                        }
                    }
                    ENV$PhenoData <- PhenoData
                } else {
                    msgSmallGeneList <- paste("The request has only", length(ENV$GeneList),
                                              " genes. The GSEA should be not robust. 
                                                  It is better to run GSEA with 1000 genes or more.",
                                              sep=" ")
                    tkmessageBox(message=msgSmallGeneList, icon="info")
                }
            }
        } 
        close(progressBar_ProfilesData)
        
    }
}