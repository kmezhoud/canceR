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
    
    
    Lchecked_Studies <- myGlobalEnv$lchecked_Studies_forCases
    Lchecked_Cases <- length(myGlobalEnv$curselectCases)
    Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
    
    
    
    LengthGenProfs<-0
    LengthCases<-0
    for (i in 1:Lchecked_Studies){
        Si =myGlobalEnv$checked_StudyIndex[i]
        progressBar_ProfilesData <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                                  max = Lchecked_GenProf, width = 400)
        
        
        LastLengthGenProfs = LengthGenProfs
        LengthGenProfs = LengthGenProfs + myGlobalEnv$LGenProfs[i]+1
        LastLengthCases = LengthCases
        LengthCases= LengthCases + myGlobalEnv$LCases[i]+1
        
        
        
        ####Tree ROOT==Studies
        StudyiChoice <- paste("Study",i,"Choice", sep="")
        
        for (k in 1:Lchecked_GenProf){
            Sys.sleep(0.1)
            setTkProgressBar(progressBar_ProfilesData, k, label=paste( round(k/Lchecked_GenProf*100, 0),
                                                                       "% of Profiles Data"))
            
            if (myGlobalEnv$curselectGenProfs[k] <= LengthGenProfs && myGlobalEnv$curselectGenProfs[k]>LastLengthGenProfs){    
                
                GenProf<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[k]]
                
                Case<-myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[k]]
                
                
                ReturnDialogSamplingGSEA<- dialogSamplingGSEA(Lchecked_GenProf)
                
                if(ReturnDialogSamplingGSEA[1] == "ID_CANCEL"){
                    stop()
                }else{ 
                    
                    if(length(myGlobalEnv$GeneList)>500){
                        ProfData <- getMegaProfData(myGlobalEnv$GeneList,k )
                    } else{
                        msgSmallGeneList <- paste("The request has only", length(myGlobalEnv$GeneList)," genes. The GSEA should be not robust. It is better to run GSEA with 1000 genes or more.", sep=" ")
                        tkmessageBox(message=msgSmallGeneList, icon="info")
                        ProfData<-getProfileData(myGlobalEnv$cgds,myGlobalEnv$GeneList, GenProf,Case)
                    }
                    
                    ##Convert data frame to numeric structure
#                     print("converting data frame of Profile data to numeric stucture...")
#                     for(i in 1:ncol(ProfData)){
#                         
#                         ProfData[,i] <- as.numeric(ProfData[,i])
#                     }
                    
                    ## substitute "NaN" by "NA"
                    if(length(grep("NaN",ProfData))!=0){
                        
                        print("This step takes a while to remove NaN string and convert them to NA (12000 genes takes about 10 min)")
                        for (j in 1:ncol(ProfData)){
                            #ProfData[,i]<- as.numeric(gsub("\\[Not Available\\]","na", ProfData[,i]))
                            #ProfData <- sapply(ProfData, function(x) gsub("NA", "na", x))
                            ProfData[,j]<- as.numeric(gsub("NaN",NA, ProfData[,j]))
                            #ProfData[,i]<- as.numeric(gsub(NA,"na", ProfData[,i]))
                        }
                    }
                    print("Removing genes without data... ")
                    ##remove all NAs rows
                    ProfData<- ProfData[which( apply( !( apply(ProfData,1,is.na) ),2,sum)!=0 ),]
                    
                    ## When matrix has negative values,  simple space are generated before every positive value. 
                    ## This space cause deleting all columns from matrix when we try to remove the columns without values NAs
                    if(min(ProfData, na.rm = TRUE) < 0){
                        for (j in 1:ncol(ProfData)){
                            
                            ProfData[,j]<- as.numeric(gsub(" ","", ProfData[,j]))
                            
                        }
                    }
                    
                    ##remove all NAs columns only for CNA profile Data
                    #if(length(grep("cna",myGlobalEnv$CaseChoice[k], ignore.case = TRUE))!=0){
                    #ProfData<- ProfData[,-which( apply( !( apply(ProfData,1,is.na) ),1,sum)==0 )]
                    #}
                    
                    if(nrow(ProfData)<ReturnDialogSamplingGSEA){
                        tkmessageBox(message= paste("After cleaning data frame, it remains: ", nrow(ProfData), "samples < ", ReturnDialogSamplingGSEA,". Select",nrow(ProfData), "as the size of sampling and repeat the request." ), icon="info")
                        close(progressBar_ProfilesData)
                        stop(paste("After cleaning data frame, it remains: ", nrow(ProfData), "samples < ", ReturnDialogSamplingGSEA,". Select", nrow(ProfData), "as the size of sampling and repeat the request." ))
                        
                    }
                    ##Sampling Profile Data
                    set.seed(1234)
                    SamplingProfData <- apply(ProfData, 2,function(x)sample(x,ReturnDialogSamplingGSEA))
                    
                    SamplingRownamesProfData <- sample(rownames(ProfData), ReturnDialogSamplingGSEA)
                    
                    rownames(SamplingProfData) <- SamplingRownamesProfData
                    
                    
                    
                    print("Getting  clinical data...")
                    
                    ClinicalData <- as.data.frame(getClinicData_MultipleCases(getSummaryGSEAExists=1))
                    ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="NA")
                    ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="[Unknown]")
                    ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="[Not Available]")
                    ClinicalData <- subset(ClinicalData, !ClinicalData[,1]=="")
                    
                    if(ncol(ClinicalData)>1){
                        tkmessageBox(message="Select only ONE clinical Data with only TWO classes")
                        stop("Select only ONE clinical Data with only TWO classes")
                    }
                    
                    print("merging profile data and clinical data tables...")
                    ## MERGE Data.frames: ClinincalData and ProfData: Select only Cases (rownames) that exist in ProfData
                    merge <- merge(ClinicalData, SamplingProfData, by="row.names")
                    ##Ordering by classes in selected variable in ClinicalData
                    
                    row.names(merge) <- merge[,1]
                    merge <- merge[,-1]
                    merge <- merge[order(merge[,1]),]
                    merge <- subset(merge, !merge[,1]=="")
                    print("Merging Clinical and Profile data...")
                    PhenoData<- merge[,1]
                    
                    
                    AssayData<-merge[,-1]
                    print("AssayData")
                    AssayData <- round(AssayData, digits=2)
                    print("round Assaydata")
                    ## GSEA-R does no accept negative value
                    ## Translate matrix with minimum negative value
                    
                    if(min(AssayData, na.rm=TRUE)<0){
                        print("There are negative values. Translating values by adding the absolute of minimum value to all matrix")                  
                        AssayData <- AssayData +(abs(min(AssayData, na.rm=TRUE)))
                    }
                    
                    
                    Table_Title<- paste("SD_",myGlobalEnv$StudyRefCase[k],"_","GenProf_",myGlobalEnv$curselectGenProfs_forStudy[k],"_","CASE_",myGlobalEnv$curselectCases_forStudy[k],".gct")
                    getInTable(AssayData, Table_Title)
                    
                    
                    
                    ###Save AssayData to GCT file                            
                    AssayData <- t(AssayData)
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
                    
                    
                    AssayData <- rbind("","",AssayData)
                    AssayData[1,1] <- "#1.2"
                    AssayData[2,1] <- nrow
                    AssayData[2,2] <- ncol
                    fileName<-paste("SD_",myGlobalEnv$StudyRefCase[k],"_","GenProf_",myGlobalEnv$curselectGenProfs_forStudy[k],"_","CASE_",myGlobalEnv$curselectCases_forStudy[k],"_",gsub(".txt","",basename(myGlobalEnv$GeneListfile)) ,".gct", sep="")
                    
                    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                    setwd(paste(getwd(),"/Results/gct_cls", sep=""))
                    
                    
                    write.table(AssayData,file=fileName, col.names=FALSE, quote=FALSE, row.names=FALSE, sep="\t")
                    
                    ## Built and Save CLS file from PhenoData frame
                    if(sum(grep("[A-Za-z]",PhenoData))==0){
                        
                        
                        fileName<-paste("SD_",myGlobalEnv$StudyRefCase[k],"_","GenProf_",myGlobalEnv$curselectGenProfs_forStudy[k],"_","CASE_",myGlobalEnv$curselectCases_forStudy[k],"_",colnames(merge[1]),"_",gsub(".txt","",basename(myGlobalEnv$GeneListfile)) ,".cls", sep="")
                        
                        sink(fileName)
                        cat("#numeric")
                        cat("\n")
                        cat("#",colnames(merge[1]), sep="")
                        cat("\n")
                        cat(PhenoData)
                        cat("\n")
                        sink()
                        
                        setwd("../../")
                        tkmessageBox(message="cls continuous file was generated but is seems not working with the present version of GSEA.1.0.R")
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
                            
                            fileName<-paste("SD_",myGlobalEnv$StudyRefCase[k],"_","GenProf_",myGlobalEnv$curselectGenProfs_forStudy[k],"_","CASE_",myGlobalEnv$curselectCases_forStudy[k],"_",colnames(merge[1]),"_",gsub(".txt","",basename(myGlobalEnv$GeneListfile)),".cls", sep="")
                            
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
                } 
            }
        } 
        close(progressBar_ProfilesData)
        
    }
    
    
    
    
    myGlobalEnv$PhenoData <- PhenoData
}