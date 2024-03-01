#' get GSEA linear modeling by variables (phenotype)
#' @usage
#' getGSEAlm_Variables()
#'
#' @return a dataframe with annotation (GO, BP)
#' @export
#'
#' @examples
#'  x <- 3
#' \dontrun{
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' getGSEAlm_Variables()
#' }
getGSEAlm_Variables <-function(){
    
    ##Remove "GenesDetails" objectf if exists
    ifrm <- function(obj, env = ENV()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    
    ####Create a new directory "Results/GSEAlm" for output
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(!file.exists("Results/")){
        dir.create(file.path(paste(getwd(), "/Results", sep="")), showWarnings = FALSE)
        dir.create(file.path(paste(getwd(), "/Results/GSEAlm", sep="")), showWarnings = FALSE)
    } else if (!file.exists("Results/GSEAlm/")){
        dir.create(file.path(paste(getwd(), "/Results/GSEAlm", sep="")), showWarnings = FALSE)
    }
    
    
    
        testCheckedCaseGenProf()
        
        Lchecked_Studies <- ENV$lchecked_Studies_forCases
        Lchecked_Cases <- length(ENV$curselectCases)
        Lchecked_GenProf <- length(ENV$curselectGenProfs)
        
        ProfDataAll=0
        ProfData=0
        LengthGenProfs=0
        LengthCases=0
        for (i in 1:Lchecked_Studies){
            
            Si =ENV$checked_StudyIndex[i]
            progressBar_ProfilesData <- tkProgressBar(title = ENV$Studies[Si], min = 0,
                                                      max = Lchecked_GenProf, width = 400)
            
            
            LastLengthGenProfs <- LengthGenProfs
            LengthGenProfs <- LengthGenProfs + ENV$LGenProfs[i]+1
            LastLengthCases <- LengthCases
            LengthCases<- LengthCases + ENV$LCases[i]+1
            
            
            
            for (k in 1:Lchecked_Cases){
                
                ## Selecting GeneList from getGeneList.R or getGenesClasses.R functions.
                ifExist <- function(obj, env = ENV()) {
                    obj <- deparse(substitute(obj))
                    if(exists(obj, envir = env)) {
                        
                        ENV$GeneList <- rownames(ENV$GenesDetails[[k]])
                        ENV$GeneListfile <- paste(names(ENV$GenesDetails)[k]) 
                    }
                }
                
                
                
                Sys.sleep(0.1)
                setTkProgressBar(progressBar_ProfilesData, k, label=paste( round(k/Lchecked_GenProf*100, 0),
                                                                           "% of Expression Set"))
                
                if (ENV$curselectGenProfs[k] <= LengthGenProfs && ENV$curselectGenProfs[k]>LastLengthGenProfs){    
                    
                    GenProf<-ENV$GenProfsRefStudies[ENV$curselectGenProfs[k]]
                    
                    Case<-ENV$CasesRefStudies[ENV$curselectCases[k]]
                 
                    
                    if(length(ENV$GeneList)>500){
                        ProfData <- getMegaProfData(ENV$GeneList,k )
                    } else{
                        ProfData<-getProfileData(ENV$cgds,ENV$GeneList, GenProf,Case)
                    }
                    
                    if(ncol(ProfData)==0){
                        tkmessageBox(message=paste("There is no mRNA expression data for", ENV$GenProfsStudies[k]),icon="info")
                        break(paste("There is no mRNA expression data for",ENV$GenProfsStudies[k]))
                        close(progressBar_ProfilesData)
                        
                    }
                    
                    ##Convert data frame to numeric structure
#                     print("converting data frame of Profile data to numeric stucture...")
#                     
#                     cidx <- !(sapply(ProfData, is.numeric))
#                     ProfData[cidx] <- lapply(ProfData[cidx], as.numeric)
                    
                    #for(p in 1:ncol(ProfData)){
                        
                     #   ProfData[,i] <- as.numeric(ProfData[,p])
                    #}
                    
                    
                    if(!exists(deparse(substitute(ENV$GenesDetails)),envir = ENV)) {
                        
                        ## Display AssyData with Tcl Table
                        title <- paste(ENV$StudyRefGenProf[k],":",ENV$GenProfChoice[k])
                        ProfData <- t(t(ProfData))
                        getInTable(ProfData, title)
                        
                        
                        
                    }
                    
                    #####getClinicData_MultipleCases function
                    Case<- ENV$CasesRefStudies[ENV$curselectCases[k]]
                    
                    ClinicalData<-getClinicalData(ENV$cgds,Case)
                    
                    
                    if(length(ClinicalData[1,])==0){
                        msgNoClinData=paste("No Clinical Data are Available for\n", ENV$CasesStudies[ENV$curselectCases[k]+1])
                        tkmessageBox(message=msgNoClinData, title= paste("Study: ",ENV$StudyRefCase[k]))
                        close(progressBar_ProfilesData)
                        break
                    } 
                    
                    
                    if(!exists(deparse(substitute(ENV$GenesDetails)), envir = ENV)) {
                        ## Display AssyData with Tcl Table
                        title<-paste(ENV$StudyRefCase[k],ENV$CasesStudies[ENV$curselectCases[k]+1], sep=": ")
                        getInTable(ClinicalData,title)
                        
                    } 
                    
                    ## Select only Cases (rownames) that exist in ClinicalDataSub and ProfData
                    merge <- merge(ClinicalData, ProfData, by="row.names")
                    print("merging Clincal and Profile Data")
                    ClinicalData<- merge[,1:(length(ClinicalData)+1)]
                    
                    
                    rownames(ClinicalData)<- ClinicalData[,1]
                    ClinicalData <- ClinicalData[-1]
                    ProfData<-merge[,!(merge %in% ClinicalData)]
                    
                    #ProfData <- ProfData[-1]
                    AssayData<- t(ProfData)
                    colnames(AssayData) <- AssayData[1,]
                    AssayData <- AssayData[-1,]
                    rnames <- rownames(AssayData) 
                    AssayData <- as.matrix(apply(AssayData,2 ,function(x) as.numeric(x)))
                    rownames(AssayData) <- rnames
                    
                    ENV$ClinicalData <- ClinicalData
                    ENV$ProfData <- ProfData
                    ENV$AssayData <- AssayData
                    
                    #Test if the same length cases for phenoData and AssayData
                    if (all(rownames(ENV$ClinicalData)==colnames(ENV$AssayData))){
                        
                        
                        
                        ## create labelDescription for columns of phenoData. 
                        ## labeldescription is used by Biobase packages
                        ## In our case labelDescription is Equal to column names
                        metaData <- data.frame(labelDescription= colnames(ENV$ClinicalData), row.names=colnames(ENV$ClinicalData))        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame   
                        ##that conveniently stores and manipulates 
                        ##the phenotypic data and its metadata in a coordinated fashion. 
                        phenoData<-new("AnnotatedDataFrame", data=ENV$ClinicalData, varMetadata=metaData)    
                        
                        print("getting eSet for selected Profileand Clinical Data ...")
                        ##Assembling an ExpressionSet  
                        eSet<-Biobase::ExpressionSet(assayData=ENV$AssayData, phenoData=phenoData, annotation="GO")  
                        
                        ##translate Negative to positive value
                        if(min(Biobase::exprs(eSet), na.rm=TRUE)<0){
                            print("There are negative values. Translating values by adding the absolute of minimum value to all matrix")                  
                            Biobase::exprs(eSet) <- Biobase::exprs(eSet) +(abs(min(Biobase::exprs(eSet), na.rm=TRUE)))
                        }
                        ENV$eSet <- eSet
                        
                        
                        ## create MSigDb for eSet
                        getMSigDB(ENV$eSet,1)
                        
                        #run GSEAlm
                        #nperm determines the number of permutations used to estimate the null distribution
                        #of the enrichment score
                        
                        
                        dialogOptionGSEAlm(k, ENV$ClinicalData)
                        print("dialog Option of GSEAlm:  OK")
                        
                        ## test if Covariables works together else get message box
#                         if (inherits(try(pVals <-GSEAlm::gsealmPerm(ENV$eSet,ENV$coVariables,ENV$mSigDB_forGeneList,nperm= ENV$permutVal, na.rm=TRUE), silent=TRUE),"try-error"))
#                         {
#                             msgBadCovariables <- paste("There is incompatible variables. Select an other Formula.")
#                             tkmessageBox(message=msgBadCovariables, icon="warning")
#                             close(progressBar_ProfilesData)
#                             stop(msgBadCovariables)
#                         } else{ 
#                             print("Computing of pVals using gsealmPerm function ...")
#                             pVals <-GSEAlm::gsealmPerm(ENV$eSet,ENV$coVariables,ENV$mSigDB_forGeneList,nperm= ENV$permutVal, na.rm=TRUE)
#                         }
                        
                        print("Computing of pVals using gsealmPerm function ...")
                        pVals <- gsealmPerm(ENV$eSet,ENV$coVariables,ENV$mSigDB_forGeneList,nperm= ENV$permutVal, na.rm=TRUE)
                        

                       print(paste("End computing of pVals of:", ENV$StudyRefCase[k]))
                        # we have to correct for multiple testing. In this case we use the method from Benjamini-Hochberg
                        pVals <- apply(pVals, 2, p.adjust, method = "BH", n = nrow(pVals))
                        
                        #pVal_Permut <- paste(names(table(pData(eSetClassifier)))[2])
                        ##class2 is from dialogOptionGSEAlm
                        colnames(pVals) <- c(paste("Down Regulated in",ENV$coVariables[-1],ENV$class2),paste("Up Regulated in",ENV$coVariables[-1],ENV$class2,"nperm:",ENV$permutVal,"pVal:",ENV$seuilpVal))
                        
                        if(exists(deparse(substitute(ENV$GenesDetails)), envir = ENV)) {
                            print(ENV$GenesDetails)
                            assign(paste("pVals", gsub(".RData","",ENV$mSigDB_SubName),sep="_"),pVals, envir=ENV)
                            workspace <- getwd()
                            setwd("./Results/GSEAlm")
                            
                            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                            if(!file.exists(ENV$StudyRefCase[k])){
                                dir.create(file.path(paste(getwd(),"/",ENV$StudyRefCase[k] , sep="")), showWarnings = FALSE)
                            }
                            setwd(paste("./",ENV$StudyRefCase[k], sep=""))
                            
                            
                            write.table(pVals, paste("pVals_", gsub(".RData","",ENV$mSigDB_SubName),".txt", sep=""), sep="\t")
                            #set a significance threshold
                            THRESHOLD<-ENV$seuilpVal
                            #gene sets that are downregulated in the BLC
                            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
                            names(downRegulated) <- paste("Down Regulated in",ENV$coVariables[-1],ENV$class2,"pVal<",THRESHOLD)
                            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
                            
                            assign(paste("downRegulated","GeneClass" ,gsub(".RData","",ENV$mSigDB_SubName), sep="_"),downRegulated, envir=ENV)
                            #save downRegulted table
                            write.table(downRegulated,"_GeneClass_",paste("downRegulated_", gsub(".RData","",ENV$mSigDB_SubName),".txt", sep=""), sep="\t") 
                            
                            #gene sets that are upregulated in basal breast cancer
                            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
                            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
                            names(upRegulated) <- paste("Up Regulated in",ENV$coVariables[-1],ENV$class2,"pVal<",THRESHOLD)
                            assign(paste("upRegulated","GeneClass" ,gsub(".RData","",ENV$mSigDB_SubName),sep="_"),upRegulated, envir=ENV)
                            #save upRegulated table
                            write.table(upRegulated,"_GeneClass_" ,paste("upRegulated_", gsub(".RData","",ENV$mSigDB_SubName),".txt", sep=""), sep="\t") 
                            
                            setwd(workspace)
                        }else{
                            assign(paste("pVals", gsub(".RData","",ENV$mSigDB_SubName),ENV$StudyRefCase[k] ,sep="_"),pVals, envir=ENV)       
                            workspace <- getwd()
                            setwd("./Results/GSEAlm")
                            ######################
                            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                            if(!file.exists(ENV$StudyRefCase[k])){
                                dir.create(file.path(paste(getwd(),"/",ENV$StudyRefCase[k] , sep="")), showWarnings = FALSE)
                            }
                            setwd(paste("./",ENV$StudyRefCase[k], sep=""))
                            
                            #################
                            write.table(pVals, paste("pVals_", gsub(".RData","",ENV$mSigDB_SubName),"_",ENV$StudyRefCase[k],".txt", sep=""), sep="\t")
                            #set a significance threshold
                            THRESHOLD<-ENV$seuilpVal
                            #gene sets that are downregulated in the BLC
                            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
                            names(downRegulated) <- paste("Down Regulated in",ENV$coVariables[-1],ENV$class2,"pVal<",THRESHOLD)
                            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
                            assign(paste("downRegulated", gsub(".RData","",ENV$mSigDB_SubName), ENV$StudyRefCase[k],sep="_"),downRegulated, envir=ENV)
                            #save downRegulted table
                            write.table(downRegulated, paste("downRegulated_", gsub(".RData","",ENV$mSigDB_SubName),"_",ENV$StudyRefCase[k],".txt", sep=""), sep="\t") 
                            #gene sets that are upregulated in basal breast cancer
                            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
                            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
                            names(upRegulated) <- paste("Up Regulated in",ENV$coVariables[-1],ENV$class2,"pVal<",THRESHOLD)
                            assign(paste("upRegulated", gsub(".RData","",ENV$mSigDB_SubName), ENV$StudyRefCase[k],sep="_"),upRegulated, envir=ENV)
                            
                            #save upRegulted table
                            write.table(upRegulated, paste("upRegulated_", gsub(".RData","",ENV$mSigDB_SubName),"_",ENV$StudyRefCase[k],".txt", sep=""), sep="\t") 
                            
                            
                            setwd(workspace)
                            
                        }
                        ##Get results in Tcltk Table
                        
                        downRegulatedTab <- rbind(colnames(downRegulated), downRegulated)
                        downRegulatedTab <- cbind(rownames(downRegulatedTab),downRegulatedTab)
                        downRegulatedTab[,1]<- as.character(downRegulatedTab[,1])
                        downRegulatedTab[1,1] <- "***** Down Regulated Gene Sets *****" 
                        colnames(downRegulatedTab) <- NULL
                        
                        upRegulatedTab <- rbind(colnames(upRegulated), upRegulated)
                        upRegulatedTab <- cbind(rownames(upRegulatedTab),upRegulatedTab)
                        upRegulatedTab[,1]<- as.character(upRegulatedTab[,1])
                        upRegulatedTab[1,1] <- "***** Up Regulated Gene Sets *****"
                        colnames(upRegulatedTab) <- NULL
                        
                        UpDownRegulatedTable <- rbind.na(downRegulatedTab,upRegulatedTab)
                        getInTable(UpDownRegulatedTable, title=ENV$tudyRefCase[k])
                        
                    }else {tkmessageBox( message= "The expression Gene Set and the Clinical Data do not have the same samples", icon="warning")
                    }
                } 
            } 
            close(progressBar_ProfilesData)
        } 
    #}
    
    ##Remove GenesDetails objectf if exists
    ifrm(GenesDetails, ENV)
    
    
}