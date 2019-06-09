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
    ifrm <- function(obj, env = myGlobalEnv()) {
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
        
        Lchecked_Studies <- myGlobalEnv$lchecked_Studies_forCases
        Lchecked_Cases <- length(myGlobalEnv$curselectCases)
        Lchecked_GenProf <- length(myGlobalEnv$curselectGenProfs)
        
        ProfDataAll=0
        ProfData=0
        LengthGenProfs=0
        LengthCases=0
        for (i in 1:Lchecked_Studies){
            
            Si =myGlobalEnv$checked_StudyIndex[i]
            progressBar_ProfilesData <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                                      max = Lchecked_GenProf, width = 400)
            
            
            LastLengthGenProfs <- LengthGenProfs
            LengthGenProfs <- LengthGenProfs + myGlobalEnv$LGenProfs[i]+1
            LastLengthCases <- LengthCases
            LengthCases<- LengthCases + myGlobalEnv$LCases[i]+1
            
            
            
            for (k in 1:Lchecked_Cases){
                
                ## Selecting GeneList from getGeneList.R or getGenesClasses.R functions.
                ifExist <- function(obj, env = myGlobalEnv()) {
                    obj <- deparse(substitute(obj))
                    if(exists(obj, envir = env)) {
                        
                        myGlobalEnv$GeneList <- rownames(myGlobalEnv$GenesDetails[[k]])
                        myGlobalEnv$GeneListfile <- paste(names(myGlobalEnv$GenesDetails)[k]) 
                    }
                }
                
                
                
                Sys.sleep(0.1)
                setTkProgressBar(progressBar_ProfilesData, k, label=paste( round(k/Lchecked_GenProf*100, 0),
                                                                           "% of Expression Set"))
                
                if (myGlobalEnv$curselectGenProfs[k] <= LengthGenProfs && myGlobalEnv$curselectGenProfs[k]>LastLengthGenProfs){    
                    
                    GenProf<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[k]]
                    
                    Case<-myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[k]]
                 
                    
                    if(length(myGlobalEnv$GeneList)>500){
                        ProfData <- getMegaProfData(myGlobalEnv$GeneList,k )
                    } else{
                        ProfData<-getProfileData(myGlobalEnv$cgds,myGlobalEnv$GeneList, GenProf,Case)
                    }
                    
                    if(ncol(ProfData)==0){
                        tkmessageBox(message=paste("There is no mRNA expression data for", myGlobalEnv$GenProfsStudies[k]),icon="info")
                        break(paste("There is no mRNA expression data for",myGlobalEnv$GenProfsStudies[k]))
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
                    
                    
                    if(!exists(deparse(substitute(myGlobalEnv$GenesDetails)),envir = myGlobalEnv)) {
                        
                        ## Display AssyData with Tcl Table
                        title <- paste(myGlobalEnv$StudyRefGenProf[k],":",myGlobalEnv$GenProfChoice[k])
                        ProfData <- t(t(ProfData))
                        getInTable(ProfData, title)
                        
                        
                        
                    }
                    
                    #####getClinicData_MultipleCases function
                    Case<- myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[k]]
                    
                    ClinicalData<-getClinicalData(myGlobalEnv$cgds,Case)
                    
                    
                    if(length(ClinicalData[1,])==0){
                        msgNoClinData=paste("No Clinical Data are Available for\n", myGlobalEnv$CasesStudies[myGlobalEnv$curselectCases[k]+1])
                        tkmessageBox(message=msgNoClinData, title= paste("Study: ",myGlobalEnv$StudyRefCase[k]))
                        close(progressBar_ProfilesData)
                        break
                    } 
                    
                    
                    if(!exists(deparse(substitute(myGlobalEnv$GenesDetails)), envir = myGlobalEnv)) {
                        ## Display AssyData with Tcl Table
                        title<-paste(myGlobalEnv$StudyRefCase[k],myGlobalEnv$CasesStudies[myGlobalEnv$curselectCases[k]+1], sep=": ")
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
                    
                    myGlobalEnv$ClinicalData <- ClinicalData
                    myGlobalEnv$ProfData <- ProfData
                    myGlobalEnv$AssayData <- AssayData
                    
                    #Test if the same length cases for phenoData and AssayData
                    if (all(rownames(myGlobalEnv$ClinicalData)==colnames(myGlobalEnv$AssayData))){
                        
                        
                        
                        ## create labelDescription for columns of phenoData. 
                        ## labeldescription is used by Biobase packages
                        ## In our case labelDescription is Equal to column names
                        metaData <- data.frame(labelDescription= colnames(myGlobalEnv$ClinicalData), row.names=colnames(myGlobalEnv$ClinicalData))        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame   
                        ##that conveniently stores and manipulates 
                        ##the phenotypic data and its metadata in a coordinated fashion. 
                        phenoData<-new("AnnotatedDataFrame", data=myGlobalEnv$ClinicalData, varMetadata=metaData)    
                        
                        print("getting eSet for selected Profileand Clinical Data ...")
                        ##Assembling an ExpressionSet  
                        eSet<-Biobase::ExpressionSet(assayData=myGlobalEnv$AssayData, phenoData=phenoData, annotation="GO")  
                        
                        ##translate Negative to positive value
                        if(min(Biobase::exprs(eSet), na.rm=TRUE)<0){
                            print("There are negative values. Translating values by adding the absolute of minimum value to all matrix")                  
                            Biobase::exprs(eSet) <- Biobase::exprs(eSet) +(abs(min(Biobase::exprs(eSet), na.rm=TRUE)))
                        }
                        myGlobalEnv$eSet <- eSet
                        
                        
                        ## create MSigDb for eSet
                        getMSigDB(myGlobalEnv$eSet,1)
                        
                        #run GSEAlm
                        #nperm determines the number of permutations used to estimate the null distribution
                        #of the enrichment score
                        
                        
                        dialogOptionGSEAlm(k, myGlobalEnv$ClinicalData)
                        print("dialog Option of GSEAlm:  OK")
                        
                        ## test if Covariables works together else get message box
#                         if (inherits(try(pVals <-GSEAlm::gsealmPerm(myGlobalEnv$eSet,myGlobalEnv$coVariables,myGlobalEnv$mSigDB_forGeneList,nperm= myGlobalEnv$permutVal, na.rm=TRUE), silent=TRUE),"try-error"))
#                         {
#                             msgBadCovariables <- paste("There is incompatible variables. Select an other Formula.")
#                             tkmessageBox(message=msgBadCovariables, icon="warning")
#                             close(progressBar_ProfilesData)
#                             stop(msgBadCovariables)
#                         } else{ 
#                             print("Computing of pVals using gsealmPerm function ...")
#                             pVals <-GSEAlm::gsealmPerm(myGlobalEnv$eSet,myGlobalEnv$coVariables,myGlobalEnv$mSigDB_forGeneList,nperm= myGlobalEnv$permutVal, na.rm=TRUE)
#                         }
                        
                        print("Computing of pVals using gsealmPerm function ...")
                        pVals <- gsealmPerm(myGlobalEnv$eSet,myGlobalEnv$coVariables,myGlobalEnv$mSigDB_forGeneList,nperm= myGlobalEnv$permutVal, na.rm=TRUE)
                        

                       print(paste("End computing of pVals of:", myGlobalEnv$StudyRefCase[k]))
                        # we have to correct for multiple testing. In this case we use the method from Benjamini-Hochberg
                        pVals <- apply(pVals, 2, p.adjust, method = "BH", n = nrow(pVals))
                        
                        #pVal_Permut <- paste(names(table(pData(eSetClassifier)))[2])
                        ##class2 is from dialogOptionGSEAlm
                        colnames(pVals) <- c(paste("Down Regulated in",myGlobalEnv$coVariables[-1],myGlobalEnv$class2),paste("Up Regulated in",myGlobalEnv$coVariables[-1],myGlobalEnv$class2,"nperm:",myGlobalEnv$permutVal,"pVal:",myGlobalEnv$seuilpVal))
                        
                        if(exists(deparse(substitute(myGlobalEnv$GenesDetails)), envir = myGlobalEnv)) {
                            print(myGlobalEnv$GenesDetails)
                            assign(paste("pVals", gsub(".RData","",myGlobalEnv$mSigDB_SubName),sep="_"),pVals, envir=myGlobalEnv)
                            workspace <- getwd()
                            setwd("./Results/GSEAlm")
                            
                            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                            if(!file.exists(myGlobalEnv$StudyRefCase[k])){
                                dir.create(file.path(paste(getwd(),"/",myGlobalEnv$StudyRefCase[k] , sep="")), showWarnings = FALSE)
                            }
                            setwd(paste("./",myGlobalEnv$StudyRefCase[k], sep=""))
                            
                            
                            write.table(pVals, paste("pVals_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),".txt", sep=""), sep="\t")
                            #set a significance threshold
                            THRESHOLD<-myGlobalEnv$seuilpVal
                            #gene sets that are downregulated in the BLC
                            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
                            names(downRegulated) <- paste("Down Regulated in",myGlobalEnv$coVariables[-1],myGlobalEnv$class2,"pVal<",THRESHOLD)
                            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
                            
                            assign(paste("downRegulated","GeneClass" ,gsub(".RData","",myGlobalEnv$mSigDB_SubName), sep="_"),downRegulated, envir=myGlobalEnv)
                            #save downRegulted table
                            write.table(downRegulated,"_GeneClass_",paste("downRegulated_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),".txt", sep=""), sep="\t") 
                            
                            #gene sets that are upregulated in basal breast cancer
                            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
                            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
                            names(upRegulated) <- paste("Up Regulated in",myGlobalEnv$coVariables[-1],myGlobalEnv$class2,"pVal<",THRESHOLD)
                            assign(paste("upRegulated","GeneClass" ,gsub(".RData","",myGlobalEnv$mSigDB_SubName),sep="_"),upRegulated, envir=myGlobalEnv)
                            #save upRegulated table
                            write.table(upRegulated,"_GeneClass_" ,paste("upRegulated_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),".txt", sep=""), sep="\t") 
                            
                            setwd(workspace)
                        }else{
                            assign(paste("pVals", gsub(".RData","",myGlobalEnv$mSigDB_SubName),myGlobalEnv$StudyRefCase[k] ,sep="_"),pVals, envir=myGlobalEnv)       
                            workspace <- getwd()
                            setwd("./Results/GSEAlm")
                            ######################
                            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
                            if(!file.exists(myGlobalEnv$StudyRefCase[k])){
                                dir.create(file.path(paste(getwd(),"/",myGlobalEnv$StudyRefCase[k] , sep="")), showWarnings = FALSE)
                            }
                            setwd(paste("./",myGlobalEnv$StudyRefCase[k], sep=""))
                            
                            #################
                            write.table(pVals, paste("pVals_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),"_",myGlobalEnv$StudyRefCase[k],".txt", sep=""), sep="\t")
                            #set a significance threshold
                            THRESHOLD<-myGlobalEnv$seuilpVal
                            #gene sets that are downregulated in the BLC
                            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
                            names(downRegulated) <- paste("Down Regulated in",myGlobalEnv$coVariables[-1],myGlobalEnv$class2,"pVal<",THRESHOLD)
                            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
                            assign(paste("downRegulated", gsub(".RData","",myGlobalEnv$mSigDB_SubName), myGlobalEnv$StudyRefCase[k],sep="_"),downRegulated, envir=myGlobalEnv)
                            #save downRegulted table
                            write.table(downRegulated, paste("downRegulated_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),"_",myGlobalEnv$StudyRefCase[k],".txt", sep=""), sep="\t") 
                            #gene sets that are upregulated in basal breast cancer
                            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
                            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
                            names(upRegulated) <- paste("Up Regulated in",myGlobalEnv$coVariables[-1],myGlobalEnv$class2,"pVal<",THRESHOLD)
                            assign(paste("upRegulated", gsub(".RData","",myGlobalEnv$mSigDB_SubName), myGlobalEnv$StudyRefCase[k],sep="_"),upRegulated, envir=myGlobalEnv)
                            
                            #save upRegulted table
                            write.table(upRegulated, paste("upRegulated_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),"_",myGlobalEnv$StudyRefCase[k],".txt", sep=""), sep="\t") 
                            
                            
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
                        getInTable(UpDownRegulatedTable, title=myGlobalEnv$tudyRefCase[k])
                        
                    }else {tkmessageBox( message= "The expression Gene Set and the Clinical Data do not have the same samples", icon="warning")
                    }
                } 
            } 
            close(progressBar_ProfilesData)
        } 
    #}
    
    ##Remove GenesDetails objectf if exists
    ifrm(GenesDetails, myGlobalEnv)
    
    
}