#' get GSEA linear modeling by studies (diseases)
#' @usage
#' getGSEAlm_Diseases()
#'
#' @return a dataframe with annotation (GO, BP)
#' @export
#' @examples
#'  readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#'  \dontrun{
#'  getGSEAlm_Diseases
#' }
#'@importFrom stats model.matrix
#' 
getGSEAlm_Diseases <-function(){
    
    ##Remove "GenesDetails" objectf if exists
    ifrm <- function(obj, env = ENV()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    ifrm(ENV$GenesDetails, ENV)
    
    
    ####Create a new directory "Results/GSEAlm" for output.
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
    
    #dialog function to select the number of samples
    ENV$ReturnDialogGeneClasses<- dialogGeneClassifier(Lchecked_Cases)
    if(ENV$ReturnDialogGeneClasses[1] == "ID_CANCEL"){
        stop()
    } else{
        
        
        DiseasesType <- 0
        SamplingProfsData<-0 
        ProfData<-0
        LengthGenProfs<-0
        LengthCases=0
        for (i in 1:Lchecked_Studies){
            
            Si =ENV$checked_StudyIndex[i]
            progressBar_ProfilesData <- tkProgressBar(title = ENV$Studies[Si], min = 0,
                                                      max = Lchecked_GenProf, width = 400)
            
            #tkfocus(progressBar_ProfilesData)
            LastLengthGenProfs = LengthGenProfs
            LengthGenProfs = LengthGenProfs + ENV$LGenProfs[i]+1
            LastLengthCases = LengthCases
            LengthCases= LengthCases + ENV$LCases[i]+1
            
            
            
            for (k in 1:Lchecked_Cases){
                
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
                    
                    ProfData <- t(ProfData)
#                     ##Convert data frame to numeric structure
#                     print("converting data frame of Profile data to numeric stucture...")
#                     
#                     cidx <- !(sapply(ProfData, is.numeric))
#                     ProfData[cidx] <- lapply(ProfData[cidx], as.numeric)
#                     
                    #for(p in 1:ncol(ProfData)){
                        
                     #   ProfData[,p] <- as.numeric(ProfData[,p])
                    #}
                    
                    ## for loop is faster than apply fonction
                    #rnames <- rownames(ProfData)
                    #ProfData <- as.data.frame(apply(ProfData,2 ,function(x) as.numeric(x)))
                    #rownames(ProfData) <- rnames
                    
                    if(!exists(deparse(substitute(ENV$GenesDetails)))) {
                        ProfData <- t(t(ProfData))
                        ## Display AssyData with Tcl Table
                        title=paste(ENV$StudyChoice[k],":",ENV$GenProfsStudies[ENV$curselectGenProfs][k])
                        getInTable(ProfData,title)
                        
                        
                    }
                    
                    if(ncol(ProfData)<ENV$ReturnDialogGeneClasses[1]){
                        msgBigSampl <- paste(ENV$StudyRefCase[k], "has only", ncol(ProfData),"samples.","\nSelect at Max: ",ncol(ProfData), "samples")
                        tkmessageBox(message=msgBigSampl, icon="info")
                        break
                        close(progressBar_ProfilesData)
                    }
                    set.seed(1234)
                    
                    
                    SamplingProfData <- t(apply(ProfData, 1,function(x)sample(x,ENV$ReturnDialogGeneClasses[1])))
                    
                    SamplingColnamesProfData <- sample(colnames(ProfData), ENV$ReturnDialogGeneClasses[1])
                    
                    colnames(SamplingProfData) <- SamplingColnamesProfData
                    
                    SamplingProfsData <- cbind(SamplingProfsData,SamplingProfData)
                    print(paste ("sampling data from study:", k))
                    
                    ### ONly for example brca_tcga73genes.RData
                    ##ENV$StudyRefGenProf <- c("brca_tcga", "prad_tcga")

                    ##Extracting Disease Type
                    DiseaseType<- as.matrix(rep(ENV$StudyRefGenProf[k],times=ENV$ReturnDialogGeneClasses[1]))
                    DiseasesType <- c(DiseasesType, DiseaseType)  
                    
                    
                    
                } 
            } 
            close(progressBar_ProfilesData)
        }            
        
        SamplingProfsData<- SamplingProfsData[,-1]
        DiseasesType <-DiseasesType[-1]
        
        DiseasesType <- as.data.frame(DiseasesType)
        rownames(DiseasesType) <- colnames(SamplingProfsData)                
        ENV$DiseasesType <- DiseasesType
        
        ## create labelDescription for columns of phenoData. 
        ## labeldescription is used by Biobase packages
        ## In our case labelDescription is Equal to column names
        
        
        metaData <- data.frame(labelDescription= "DiseasesType", row.names="DiseasesType")        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame   
        
        
        ##that conveniently stores and manipulates 
        ##the phenotypic data and its metadata in a coordinated fashion. 
        
        phenoData<-new("AnnotatedDataFrame", data=DiseasesType, varMetadata=metaData) 
        
        ##Assembling an ExpressionSet  
        eSetClassifier<-Biobase::ExpressionSet(assayData=SamplingProfsData, phenoData=phenoData, annotation="GO") 
        
        ##translate Négative to positive value
        
        if(min(Biobase::exprs(eSetClassifier), na.rm=TRUE)<0){
            print("There are negative values. Translating values by adding the absolute of minimum value to all matrix")                  
            Biobase::exprs(eSetClassifier) <- Biobase::exprs(eSetClassifier) +(abs(min(Biobase::exprs(eSetClassifier), na.rm=TRUE)))
        }
        ENV$eSetClassifier <- eSetClassifier
        
        
        ## create MSigDb for eSetClassifier
        getMSigDB(ENV$eSetClassifier,1)
        
        
        #run GSEAlm
        #nperm determines the number of permutations used to estimate the null distribution
        #of the enrichment score
        
        
        dialogOptionGSEAlm(1, ENV$DiseasesType)
        print("dialog Option of  GSEAlm: OK")
        
        ## test if Covariables works together else get message box
#         if (inherits(try(pVals <-GSEAlm::gsealmPerm(ENV$eSetClassifier,ENV$coVariables,ENV$mSigDB_forGeneList,nperm= ENV$permutVal, na.rm=TRUE), silent=TRUE),"try-error"))
#         {
#             msgBadCovariables <- paste("There is incompatible variables. Select an other Formula.")
#             tkmessageBox(message=msgBadCovariables, icon="warning")
#             close(progressBar_ProfilesData)
#             stop(msgBadCovariables)
#         } else{  
#             print("Computing of pVals using gsealmPerm function ...")
#             pVals <-GSEAlm::gsealmPerm(ENV$eSetClassifier,ENV$coVariables,ENV$mSigDB_forGeneList,nperm= ENV$permutVal, na.rm=TRUE)
#         }
        
         print("Computing of pVals using gsealmPerm function ...")
         pVals <- gsealmPerm(ENV$eSetClassifier,ENV$coVariables,ENV$mSigDB_forGeneList,nperm= ENV$permutVal, na.rm=TRUE)



        #print(paste("pVals:", StudyRefCase[k]))
        # we have to correct for multiple testing. In this case we use the method from Benjamini-Hochberg
        pVals <- apply(pVals, 2, p.adjust, method = "BH", n = nrow(pVals))
        
        pVal_Permut <- paste(names(table(pData(ENV$eSetClassifier)))[2])
        colnames(pVals) <- c(paste("Down Regulated in ",pVal_Permut),paste("Up Regulated in ",pVal_Permut,"nperm:",ENV$permutVal,"pVal:",ENV$seuilpVal))
        
        if(exists(deparse(substitute(GenesDetails)), envir = ENV)) {
            assign(paste("pVals", gsub(".RData","",ENV$mSigDB_SubName),sep="_"),pVals, envir=ENV)
            workspace <- getwd()
            setwd("./Results/GSEAlm")
            ######################
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            if(!file.exists(paste(ENV$StudyRefCase, collapse="_VS_"))){
                dir.create(file.path(paste(getwd(),"/",paste(ENV$StudyRefCase, collapse="_VS_") , sep="")), showWarnings = FALSE)
            }
            setwd(paste("./",paste(ENV$StudyRefCase, collapse="_VS_"), sep=""))
            
            #################
            
            write.table(pVals, paste("pVals_", gsub(".RData","",ENV$mSigDB_SubName),".txt", sep=""), sep="\t")
            #set a significance threshold
            THRESHOLD<-ENV$seuilpVal
            #gene sets that are downregulated in the BLC
            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
            names(downRegulated) <- paste("Down Regulated in ",names(table(pData(ENV$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
            
            assign(paste("downRegulated","GeneClass" ,gsub(".RData","",ENV$mSigDB_SubName), sep="_"),downRegulated, envir=ENV)
            #save downRegulted table
            write.table(downRegulated,"_GeneClass_",paste("downRegulated_", gsub(".RData","",mSigDB_SubName),".txt", sep=""), sep="\t") 
            #gene sets that are upregulated in basal breast cancer
            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
            names(upRegulated) <- paste("Up Regulated in",names(table(pData(ENV$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            assign(paste("upRegulated","GeneClass" ,gsub(".RData","",ENV$mSigDB_SubName),sep="_"),upRegulated, envir=ENV)
            #save upRegulated table
            write.table(upRegulated,"_GeneClass_" ,paste("upRegulated_", gsub(".RData","",mSigDB_SubName),".txt", sep=""), sep="\t") 
            
            setwd(workspace)
        }else{
            assign(paste("pVals", gsub(".RData","",ENV$mSigDB_SubName),ENV$StudyRefCase[k] ,sep="_"),pVals, envir=ENV)       
            workspace <- getwd()
            setwd("./Results/GSEAlm")
            ################
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            if(!file.exists(paste(ENV$StudyRefCase, collapse="_VS_"))){
                dir.create(file.path(paste(getwd(),"/",paste(ENV$StudyRefCase, collapse="_VS_") , sep="")), showWarnings = FALSE)
            }
            setwd(paste("./",paste(ENV$StudyRefCase, collapse="_VS_"), sep=""))
            ################
            write.table(pVals, paste("pVals_", gsub(".RData","",ENV$mSigDB_SubName),"_",ENV$StudyRefCase[k],".txt", sep=""), sep="\t")
            #set a significance threshold
            THRESHOLD<-ENV$seuilpVal
            #gene sets that are downregulated in the BLC
            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
            names(downRegulated) <- paste("Down Regulated in ",names(table(pData(ENV$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
            assign(paste("downRegulated", gsub(".RData","",ENV$mSigDB_SubName), ENV$StudyRefCase[k],sep="_"),downRegulated, envir=ENV)
            #save downRegulted table
            write.table(downRegulated, paste("downRegulated_", gsub(".RData","",ENV$mSigDB_SubName),"_",ENV$StudyRefCase[k],".txt", sep=""), sep="\t") 
            #gene sets that are upregulated in basal breast cancer
            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
            names(upRegulated) <- paste("Up Regulated in",names(table(pData(ENV$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            assign(paste("upRegulated", gsub(".RData","",ENV$mSigDB_SubName), ENV$StudyRefCase[k],sep="_"),upRegulated, envir=ENV)
            
            #save upRegulted table
            write.table(upRegulated, paste("upRegulated_", gsub(".RData","",ENV$mSigDB_SubName),"_",ENV$tudyRefCase[k],".txt", sep=""), sep="\t") 
            
            
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
        getInTable(UpDownRegulatedTable, title=paste(ENV$StudyRefCase[1]," / ",ENV$StudyRefCase[2]))
    }
}