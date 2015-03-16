getGSEAlm_Diseases <-function(){
    
    ##Remove "GenesDetails" objectf if exists
    ifrm <- function(obj, env = myGlobalEnv()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    ifrm(myGlobalEnv$GenesDetails, myGlobalEnv)
    
    
    ####Create a new directory "Results/GSEAlm" for output.
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
    
    #dialog function to select the number of samples
    myGlobalEnv$ReturnDialogGeneClasses<- dialogGeneClassifier(Lchecked_Cases)
    if(myGlobalEnv$ReturnDialogGeneClasses[1] == "ID_CANCEL"){
        stop()
    } else{
        
        
        DiseasesType <- 0
        SamplingProfsData<-0 
        ProfData<-0
        LengthGenProfs<-0
        LengthCases=0
        for (i in 1:Lchecked_Studies){
            
            Si =myGlobalEnv$checked_StudyIndex[i]
            progressBar_ProfilesData <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                                      max = Lchecked_GenProf, width = 400)
            
            #tkfocus(progressBar_ProfilesData)
            LastLengthGenProfs = LengthGenProfs
            LengthGenProfs = LengthGenProfs + myGlobalEnv$LGenProfs[i]+1
            LastLengthCases = LengthCases
            LengthCases= LengthCases + myGlobalEnv$LCases[i]+1
            
            
            
            for (k in 1:Lchecked_Cases){
                
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
                    
                    if(!exists(deparse(substitute(myGlobalEnv$GenesDetails)))) {
                        ProfData <- t(t(ProfData))
                        ## Display AssyData with Tcl Table
                        title=paste(myGlobalEnv$StudyChoice[k],":",myGlobalEnv$GenProfsStudies[myGlobalEnv$curselectGenProfs][k])
                        getInTable(ProfData,title)
                        
                        
                    }
                    
                    if(ncol(ProfData)<myGlobalEnv$ReturnDialogGeneClasses[1]){
                        msgBigSampl <- paste(myGlobalEnv$StudyRefCase[k], "has only", ncol(ProfData),"samples.","\nSelect at Max: ",ncol(ProfData), "samples")
                        tkmessageBox(message=msgBigSampl, icon="info")
                        break
                        close(progressBar_ProfilesData)
                    }
                    set.seed(1234)
                    
                    
                    SamplingProfData <- t(apply(ProfData, 1,function(x)sample(x,myGlobalEnv$ReturnDialogGeneClasses[1])))
                    
                    SamplingColnamesProfData <- sample(colnames(ProfData), myGlobalEnv$ReturnDialogGeneClasses[1])
                    
                    colnames(SamplingProfData) <- SamplingColnamesProfData
                    
                    SamplingProfsData <- cbind(SamplingProfsData,SamplingProfData)
                    print(paste ("sampling data from study:", k))
                    
                    ### ONly for example brca_tcga73genes.RData
                    ##myGlobalEnv$StudyRefGenProf <- c("brca_tcga", "prad_tcga")

                    ##Extracting Disease Type
                    DiseaseType<- as.matrix(rep(myGlobalEnv$StudyRefGenProf[k],times=myGlobalEnv$ReturnDialogGeneClasses[1]))
                    DiseasesType <- c(DiseasesType, DiseaseType)  
                    
                    
                    
                } 
            } 
            close(progressBar_ProfilesData)
        }            
        
        SamplingProfsData<- SamplingProfsData[,-1]
        DiseasesType <-DiseasesType[-1]
        
        DiseasesType <- as.data.frame(DiseasesType)
        rownames(DiseasesType) <- colnames(SamplingProfsData)                
        myGlobalEnv$DiseasesType <- DiseasesType
        
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
        myGlobalEnv$eSetClassifier <- eSetClassifier
        
        
        ## create MSigDb for eSetClassifier
        getMSigDB(myGlobalEnv$eSetClassifier,1)
        
        
        #run GSEAlm
        #nperm determines the number of permutations used to estimate the null distribution
        #of the enrichment score
        
        
        dialogOptionGSEAlm(1, myGlobalEnv$DiseasesType)
        print("dialog Option of  GSEAlm: OK")
        
        ## test if Covariables works together else get message box
#         if (inherits(try(pVals <-GSEAlm::gsealmPerm(myGlobalEnv$eSetClassifier,myGlobalEnv$coVariables,myGlobalEnv$mSigDB_forGeneList,nperm= myGlobalEnv$permutVal, na.rm=TRUE), silent=TRUE),"try-error"))
#         {
#             msgBadCovariables <- paste("There is incompatible variables. Select an other Formula.")
#             tkmessageBox(message=msgBadCovariables, icon="warning")
#             close(progressBar_ProfilesData)
#             stop(msgBadCovariables)
#         } else{  
#             print("Computing of pVals using gsealmPerm function ...")
#             pVals <-GSEAlm::gsealmPerm(myGlobalEnv$eSetClassifier,myGlobalEnv$coVariables,myGlobalEnv$mSigDB_forGeneList,nperm= myGlobalEnv$permutVal, na.rm=TRUE)
#         }
        
         print("Computing of pVals using gsealmPerm function ...")
         pVals <-GSEAlm::gsealmPerm(myGlobalEnv$eSetClassifier,myGlobalEnv$coVariables,myGlobalEnv$mSigDB_forGeneList,nperm= myGlobalEnv$permutVal, na.rm=TRUE)



        #print(paste("pVals:", StudyRefCase[k]))
        # we have to correct for multiple testing. In this case we use the method from Benjamini-Hochberg
        pVals <- apply(pVals, 2, p.adjust, method = "BH", n = nrow(pVals))
        
        pVal_Permut <- paste(names(table(pData(myGlobalEnv$eSetClassifier)))[2])
        colnames(pVals) <- c(paste("Down Regulated in ",pVal_Permut),paste("Up Regulated in ",pVal_Permut,"nperm:",myGlobalEnv$permutVal,"pVal:",myGlobalEnv$seuilpVal))
        
        if(exists(deparse(substitute(GenesDetails)), envir = myGlobalEnv)) {
            assign(paste("pVals", gsub(".RData","",myGlobalEnv$mSigDB_SubName),sep="_"),pVals, envir=myGlobalEnv)
            workspace <- getwd()
            setwd("./Results/GSEAlm")
            ######################
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            if(!file.exists(paste(myGlobalEnv$StudyRefCase, collapse="_VS_"))){
                dir.create(file.path(paste(getwd(),"/",paste(myGlobalEnv$StudyRefCase, collapse="_VS_") , sep="")), showWarnings = FALSE)
            }
            setwd(paste("./",paste(myGlobalEnv$StudyRefCase, collapse="_VS_"), sep=""))
            
            #################
            
            write.table(pVals, paste("pVals_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),".txt", sep=""), sep="\t")
            #set a significance threshold
            THRESHOLD<-myGlobalEnv$seuilpVal
            #gene sets that are downregulated in the BLC
            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
            names(downRegulated) <- paste("Down Regulated in ",names(table(pData(myGlobalEnv$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
            
            assign(paste("downRegulated","GeneClass" ,gsub(".RData","",myGlobalEnv$mSigDB_SubName), sep="_"),downRegulated, envir=myGlobalEnv)
            #save downRegulted table
            write.table(downRegulated,"_GeneClass_",paste("downRegulated_", gsub(".RData","",mSigDB_SubName),".txt", sep=""), sep="\t") 
            #gene sets that are upregulated in basal breast cancer
            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
            names(upRegulated) <- paste("Up Regulated in",names(table(pData(myGlobalEnv$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            assign(paste("upRegulated","GeneClass" ,gsub(".RData","",myGlobalEnv$mSigDB_SubName),sep="_"),upRegulated, envir=myGlobalEnv)
            #save upRegulated table
            write.table(upRegulated,"_GeneClass_" ,paste("upRegulated_", gsub(".RData","",mSigDB_SubName),".txt", sep=""), sep="\t") 
            
            setwd(workspace)
        }else{
            assign(paste("pVals", gsub(".RData","",myGlobalEnv$mSigDB_SubName),myGlobalEnv$StudyRefCase[k] ,sep="_"),pVals, envir=myGlobalEnv)       
            workspace <- getwd()
            setwd("./Results/GSEAlm")
            ################
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            if(!file.exists(paste(myGlobalEnv$StudyRefCase, collapse="_VS_"))){
                dir.create(file.path(paste(getwd(),"/",paste(myGlobalEnv$StudyRefCase, collapse="_VS_") , sep="")), showWarnings = FALSE)
            }
            setwd(paste("./",paste(myGlobalEnv$StudyRefCase, collapse="_VS_"), sep=""))
            ################
            write.table(pVals, paste("pVals_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),"_",myGlobalEnv$StudyRefCase[k],".txt", sep=""), sep="\t")
            #set a significance threshold
            THRESHOLD<-myGlobalEnv$seuilpVal
            #gene sets that are downregulated in the BLC
            downRegulated <- data.frame(sort(pVals[pVals[, 1]< THRESHOLD,1]))
            names(downRegulated) <- paste("Down Regulated in ",names(table(pData(myGlobalEnv$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            #colnames(pVals) <- c(paste("Down Regulated\n",pVal_Permut),paste("Up Regulated\n",pVal_Permut))
            assign(paste("downRegulated", gsub(".RData","",myGlobalEnv$mSigDB_SubName), myGlobalEnv$StudyRefCase[k],sep="_"),downRegulated, envir=myGlobalEnv)
            #save downRegulted table
            write.table(downRegulated, paste("downRegulated_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),"_",myGlobalEnv$StudyRefCase[k],".txt", sep=""), sep="\t") 
            #gene sets that are upregulated in basal breast cancer
            upRegulated <- data.frame(sort(pVals[pVals[, 2] < THRESHOLD, 2]))
            #colnames(upRegulated[1]) <- paste("Up Regulated\n", seuilpVal, pVal_permut, coVariables)
            names(upRegulated) <- paste("Up Regulated in",names(table(pData(myGlobalEnv$eSetClassifier)))[2],"\tpVal<",THRESHOLD)
            assign(paste("upRegulated", gsub(".RData","",myGlobalEnv$mSigDB_SubName), myGlobalEnv$StudyRefCase[k],sep="_"),upRegulated, envir=myGlobalEnv)
            
            #save upRegulted table
            write.table(upRegulated, paste("upRegulated_", gsub(".RData","",myGlobalEnv$mSigDB_SubName),"_",myGlobalEnv$tudyRefCase[k],".txt", sep=""), sep="\t") 
            
            
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
        getInTable(UpDownRegulatedTable, title=paste(myGlobalEnv$StudyRefCase[1]," / ",myGlobalEnv$StudyRefCase[2]))
    }
}