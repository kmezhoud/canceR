#' get Genes Classifier
#' @usage
#' getGenesClassifier()
#'
#' @return a data frma with genes classes
#' @export
#'
#' @examples
#' x <- 0
#' \dontrun{
#' load(paste(.libPaths(),"/canceR/data/brca_tcga73genes.RData", sep=""))
#' getGenesClassifier()
#' }
#' @importFrom geNetClassifier calculateGenesRanking
#' @importFrom geNetClassifier genesDetails
#' @importFrom Biobase ExpressionSet
#' @importFrom Biobase exprs
#' @importFrom Biobase openPDF
#' @importFrom Biobase pData
#' @importFrom Biobase varLabels
#' @importFrom GSEABase GeneSet
#' @importFrom GSEABase GeneSetCollection

getGenesClassifier <- function(){
    
    ## function to remove existant object
    ifrm <- function(obj, env = myGlobalEnv()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    
    
    
    ####Create a new directory "Results/Classifier" for output: graph of significant genes and Table of genes details
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(!file.exists("Results/")){
        dir.create(file.path(paste(getwd(), "/Results", sep="")), showWarnings = FALSE)
        dir.create(file.path(paste(getwd(), "/Results/Classifier", sep="")), showWarnings = FALSE)
    } else if (!file.exists("Results/Classifier/")){
        dir.create(file.path(paste(getwd(), "/Results/Classifier", sep="")), showWarnings = FALSE)
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
            ProfDataAll<-0
            ProfData<-0
            LengthGenProfs<-0
            LengthCases<-0
            for (i in 1:Lchecked_Studies){
                Si <- myGlobalEnv$checked_StudyIndex[i]
                progressBar_ProfilesData <- tkProgressBar(title = myGlobalEnv$Studies[Si], min = 0,
                                                          max = Lchecked_GenProf, width = 400)
                
                #tkfocus(progressBar_ProfilesData)
                LastLengthGenProfs <- LengthGenProfs
                LengthGenProfs <- LengthGenProfs + myGlobalEnv$LGenProfs[i]+1
                LastLengthCases <- LengthCases
                LengthCases <- LengthCases + myGlobalEnv$LCases[i]+1
                
                for (k in 1:Lchecked_Cases){
                    
                    Sys.sleep(0.1)
                    setTkProgressBar(progressBar_ProfilesData, k, label=paste( round(k/Lchecked_GenProf*100, 0),
                                                                               "% of Expression Set"))
                    
                    if (myGlobalEnv$curselectGenProfs[k] <= LengthGenProfs && myGlobalEnv$curselectGenProfs[k]>LastLengthGenProfs){    
                        
                        GenProf<- myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[k]]
                        
                        Case<- myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[k]]
                        
                        ProfData<- getProfileData(myGlobalEnv$cgds,myGlobalEnv$GeneList, GenProf,Case)
                        
                        ProfData <- t(ProfData)
                        
                        ##remove all NAs rows
                        ProfData<- ProfData[which( apply( !( apply(ProfData,1,is.na) ),2,sum)!=0 ),]
                        
                        ##Convert data frame to numeric structure
#                         print("converting data frame of Profile data to numeric stucture...")
#                         
#                         for(i in 1:ncol(ProfData)){
#                             
#                             ProfData[,i] <- as.numeric(ProfData[,i])
#                         }
                        ## for loop is faster than apply fonction
                        #rnames <- rownames(ProfData)
                        #ProfData <- as.data.frame(apply(ProfData,2 ,function(x) as.numeric(x)))
                        #rownames(ProfData) <- rnames
                        
                        
                                            
                        title <- paste(myGlobalEnv$StudyRefGenProf[k],":",myGlobalEnv$GenProfChoice[k])
                        
                        if(ncol(ProfData)<myGlobalEnv$ReturnDialogGeneClasses[1]){
                            msgBigSampl <- paste(myGlobalEnv$StudyRefCase[k], "has only", ncol(ProfData),"samples.","\nSelect at Max: ",ncol(ProfData), "samples")
                            tkmessageBox(message=msgBigSampl, icon="info")
                            close(progressBar_ProfilesData)
                            stop(msgBigSampl)
                        }
                        set.seed(1234)
                        SamplingProfData <- t(apply(ProfData, 1,function(x)sample(x[!is.na(x)],myGlobalEnv$ReturnDialogGeneClasses[1])))
                        
                        SamplingColnamesProfData <- sample(colnames(ProfData), myGlobalEnv$ReturnDialogGeneClasses[1])
                        
                        colnames(SamplingProfData) <- SamplingColnamesProfData
                        SamplingProfsData <- cbind.na(SamplingProfsData,SamplingProfData)
                        print(paste("Sampling from ",myGlobalEnv$StudyRefCase[k]))
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
            print("converting DiseaseType as DataFrame...")
            
            if (inherits(try(rownames(DiseasesType) <- colnames(SamplingProfsData) , silent=TRUE),"try-error"))
            {
                msgDuplicateSamples <- paste("Duplicate sample names are not allowed. Do no select two studies from the same disease.")
                tkmessageBox(message=msgDuplicateSamples , icon="warning")
                
                stop(msgDuplicateSamples)
            } else{ 
                
                rownames(DiseasesType) <- colnames(SamplingProfsData)
            }
            
            
            print("adding rownames to DiseasesType...")
            ## create labelDescription for columns of phenoData. 
            ## labeldescription is used by Biobase packages
            ## In our case labelDescription is Equal to column names
            ##metaData <- data.frame(labelDescription= colnames(ClinicalData), row.names=colnames(ClinicalData))        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame   
            metaData <- data.frame(labelDescription= "DiseasesType", row.names="DiseasesType")        ## Bioconductor’s Biobase package provides a class called AnnotatedDataFrame   
            
            print("getting metaData...")
            ##that conveniently stores and manipulates 
            ##the phenotypic data and its metadata in a coordinated fashion. 
            phenoData<-new("AnnotatedDataFrame", data=DiseasesType, varMetadata=metaData)    
            print("getting phenoData...")
            ##Assembling an ExpressionSet  
            
            
            eSetClassifier<-Biobase::ExpressionSet(assayData=SamplingProfsData, phenoData=phenoData, annotation="GO")
            print("getting eSetClassifier...")
            if(min(Biobase::exprs(eSetClassifier), na.rm=TRUE)<0){
                print("There are negative values. Translating values by adding the absolute of minimum value to all matrix")
                Biobase::exprs(eSetClassifier) <- Biobase::exprs(eSetClassifier)+(abs(min(Biobase::exprs(eSetClassifier), na.rm=TRUE)))
            }
            
            myGlobalEnv$eSetClassifier <- eSetClassifier
            
            if (inherits(try(signGenesRank_DiseaseType<- geNetClassifier::calculateGenesRanking(myGlobalEnv$eSetClassifier[,1:(myGlobalEnv$ReturnDialogGeneClasses[1]*Lchecked_Cases)], sampleLabels="DiseasesType", lpThreshold=myGlobalEnv$ReturnDialogGeneClasses[2], returnRanking="significant", plotLp = FALSE), silent=TRUE),"try-error"))
            {
                msgNoSignificantDiff <- paste("The current genes don't differentiate the classes..")
                tkmessageBox(message=msgNoSignificantDiff , icon="warning")
                
                stop(msgNoSignificantDiff )
            } else{ 
                
                signGenesRank_DiseaseType<- geNetClassifier::calculateGenesRanking(myGlobalEnv$eSetClassifier[,1:(myGlobalEnv$ReturnDialogGeneClasses[1]*Lchecked_Cases)], sampleLabels="DiseasesType", lpThreshold=myGlobalEnv$ReturnDialogGeneClasses[2], returnRanking="significant", plotLp = FALSE)
            }
            
            plotCommand<- function(){
                signGenesRank_DiseaseType<- geNetClassifier::calculateGenesRanking(myGlobalEnv$eSetClassifier[,1:(myGlobalEnv$ReturnDialogGeneClasses[1]*Lchecked_Cases)], sampleLabels="DiseasesType", lpThreshold=myGlobalEnv$ReturnDialogGeneClasses[2], returnRanking="significant", plotLp = TRUE)
                
                
            }
            plotModel(plotCommand, title="Ranking Genes By Diseases",hscale=1, vscale=1)
            
            print("plotting model...")
            
            
            myGlobalEnv$GenesDetails <- geNetClassifier::genesDetails(signGenesRank_DiseaseType)
            print("getting Genes Details...")
            
            GenesDetailsTab <- do.call(rbind.data.frame, myGlobalEnv$GenesDetails)
            GenesDetailsTab <- t(t(as.data.frame.matrix(GenesDetailsTab)))
            
            
            title=paste("Ranking of significant Genes By Classes")
            getInTable(GenesDetailsTab, title)
            
            
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            
            name1 <- paste(myGlobalEnv$StudyRefCase, collapse='_', sep="_")
            name2 <- basename(gsub(".txt","",myGlobalEnv$GeneListfile))
            name3 <- paste(name2,"_" ,name1,".txt",sep="")
            path  <- paste("./Results/Classifier/", name3, sep="")
            write.table(GenesDetailsTab,file=path, col.names=TRUE, quote=FALSE, row.names=TRUE, sep="\t")
            
            msgSavedGenesDetailsClassifier="The table of the ranking genes was saved to /Results/Classifier/"
            tkmessageBox(message=msgSavedGenesDetailsClassifier, icon="info")
            
           
        }
    
    
    ifrm(GenesDetails, myGlobalEnv)
    
}