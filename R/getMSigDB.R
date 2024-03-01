#' Reduce MSigDB size for only gene list
#' @usage getMSigDB(eSet, k)
#' @param eSet Expression Set
#' @param k integer Number of studies
#' 
#' @return MSigDB for user gene List
#' @export
#' 
#' @examples
#'  d <- 7
#' \dontrun{
#' setWorkspace()
#' getMSigDB(eSet = ENV$eSetClassifier,k = 1)
#' }
#' 
getMSigDB <- function(eSet,k){
     
    ## Selecting GeneList from getGeneList.R or getGenesClasses.R functions.
    ifExist <- function(obj, env = ENV()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            ENV$GeneList <- rownames(ENV$GenesDetails[[k]])
            ENV$GeneListfile <- paste(names(ENV$GenesDetails)[k])
            return(ENV$GeneListfile)
        }
    }
 
    ifExist(ENV$GenesDetails, ENV)
    
    #list of GO terms (biological processes) acquired from MSigDB
    #http://www.broadinstitute.org/gsea/downloads.jsp
    mSigDBPath <- tclvalue(tkgetOpenFile(filetypes = "{{GMT Files} {.gmt}} {{All files} *}",
                                         title="load MSigDB for Gene Symbol List ")) # Very simple, isn't it?
    if (!nchar(mSigDBPath)) {
        tkmessageBox(message = "No file was selected!")
    } else {
        tkmessageBox(message = paste("The file selected was", mSigDBPath))
    }
    ##verify if the mSigDB_forGenelist  exists or not If yes skip if no do the remain codes
    ####Changing the gene list file with genes classes
    mSigDB_SubName <- paste(basename(mSigDBPath), "_",basename(ENV$GeneListfile),".RData",sep="")
    mSigDB_SubName <- gsub(".txt","", mSigDB_SubName)
    ENV$mSigDB_SubName <- gsub(".gmt","", mSigDB_SubName)
    
    #########################
    ####Create a new directory "MSigDB" from MSigDB and All specific MSigDB created
    ## for specific Gene List (MSigDB_GeneList.RData)
    ##Set MSigDB as tempary work directory
    Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
    if(!file.exists("Results/MSigDB/")){
        dir.create(file.path(paste(getwd(), "/Results/MSigDB", sep="")), showWarnings = FALSE)
    } 
    ###########
    
    workspace <- getwd()
    setwd("./Results/MSigDB")
    if(file.exists(ENV$mSigDB_SubName)&& exists("mSigDB_forGeneList",envir = ENV)){
        setwd(workspace)
        print("skip getMSigDB: The SubMSigDB existe in /Results/MSigDB folder.")
        ENV$mSigDB_SubName <-ENV$mSigDB_SubName
    } else{
        setwd(workspace)
        print("Getting SubMSigDB for requested eSet and genes list.")
        mSigDB<-readLines(mSigDBPath)
        
        #split each line at the tabs
        mSigDB<-strsplit(mSigDB,'\t')
        
        
        #use the first entry of each gene set as name
        names(mSigDB)<-sapply(mSigDB,function(x)x[1])
               
        #remove the first two entries
        mSigDB<-sapply(mSigDB,function(x)x[3:length(x)])
        
        
        mSigDB_forGeneList <- sapply(rownames(Biobase::exprs(eSet)), function(x) as.numeric(sapply(mSigDB, '%in%', x=x)))
        rownames(mSigDB_forGeneList) <- names(mSigDB)
        
        #save the processed gene sets for further use
        #save(mSigDB_forGeneList,file='mSigDB_forGeneList.RData')
        mSigDB_SubName <-paste(basename(mSigDBPath), "_",basename(ENV$GeneListfile),".RData",sep="")
        mSigDB_SubName <- gsub(".txt","", mSigDB_SubName)
        mSigDB_SubName <- gsub(".gmt","", mSigDB_SubName)
        ENV$mSigDB_SubName <- mSigDB_SubName
        ENV$mSigDB_forGeneList<-mSigDB_forGeneList
        
        #assign(mSigDB_SubName,mSigDB_SubName, envir=.GlobalEnv)
        workspace <- getwd()
        setwd("./Results/MSigDB")
        Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        save(mSigDB_forGeneList,file=ENV$mSigDB_SubName, envir = ENV)
        setwd(workspace)
    }
}
