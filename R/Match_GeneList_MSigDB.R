#' Search MSigDb that overlap gene list
#' @usage Match_GeneList_MSigDB
#' @return GeneList
#' @export
#' @examples
#' load(paste(path.package("canceR"),"/data/prad_michPhenoTest1021.RData", sep=""))
#' \dontrun{
#' Match_GeneList_MSigDB()
#' }
Match_GeneList_MSigDB <- function(){
    
    
    ttdialogMSigDB<-tktoplevel()
    tktitle(ttdialogMSigDB) <- "Setting MSigDB Directory"
    
    fromDir <- function(){
        ##Select directory which has MSigDB
        myGlobalEnv$dirMSigDBPath <- tk_choose.dir()
        tkdestroy(ttdialogMSigDB)
        #return(dirMSigDBPath)
    }
    
    fromExample <- function(){
        ##Or select MSigDB from package
        myGlobalEnv$dirMSigDBPath <- paste(path.package("canceR"),"/extdata/MSigDB/",sep="")
        tkdestroy(ttdialogMSigDB)
        #return(dirMSigDBPath)
    }
    
    text <- tklabel(ttdialogMSigDB,text="Select Directory with MSigDB.gmt files")
    tkgrid(text)
    fromDir.but <- tkbutton(ttdialogMSigDB, text = "Directory", command = fromDir)
    fromExample.but <- tkbutton(ttdialogMSigDB, text = "Example", command = fromExample)
    tkgrid(fromDir.but,fromExample.but)
    tkgrid.configure(fromDir.but,columnspan=1,sticky="w")
    
    tkwait.window(ttdialogMSigDB)
    
    MSigDBList <-  list.files(myGlobalEnv$dirMSigDBPath)  
    MSigDB_Rate <- 0
    for(i in 1 :length(MSigDBList)){
        
        MSigDBPath <- paste(myGlobalEnv$dirMSigDBPath,"/",MSigDBList[i] ,sep="")
        
        mSigDB<-readLines(MSigDBPath)
        
        #split each line at the tabs
        mSigDB<-strsplit(mSigDB,'\t')
        
        
        #use the first entry of each gene set as name
        names(mSigDB)<-sapply(mSigDB,function(x)x[1])
        
        
        #remove the first two entries
        mSigDB<-sapply(mSigDB,function(x)x[3:length(x)])
        
        
        ##get a unique list of all genes
        #allGenes<-unique(unlist(mSigDB))
        
        
        Matched_GeneSets <- sapply(myGlobalEnv$GeneList, function(x) which(sapply(x, regexpr, mSigDB)!=-1))
        
        MeanOfGeneSetsHavingGene <- round(length(unlist(Matched_GeneSets))/length(Matched_GeneSets),digits=2)
        
        
        MSigDB_Rate <- rbind(MSigDB_Rate,cbind(MSigDBList[i],MeanOfGeneSetsHavingGene))
        
        
        print(paste(MSigDBList[i],MeanOfGeneSetsHavingGene, sep=": "))
        
    }
    MSigDB_Rate <- MSigDB_Rate[-1,]
    colnames(MSigDB_Rate) <- c("MSigDB","Matched(Genes/Gene Sets)")
    #rownames(MSigDB_Rate) <- MSigDB_Rate[,1]
    title<- "Mean of Gene Sets overlapping Gene List"
    getInTable(MSigDB_Rate, title)
    
    
}

