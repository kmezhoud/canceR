
#mSigDB<- list(set1=c("a","b","d","x"), set2=c("b","c","q","m"), set3=c("b","f","e","k","q","h"))
## Remove gene in duplicates
#sapply(1:length(MSigDB),function(x){MSigDB[[x]][as.numeric(table(unlist(MSigDB))[MSigDB[[x]]])==1]})
#MSigDB <- readLines("/home/mezhoud/CGDS-R/canceRdev/inst/extdata/MSigDB/c5.bp.v4.0.symbols.gmt")

#' get gene list from MSigDB
#' @usage
#' getGeneListFromMSigDB()
#' 
#' @return a vector with gene list
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcgaGSEAlm1021.rds", sep=""))
#' \dontrun{
#' getGeneListFromMSigDB()
#'}
#'
getGeneListFromMSigDB <- function(){
    if(exists("GeneListMSigDB", envir = myGlobalEnv)){
        rm("GeneListMSigDB", envir = myGlobalEnv)
        rm("GeneList", envir = myGlobalEnv)
    }
    
    getMSigDBfile()
    MSigDB <- myGlobalEnv$mSigDB
    
    #split each line at the tabs
    MSigDB<-strsplit(MSigDB,'\t')
    #use the first entry of each gene set as name
    names(MSigDB)<-sapply(MSigDB,function(x)x[1])
    #remove the first two entries
    MSigDB<-sapply(MSigDB,function(x)x[3:length(x)])
    
    
    ## Filtring interesting/Wanted GeneSet before 
    ## removing duplicate genes between Gene Sets
    # 
    # regex <- c("DNA_DAMAGE_CHECKPOINT",
    #            "DNA_INTEGRITY_CHECKPOINT",
    #            "MITOTIC_CELL_CYCLE_CHECKPOINT",
    #            "DNA_REPAIR",
    #            "NEGATIVE_REGULATION_OF_PROGRAMMED_CELL_DEATH",
    #            "INDUCTION_OF_APOPTOSIS_BY_INTRACELLULAR_SIGNALS", 
    #            "DNA_DAMAGE_RESPONSESIGNAL_TRANSDUCTION_RESULTING_IN_INDUCTION_OF_APOPTOSIS",
    #             "NEGATIVE_REGULATION_OF_APOPTOSIS",
    #             "RESPONSE_TO_OXIDATIVE_STRESS",
    #            "STRESS_ACTIVATED_PROTEIN_KINASE_SIGNALING_PATHWAY",
    #            "NEGATIVE_REGULATION_OF_CELL_ADHESION",
    #            "REGULATION_OF_CELL_CELL_ADHESION",
    #            "POSITIVE_REGULATION_OF_CELL_ADHESION"
    #            )
    dialoggetGeneListMSigDB(MSigDB)
    regex <- myGlobalEnv$regex
    
    SubMSigDB <- 0
    for( i in 1:length(regex)){
        
        SubMSigDB_regex <- MSigDB[grep(regex[i],names(MSigDB), ignore.case = TRUE)]
        SubMSigDB <- c( SubMSigDB,SubMSigDB_regex)
    }
    SubMSigDB <- SubMSigDB[-1]
    MSigDB <- SubMSigDB
    
    ## remove duplicates genes from Gene Sets:
    ## any gene must belong to one gene Set. First one.
    GeneListMSigDB <- sapply(1:length(MSigDB),function(x){
        c <- list() 
        for (j in 1:x){
            c <- c(c,MSigDB[[j]])}
        MSigDB[[x]][table(unlist(c))[MSigDB[[x]]]==1]
        
    })
    
    names(GeneListMSigDB) <- names(MSigDB)
    
    GeneListMSigDB <- as.data.frame(unlist(GeneListMSigDB[sapply(GeneListMSigDB,length)>0]))
    
    rnames <- gsub("[[:digit:]]", "", rownames(GeneListMSigDB))
    GeneListMSigDB <- cbind(rnames, GeneListMSigDB)
    rownames(GeneListMSigDB) <- NULL
    colnames(GeneListMSigDB) <- c("GeneSet","Gene")
    myGlobalEnv$GeneListMSigDB <- GeneListMSigDB
    myGlobalEnv$GeneList <- t(as.character(GeneListMSigDB[,2]))
}