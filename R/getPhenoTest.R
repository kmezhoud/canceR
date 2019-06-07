#' Associate phenotype to Studies (cancers)
#' @usage
#' getPhenoTest()
#' 
#' @return a dataframe with disease/ variables association
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/prad_michPhenoTest1021.rds", sep=""))
#' \dontrun{
#' getPhenoTest(myGlobalEnv$eSet)
#' }
#' @importFrom phenoTest ExpressionPhenoTest
#' @importFrom phenoTest getSignif
#' @importFrom phenoTest getFc
#' 

getPhenoTest <- function(){
    
    print("Getting eSet from selected Case and Genetic Profile...")
    geteSet()
    
    
    Biobase::pData(myGlobalEnv$eSet)<- as.data.frame(lapply(Biobase::pData(myGlobalEnv$eSet),function(x) if(is.character(x)|is.factor(x)) gsub("N/A","NA",x,  ignore.case = TRUE) else x))
    
    
    ##popup select variable
    
    dialogOptionPhenoTest(myGlobalEnv$eSet)
    tkwait.window(myGlobalEnv$ttPheno)
    
    if(length(table(Biobase::pData(myGlobalEnv$eSet)[myGlobalEnv$Category]))>4){
        
        msgBadCategory <- paste("Category variable has more than 4 Levels. Select another category.")
        tkmessageBox(message= msgBadCategory, icon="info")
        stop(msgBadCategory)
    }
    
    
    if (inherits(try(epheno <- phenoTest::ExpressionPhenoTest(myGlobalEnv$eSet, myGlobalEnv$var2test, p.adjust.method=myGlobalEnv$p.adjustChoice, approach="frequentist"), silent=TRUE),"try-error"))
    {
        msgBadCovariables <- paste("There is incompatible variables. Select another Formula.")
        tkmessageBox(message=msgBadCovariables, icon="warning")
        stop(msgBadCovariables)
    } else{         
        epheno <- phenoTest::ExpressionPhenoTest(myGlobalEnv$eSet, myGlobalEnv$var2test, p.adjust.method=myGlobalEnv$p.adjustChoice, approach="frequentist", continuousCategories= length(table(pData(myGlobalEnv$eSet)[myGlobalEnv$Category])))
        
        
    }
    
    ## add continuousCategories and approach options
    #epheno <- ExpressionPhenoTest(myGlobalEnv$eSet, var2test, p.adjust.method=p.adjustChoice, continuousCategories= 3)
    #epheno <- ExpressionPhenoTest(myGlobalEnv$eSet, var2test, p.adjust.method=p.adjustChoice, continuousCategories= 3, approach="frequentist")
    
    # ##################################
    ephenoSign<- phenoTest::getSignif(epheno)
    fChange <- phenoTest::getFc(epheno)
    PhenoTestResults <- merge(ephenoSign, fChange, by="row.names")
    
    row.names(PhenoTestResults) <- PhenoTestResults[,1]
    PhenoTestResults <- PhenoTestResults[,-1]
    
    ## Round numeric values
    for (i in 1:length(PhenoTestResults)){
        if(is.numeric(PhenoTestResults[1,i])){
            PhenoTestResults[,i] <- round(PhenoTestResults[,i], digits=3)
        } 
        
    }
    ## rearange row by Significant pval
    if(exists("EventTime", envir = myGlobalEnv)){
        myGlobalEnv$PhenoTestResults <- PhenoTestResults[order(PhenoTestResults[,length(table(pData(myGlobalEnv$eSet)[myGlobalEnv$Category]))]),]
    } else{
        PhenoTestResults <- PhenoTestResults[order(PhenoTestResults[,2]),]
    }
    
    PhenoTestResults <-rbind(colnames(PhenoTestResults), PhenoTestResults)
    PhenoTestResults<- cbind(as.character(row.names(PhenoTestResults)), PhenoTestResults)
    
    Title <- paste(myGlobalEnv$GenProfChoice,"Significant pVal <0.05", sep=":")
    getInTable(PhenoTestResults, Title)
    
    
    
    ### filtering only pVal < 0.05.
    if(exists("EventTime", envir = myGlobalEnv)){
        PhenoTestResultsFilter <- subset(PhenoTestResults, PhenoTestResults[,length(table(pData(myGlobalEnv$eSet)[myGlobalEnv$Category]))] <= 0.05)
        
    }else{
        PhenoTestResultsFilter <- subset(PhenoTestResults, PhenoTestResults[,3] <= 0.05)
    }
    
    title<- paste(myGlobalEnv$GenProfChoice," ONLY Significant pVal <0.05", sep=":")
    getInTable(PhenoTestResultsFilter, title)
    
     
}