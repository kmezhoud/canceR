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
#' getPhenoTest(ENV$eSet)
#' }
#' @importFrom phenoTest ExpressionPhenoTest
#' @importFrom phenoTest getSignif
#' @importFrom phenoTest getFc
#' 

getPhenoTest <- function(){
    
    print("Getting eSet from selected Case and Genetic Profile...")
    geteSet()
    
    
    Biobase::pData(ENV$eSet)<- as.data.frame(lapply(Biobase::pData(ENV$eSet),
                                                    function(x) if(is.character(x)|is.factor(x)) gsub("N/A","NA",x,  ignore.case = TRUE) else x))
    
    
    ##popup select variable
    
    dialogOptionPhenoTest(ENV$eSet)
    tkwait.window(ENV$ttPheno)
    
    if(length(table(Biobase::pData(ENV$eSet)[ENV$Category]))>4){
        
        msgBadCategory <- paste("Category variable has more than 4 Levels. Select another category.")
        tkmessageBox(message= msgBadCategory, icon="info")
        stop(msgBadCategory)
    }
    
    
    if (inherits(try(epheno <- phenoTest::ExpressionPhenoTest(ENV$eSet, ENV$var2test,
                                                              p.adjust.method=ENV$p.adjustChoice, approach="frequentist"), silent=TRUE),"try-error"))
    {
        msgBadCovariables <- paste("There is incompatible variables. Select another Formula.")
        tkmessageBox(message=msgBadCovariables, icon="warning")
        stop(msgBadCovariables)
    } else{         
        epheno <- phenoTest::ExpressionPhenoTest(ENV$eSet, ENV$var2test,
                                                 p.adjust.method=ENV$p.adjustChoice, approach="frequentist", continuousCategories= length(table(pData(ENV$eSet)[ENV$Category])))
        
        
    }
    
    ## add continuousCategories and approach options
    #epheno <- ExpressionPhenoTest(ENV$eSet, var2test, p.adjust.method=p.adjustChoice, continuousCategories= 3)
    #epheno <- ExpressionPhenoTest(ENV$eSet, var2test, p.adjust.method=p.adjustChoice, continuousCategories= 3, approach="frequentist")
    
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
    if(exists("EventTime", envir = ENV)){
        ENV$PhenoTestResults <- PhenoTestResults[order(PhenoTestResults[,length(table(pData(ENV$eSet)[ENV$Category]))]),]
    } else{
        PhenoTestResults <- PhenoTestResults[order(PhenoTestResults[,2]),]
    }
    
    PhenoTestResults <-rbind(colnames(PhenoTestResults), PhenoTestResults)
    PhenoTestResults<- cbind(as.character(row.names(PhenoTestResults)), PhenoTestResults)
    
    Title <- paste(ENV$GenProfChoice,"Significant pVal <0.05", sep=":")
    getInTable(PhenoTestResults, Title)
    
    
    
    ### filtering only pVal < 0.05.
    if(exists("EventTime", envir = ENV)){
        PhenoTestResultsFilter <- subset(PhenoTestResults, 
                                         PhenoTestResults[,length(table(pData(ENV$eSet)[ENV$Category]))] <= 0.05)
        
    }else{
        PhenoTestResultsFilter <- subset(PhenoTestResults, PhenoTestResults[,3] <= 0.05)
    }
    
    title<- paste(ENV$GenProfChoice," ONLY Significant pVal <0.05", sep=":")
    getInTable(PhenoTestResultsFilter, title)
    
     
}