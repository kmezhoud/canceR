#' Dialog Box to set methylation options
#' @usage
#' dialogMetOption(ProfData, k)
#' @param ProfData adataframe with methylation data
#' @param k threshold of silencing gene 0:1
#'
#' @return a dialog box to set methylation option (threshold of silencing gene)
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' getMetDataMultipleGenes()
#' #dialogMetOption(ProfData,0.7)
#' }
#' 
#' 
dialogMetOption <- function(ProfData,k){
    
    ttMethData <- tktoplevel()
    #tkwm.geometry(ttMethData,"180x250")
    
    tktitle(ttMethData) <- paste(ENV$StudyRefCase[k],ENV$GenProfChoice[k], sep=" ")
    
    rEntry  = tclVar(.8)
    
    frameTHRESHOLD<- tkframe(ttMethData,relief="groove",borderwidth=2)
    tkgrid(tklabel(frameTHRESHOLD, text="Specify Threshold of correlation rate:"))
    tkgrid(frameTHRESHOLD)
    tkgrid.configure(frameTHRESHOLD, sticky="new")
    
    tkgrid(tkscale(frameTHRESHOLD,from=0.5,to=1,showvalue=TRUE,
                   variable=rEntry,resolution=.05,orient='horiz'))
    
    
    okOn <- function(){
    
        #Compute mean of correlation
        meanProfData <-as.matrix(round(colMeans(ProfData), digits=3))
        #Compute median of correlation
        medianProfData<- as.matrix(apply(ProfData, 2, median))
        #Entry Threshold of correlation
        seuilRVal <- as.numeric(tclvalue(rEntry))
        
        MeanTable <- subset(meanProfData, meanProfData[,1] > seuilRVal)
        MedianTable <- subset(medianProfData, medianProfData[,1] > seuilRVal)
        
        mergeTable <- merge(MeanTable, MedianTable, by="row.names")
        
        colnames(mergeTable) <- c("Gene","Mean", "Median")
        
        title <- paste (ENV$StudyRefCase[k],":",
                        "Correlation of silencing gene expression by Methylation",
                        "r(met/mRNA)>",
                        seuilRVal,ENV$GenProfChoice[k], sep=" " )
        
        getInTable(mergeTable, title=title)
        
        
        tkdestroy(ttMethData)
    }
    
    Ok.but <- tkbutton(ttMethData, text= "OK", command= okOn)
    
    
    tkgrid(Ok.but)
    tkgrid.configure(Ok.but, sticky="n", column=0)
    
    
    tkwait.window(ttMethData)
    
}