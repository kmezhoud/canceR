#' plot correlation of two genes expressions.
#' @usage plot_2Genes_1GenProf()
#' @return plot
#' @export
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' plot_2Genes_1GenProf()
#' }
plot_2Genes_1GenProf <- function(){
    
    if(!exists("curselectCases", envir = ENV)){
        msgNoCaseChoice= paste("Select at least ONE Case and ONE Genetic Profile")
        tkmessageBox(message= msgNoCaseChoice)
        stop(msgNoCaseChoice)
    } else if (!exists("curselectGenProfs", envir = ENV)){
        
        msgNoGenProfChoice= paste("Select at least ONE Genetic Profile")
        tkmessageBox(message= msgNoGenProfChoice)
        stop(msgNoGenProfChoice)
    } else if(length(ENV$GenProfChoice)!=1 || !exists("GenProfChoice", envir = ENV)) {
        msgLGenProfChoice= "Select ONE Genetic Profiles"
        tkmessageBox(message=msgLGenProfChoice, icon="warning")
        
        stop(msgLGenProfChoice)
        
    } else if(length(ENV$CaseChoice)!= 1 || !exists("CaseChoice", envir = ENV)){
        msgLCaseChoice="Select ONE Case"
        tkmessageBox(message=msgLCaseChoice, icon="warning")
        
        stop(msgLCaseChoice)
        
    } else{
        LengthCases <- 0
        d <- 0
        for (s in 1:length(ENV$checked_Studies)){
            
            LengthCases <- LengthCases + ENV$LCases[s]+1
            d <- d +1 
            if(ENV$curselectCases <= LengthCases){
                
                
                
                
                Lchecked_GenProfs <- length(ENV$curselectGenProfs)
                ######### Test if all cases were corresponded to appropriate Gen profs (same Study)
                for (i in 1:Lchecked_GenProfs){
                    if(ENV$StudyRefCase[i]!=ENV$StudyRefGenProf[i]){
                        
                        msgBadChoice="Correspond the Genetic Profile to the Case for the same Study"
                        tkmessageBox(message=msgBadChoice, icon="warning")
                        
                        #stop("Correpond the Genetic Profile to the Case for the same Study")
                        break
                        
                    }
                }
                
                #dialogPlotOption_SkinCor()
                #tkwait.window(ttDialSkinCor)
                launchDialog1 <- function() {
                    ENV$GENE1 <- modalDialog("CNA/Mut vs mRNA ", "Enter HUGO Gene 1", "MDM4")
                    if (ENV$GENE1 == "ID_CANCEL") return()
                    
                }
                launchDialog1()
                
                launchDialog2 <- function() {
                    GENE2 <- modalDialog("CNA/Mut vs mRNA ", "Enter HUGO Gene 2", "MDM2")
                    if (GENE2 == "ID_CANCEL") return()
                    oneGenProf<- ENV$GenProfsRefStudies[ENV$curselectGenProfs]
                    oneCase <- ENV$CasesRefStudies[ENV$curselectCases]
                    plotCommand<- function(){
                        
                        if (inherits(try(plot(ENV$cgds,ENV$checked_Studies[s],c(ENV$GENE1,GENE2),oneGenProf ,oneCase), silent=TRUE),"try-error"))
                        {
                            msgBadMARGIN <- paste(" Error in plot.new() : figure margins too large ")
                            tkmessageBox(message=msgBadMARGIN, icon="warning")
                            stop(msgBadMARGIN)
                        } else{         
                            plot(ENV$cgds,ENV$checked_Studies[s],c(ENV$GENE1,GENE2),oneGenProf ,oneCase)
                            
                            
                        }
                        
                        
                        
                    }
                    Title<- paste (ENV$StudyRefCase, ENV$CaseChoice, sep=": ")
                    plotModel(plotCommand, title=Title,hscale=1, vscale=1)
                    plotCommand()
                }
                launchDialog2()
                
                
            }
        }
    }
    
}