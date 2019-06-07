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
    
    if(!exists("curselectCases", envir = myGlobalEnv)){
        msgNoCaseChoice= paste("Select at least ONE Case and ONE Genetic Profile")
        tkmessageBox(message= msgNoCaseChoice)
        stop(msgNoCaseChoice)
    } else if (!exists("curselectGenProfs", envir = myGlobalEnv)){
        
        msgNoGenProfChoice= paste("Select at least ONE Genetic Profile")
        tkmessageBox(message= msgNoGenProfChoice)
        stop(msgNoGenProfChoice)
    } else if(length(myGlobalEnv$GenProfChoice)!=1 || !exists("GenProfChoice", envir = myGlobalEnv)) {
        msgLGenProfChoice= "Select ONE Genetic Profiles"
        tkmessageBox(message=msgLGenProfChoice, icon="warning")
        
        stop(msgLGenProfChoice)
        
    } else if(length(myGlobalEnv$CaseChoice)!= 1 || !exists("CaseChoice", envir = myGlobalEnv)){
        msgLCaseChoice="Select ONE Case"
        tkmessageBox(message=msgLCaseChoice, icon="warning")
        
        stop(msgLCaseChoice)
        
    } else{
        LengthCases <- 0
        d <- 0
        for (s in 1:length(myGlobalEnv$checked_Studies)){
            
            LengthCases <- LengthCases + myGlobalEnv$LCases[s]+1
            d <- d +1 
            if(myGlobalEnv$curselectCases <= LengthCases){
                
                
                
                
                Lchecked_GenProfs <- length(myGlobalEnv$curselectGenProfs)
                ######### Test if all cases were corresponded to appropriate Gen profs (same Study)
                for (i in 1:Lchecked_GenProfs){
                    if(myGlobalEnv$StudyRefCase[i]!=myGlobalEnv$StudyRefGenProf[i]){
                        
                        msgBadChoice="Correpond the Genetic Profile to the Case for the same Study"
                        tkmessageBox(message=msgBadChoice, icon="warning")
                        
                        #stop("Correpond the Genetic Profile to the Case for the same Study")
                        break
                        
                    }
                }
                
                #dialogPlotOption_SkinCor()
                #tkwait.window(ttDialSkinCor)
                launchDialog1 <- function() {
                    myGlobalEnv$GENE1 <- modalDialog("CNA/Mut vs mRNA ", "Enter HUGO Gene 1", "MDM4")
                    if (myGlobalEnv$GENE1 == "ID_CANCEL") return()
                    
                }
                launchDialog1()
                
                launchDialog2 <- function() {
                    GENE2 <- modalDialog("CNA/Mut vs mRNA ", "Enter HUGO Gene 2", "MDM2")
                    if (GENE2 == "ID_CANCEL") return()
                    oneGenProf<- myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs]
                    oneCase <- myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases]
                    plotCommand<- function(){
                        
                        if (inherits(try(plot(myGlobalEnv$cgds,myGlobalEnv$checked_Studies[s],c(myGlobalEnv$GENE1,GENE2),oneGenProf ,oneCase), silent=TRUE),"try-error"))
                        {
                            msgBadMARGIN <- paste(" Error in plot.new() : figure margins too large ")
                            tkmessageBox(message=msgBadMARGIN, icon="warning")
                            stop(msgBadMARGIN)
                        } else{         
                            plot(myGlobalEnv$cgds,myGlobalEnv$checked_Studies[s],c(myGlobalEnv$GENE1,GENE2),oneGenProf ,oneCase)
                            
                            
                        }
                        
                        
                        
                    }
                    Title<- paste (myGlobalEnv$StudyRefCase, myGlobalEnv$CaseChoice, sep=": ")
                    plotModel(plotCommand, title=Title,hscale=1, vscale=1)
                    plotCommand()
                }
                launchDialog2()
                
                
            }
        }
    }
    
}