#' Plotting two genetic profiles for  one Gene 
#' @usage plot_1Gene_2GenProfs()
#' @return plot
#' @export
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/gbm_tcgaPlotTwoGenProf.rds", sep=""))
#' \dontrun{
#' plot_1Gene_2GenProfs()
#' }
plot_1Gene_2GenProfs <- function(){
    
    
    
    if(!exists("curselectCases", envir = ENV)){
        msgNoCaseChoice= paste("Select at least ONE Case and TWO Genetic Profiles")
        tkmessageBox(message= msgNoCaseChoice)
        stop(msgNoCaseChoice)
    } else if (!exists("curselectGenProfs", envir = ENV)){
        
        msgNoGenProfChoice= paste("Select at least ONE Genetic Profile")
        tkmessageBox(message= msgNoGenProfChoice)
        stop(msgNoGenProfChoice)
    } else if(length(ENV$GenProfChoice)%%2!=0 || !exists("GenProfChoice", envir = ENV)) {
        msgLGenProfChoice= "Select (x2) Genetic Profiles"
        tkmessageBox(message=msgLGenProfChoice)
        tkfocus(ENV$ttCasesGenProfs)
        stop(msgLGenProfChoice)
        
    } else if(length(ENV$CaseChoice)!= length(ENV$GenProfChoice)/2 || !exists("CaseChoice", envir = ENV)){
        msgLCaseChoice="Associate two Genetic Profiles to only one Case for every Study"
        tkmessageBox(message=msgLCaseChoice)
        tkfocus(ENV$ttCasesGenProfs)
        stop(msgLCaseChoice)
        
    } else{
        ## Test if all Cases were corresponded to TWO appropriate Genetic profiles (same Study)
        for (i in 0:(length(ENV$StudyRefCase)-1)){
            if(ENV$StudyRefCase[i+1] != ENV$StudyRefGenProf[(i*2)+1] || ENV$StudyRefCase[i+1] != ENV$StudyRefGenProf[(i*2)+2]){
                msgBadChoice="Correspond two Genetic Profiles to one Case for the same Study"
                tkmessageBox(message=msgBadChoice, icon="warning")
                stop(msgBadChoice)
                
            }
        }
        
        for(s in 0:(length(ENV$StudyRefCase)-1)){
            
            #Lchecked_GenProfs <- length(curselectGenProfs)
            dialogPlotOption_SkinCor(s)
            tkwait.window(ENV$ttDialSkinCor)
            
            launchDialog <- function() {
                GENE <- modalDialog("CNA/Mut vs mRNA ", "Enter HUGO Gene Symbol", "MDM4")
                if (GENE == "ID_CANCEL") return()
                
                if(ENV$Axes == "0"){
                GenProf1<-ENV$GenProfsRefStudies[ENV$curselectGenProfs[s*2+1]]
                GenProf2<-ENV$GenProfsRefStudies[ENV$curselectGenProfs[s*2+2]]
                } else{
                GenProf2<-ENV$GenProfsRefStudies[ENV$curselectGenProfs[s*2+1]]
                GenProf1<-ENV$GenProfsRefStudies[ENV$curselectGenProfs[s*2+2]]
                    
                }
                
                twoGenProfs<- c(GenProf1,GenProf2)
                oneCase<-ENV$CasesRefStudies[ENV$curselectCases[s+1]]
                ##skin and Correlation are from dialogPlotOption_SkinCor.R
                plotCommand<- function(){
                    if (inherits(try(plot(ENV$cgds,ENV$checked_Studies[s+1],GENE,twoGenProfs ,oneCase, skin= ENV$skin, add.corr=ENV$correlation, legend.pos = 'topright'), silent=TRUE),"try-error"))
                    {
                        msgBadAcces <- paste(" Error in plot.new() : figure margins too large or failed access to cbioportal or genetic profiles are not numeric. For example Mutation variable is not numeric. ")
                     tkmessageBox(message=msgBadAcces, icon="warning")
                     stop(msgBadAcces)
                    } else{
                    plot(ENV$cgds,ENV$checked_Studies[s+1],GENE,twoGenProfs ,oneCase, skin= ENV$skin, add.corr=ENV$correlation, legend.pos = 'topright')
                    }
                }
                Title<- paste(ENV$StudyRefCase[s+1], ENV$CaseChoice[s+1], sep=": ")
                plotModel(plotCommand, title=Title,hscale=1, vscale=1)
            }
            launchDialog()
            
           
        }
    }
}