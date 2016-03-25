#' Plotting two genetic profiles for  one Gene 
#' @usage plot_1Gene_2GenProfs()
#' @return plot
#' @export
#' @examples 
#' load(paste(path.package("canceR"),"/data/gbm_tcgaPlotTwoGenProf.RData", sep=""))
#' \dontrun{
#' plot_1Gene_2GenProfs()
#' }
plot_1Gene_2GenProfs <- function(){
    
    
    
    if(!exists("curselectCases", envir = myGlobalEnv)){
        msgNoCaseChoice= paste("Select at least ONE Case and TWO Genetic Profiles")
        tkmessageBox(message= msgNoCaseChoice)
        stop(msgNoCaseChoice)
    } else if (!exists("curselectGenProfs", envir = myGlobalEnv)){
        
        msgNoGenProfChoice= paste("Select at least ONE Genetic Profile")
        tkmessageBox(message= msgNoGenProfChoice)
        stop(msgNoGenProfChoice)
    } else if(length(myGlobalEnv$GenProfChoice)%%2!=0 || !exists("GenProfChoice", envir = myGlobalEnv)) {
        msgLGenProfChoice= "Select (x2) Genetic Profiles"
        tkmessageBox(message=msgLGenProfChoice)
        tkfocus(myGlobalEnv$ttCasesGenProfs)
        stop(msgLGenProfChoice)
        
    } else if(length(myGlobalEnv$CaseChoice)!= length(myGlobalEnv$GenProfChoice)/2 || !exists("CaseChoice", envir = myGlobalEnv)){
        msgLCaseChoice="Associate two Genetic Profiles to only one Case for every Study"
        tkmessageBox(message=msgLCaseChoice)
        tkfocus(myGlobalEnv$ttCasesGenProfs)
        stop(msgLCaseChoice)
        
    } else{
        ## Test if all Cases were corresponded to TWO appropriate Genetic profiles (same Study)
        for (i in 0:(length(myGlobalEnv$StudyRefCase)-1)){
            if(myGlobalEnv$StudyRefCase[i+1] != myGlobalEnv$StudyRefGenProf[(i*2)+1] || myGlobalEnv$StudyRefCase[i+1] != myGlobalEnv$StudyRefGenProf[(i*2)+2]){
                msgBadChoice="Correpond two Genetic Profiles to one Case for the same Study"
                tkmessageBox(message=msgBadChoice, icon="warning")
                stop(msgBadChoice)
                
            }
        }
        
        for(s in 0:(length(myGlobalEnv$StudyRefCase)-1)){
            
            #Lchecked_GenProfs <- length(curselectGenProfs)
            dialogPlotOption_SkinCor(s)
            tkwait.window(myGlobalEnv$ttDialSkinCor)
            
            launchDialog <- function() {
                GENE <- modalDialog("CNA/Mut vs mRNA ", "Enter HUGO Gene Symbol", "MDM4")
                if (GENE == "ID_CANCEL") return()
                
                if(myGlobalEnv$Axes == "0"){
                GenProf1<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[s*2+1]]
                GenProf2<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[s*2+2]]
                } else{
                GenProf2<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[s*2+1]]
                GenProf1<-myGlobalEnv$GenProfsRefStudies[myGlobalEnv$curselectGenProfs[s*2+2]]
                    
                }
                
                twoGenProfs<- c(GenProf1,GenProf2)
                oneCase<-myGlobalEnv$CasesRefStudies[myGlobalEnv$curselectCases[s+1]]
                ##skin and Correlation are from dialogPlotOption_SkinCor.R
                plotCommand<- function(){
                    if (inherits(try(plot(myGlobalEnv$cgds,myGlobalEnv$checked_Studies[s+1],GENE,twoGenProfs ,oneCase, skin= myGlobalEnv$skin, add.corr=myGlobalEnv$correlation, legend.pos = 'topright'), silent=TRUE),"try-error"))
                    {
                        msgBadAcces <- paste(" Error in plot.new() : figure margins too large or failed access to cbioportal or genetic profiles are not numeric. For example Mutation variable is not numeric. ")
                     tkmessageBox(message=msgBadAcces, icon="warning")
                     stop(msgBadAcces)
                    } else{
                    plot(myGlobalEnv$cgds,myGlobalEnv$checked_Studies[s+1],GENE,twoGenProfs ,oneCase, skin= myGlobalEnv$skin, add.corr=myGlobalEnv$correlation, legend.pos = 'topright')
                    }
                }
                Title<- paste(myGlobalEnv$StudyRefCase[s+1], myGlobalEnv$CaseChoice[s+1], sep=": ")
                plotModel(plotCommand, title=Title,hscale=1, vscale=1)
            }
            launchDialog()
            
           
        }
    }
}