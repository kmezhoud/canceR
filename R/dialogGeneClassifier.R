#' Dialogue Box for gene classifier setting: sample size and postprob threshold
#' @usage
#' dialogGeneClassifier(Lchecked_Cases,entryWidth = 10,returnValOnCancel = "ID_CANCEL")
#' @param Lchecked_Cases integer with a number of checked cases
#' @param  entryWidth integer default 10
#' @param returnValOnCancel "ID_CANCEL"
#' 
#' @return a dataframe with genes classes
#' @export
#'
#' @examples
#' load(paste(path.package("canceR"),"/data/gbm_tcgaPlotTwoGenProf.RData", sep=""))
#' \dontrun{
#' getGenesClassifier()
#' dialogGeneClassifier(1,10,returnValOnCancel = "ID_CANCEL")
#'}
#'
#'

dialogGeneClassifier <- function(Lchecked_Cases,entryWidth = 10,
                                 returnValOnCancel = "ID_CANCEL") {
    
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, "Sampling...")
    
    
    txtMsg <- tklabel(dlg, text = "Max Nbr of samples does not exceed the smaller samples Nbr of all checked Cases",background="white")
    tkgrid(txtMsg)
    tkgrid.configure(txtMsg, column=0, row=1,sticky="we")
    textEntryVarTcl <- tclVar("50")
    textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
                               textvariable = textEntryVarTcl)
    #tkgrid(tklabel(dlg, text = "       "))
    txtNbr <- tklabel(dlg, text = "Number of Samples")
    tkgrid(txtNbr)
    tkgrid.configure(txtNbr, column=0, row=2, sticky="w")
    tkgrid(textEntryWidget)
    tkgrid.configure(textEntryWidget, column=0, row=2, sticky="ne")
    
    frameCases<- tkframe(dlg,relief="groove",borderwidth=2)
    tkgrid(tklabel(frameCases, text="Checked Cases:"))
    tkgrid.configure(frameCases, sticky="new")
    
    
    
    for(i in 3:(Lchecked_Cases+2)){
        
        stdy <- tklabel(frameCases, text=myGlobalEnv$StudyRefCase[i-2])
        tkgrid(stdy)
        tkgrid.configure(stdy, column=0,row=i, sticky="e")
        SamplesNbr <-tklabel(frameCases, text = paste(": " ,myGlobalEnv$CaseChoice[i-2]))
        tkgrid(SamplesNbr)
        tkgrid.configure(SamplesNbr, column=1, row=i,sticky="w")
    }
    
    ReturnSamplesNbr <- returnValOnCancel
    
    textEntrylpThreshold <- tclVar("0.95")
    textEntryWidget2 <- tkentry(dlg, width = paste(entryWidth),
                                textvariable = textEntrylpThreshold)
    #tkgrid(tklabel(dlg, text = "       "))
    TxtPostProb <- tklabel(dlg, text="Posterior Probability Threshold")
    tkgrid(TxtPostProb)
    tkgrid.configure(TxtPostProb, column=0, row= (Lchecked_Cases+3), sticky="w")
    
    txtThres <- tklabel(dlg, text = "lpThreshold")
    tkgrid(txtThres)
    tkgrid.configure(txtThres, column=0, row=(Lchecked_Cases+4), sticky="w")
    tkgrid(textEntryWidget2)
    tkgrid.configure(textEntryWidget2, column=0, row=(Lchecked_Cases+4), sticky="ne")
    
    
    onOK <- function() {
        myGlobalEnv$ReturnSamplesNbr <- as.numeric(tclvalue(textEntryVarTcl))
        myGlobalEnv$Threshold <- as.numeric(tclvalue(textEntrylpThreshold))
        
        tkgrab.release(dlg)
        tkdestroy(dlg)
        
    }
    onCancel <- function() {
        #ReturnSamplesNbr <- returnValOnCancel
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
    Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
    tkgrid(Cancel.but)
    tkgrid.configure(Cancel.but, column=0,row= (Lchecked_Cases+5), sticky="w")
    tkgrid(OK.but)
    tkgrid.configure(OK.but, column=0,row= (Lchecked_Cases+5), sticky="e")
    #tkgrid(tklabel(dlg, text = "    "))
    
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg)})
    tkbind(textEntryWidget, "<Return>", onOK)
    tkwait.window(dlg)
    
    return(c(myGlobalEnv$ReturnSamplesNbr, myGlobalEnv$Threshold))
    
    
}