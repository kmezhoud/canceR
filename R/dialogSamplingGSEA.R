#' Dialog  Box for Sampling patients from expression profile data used for GSEA-R (Broad Institute)
#' @param n_checked_GenProf  Number of checked genetic profiles
#' @param entryWidth 10
#' @param returnValOnCancel  "ID_CANCEL"
#' 
#' @usage dialogSamplingGSEA(n_checked_GenProf, entryWidth = 10,returnValOnCancel = "ID_CANCEL")
#' 
#' @return A vector with sampling size
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' Run.GSEA()
#' #dialogSamplingGSEA(1,entryWidth=10,returnValOnCancel = "ID_CANCEL")
#' }
#'  
dialogSamplingGSEA <- function( n_checked_GenProf, entryWidth = 10,
                                returnValOnCancel = "ID_CANCEL") {
    
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, "Sampling from Profile Data matrix")
    
    txtMsg <- tklabel(dlg, text = "")
    tkgrid(txtMsg)
    tkgrid.configure(txtMsg, column=0, row=1,sticky="we")
    
    textEntryVarTcl <- tclVar("50")
    textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
                               textvariable = textEntryVarTcl)
    
    txtNbr <- tklabel(dlg, text = "Number of Samples")
    tkgrid(txtNbr)
    tkgrid.configure(txtNbr, column=0, row=2, sticky="w")
    tkgrid(textEntryWidget)
    tkgrid.configure(textEntryWidget, column=0, row=2, sticky="ne")
    
    frameCases<- tkframe(dlg,relief="groove",borderwidth=2)
    tkgrid(tklabel(frameCases, text="Checked Cases:"))
    tkgrid.configure(frameCases, sticky="new")
    
    for(i in 3:(n_checked_GenProf+2)){
        
        stdy <- tklabel(frameCases, text=ENV$StudyRefCase[i-2])
        tkgrid(stdy)
        tkgrid.configure(stdy, column=0,row=i, sticky="e")
        SamplesNbr <-tklabel(frameCases, text = paste(": " ,ENV$CaseChoice[i-2]))
        tkgrid(SamplesNbr)
        tkgrid.configure(SamplesNbr, column=1, row=i,sticky="w")
    }
    
    ReturnSamplesNbr <- returnValOnCancel
    
    onOK <- function() {
        ENV$ReturnSamplesNbr <- as.numeric(tclvalue(textEntryVarTcl))
        tkgrab.release(dlg)
        tkdestroy(dlg)
        
    }
    onCancel <- function() {
        ENV$ReturnSamplesNbr <- returnValOnCancel
        tkgrab.release(dlg)
        tkdestroy(dlg)
    }
    OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
    Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
    tkgrid(Cancel.but)
    tkgrid.configure(Cancel.but, column=0,row= (n_checked_GenProf+5), sticky="w")
    tkgrid(OK.but)
    tkgrid.configure(OK.but, column=0,row= (n_checked_GenProf+5), sticky="e")
    
    
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg)})
    tkbind(textEntryWidget, "<Return>", onOK)
    tkwait.window(dlg)
    
    return(ENV$ReturnSamplesNbr)
}