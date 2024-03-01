#' Dialog box to specify Gene Symbol.
#' @usage modalDialog(title, question, entryInit, entryWidth = 40,returnValOnCancel = "ID_CANCEL")
#' @param title string
#' @param question string
#' @param entryInit entryInit
#' @param entryWidth 40
#' @param returnValOnCancel "ID_CANCEL"
#' 
#' @return dialog box
#' @export
#' @examples 
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' ## Select Case from Breast Cancer
#' ENV <- new.env(parent = emptyenv())
#' ENV$curselectCases <- 9
#' ##Select Genetic Profile from Breast Cancer
#' ENV$curselectGenProfs <- 4
#' ## get Specific Mutation data for 73 Genes list
#' \dontrun{
#' getProfilesDataSingleGene()
#' }
modalDialog <- function(title, question, entryInit, entryWidth = 40,
                        returnValOnCancel = "ID_CANCEL") {
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    tkwm.title(dlg, title)
    
    textEntryVarTcl <- tclVar(paste(entryInit))
    textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
                               textvariable = textEntryVarTcl)
    tkgrid(tklabel(dlg, text = "       "))
    tkgrid(tklabel(dlg, text = question), textEntryWidget)
    tkgrid(tklabel(dlg, text = "       "))
    ReturnVal <- returnValOnCancel
    
    onOK <- function() {
        ENV$ReturnVal <- tclvalue(textEntryVarTcl)
        tkgrab.release(dlg)
        tkdestroy(dlg)
        #tkfocus(ttMain)
    }
    onCancel <- function() {
        ENV$ReturnVal <- returnValOnCancel
        tkgrab.release(dlg)
        tkdestroy(dlg)
        #tkfocus(ttMain)
    }
    OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
    Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
    tkgrid(Cancel.but,OK.but)
    tkgrid(tklabel(dlg, text = "    "))
    
    tkfocus(dlg)
    tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg); tkfocus(ENV$ttMain)})
    tkbind(textEntryWidget, "<Return>", onOK)
    tkwait.window(dlg)
    
    return(ENV$ReturnVal)
}