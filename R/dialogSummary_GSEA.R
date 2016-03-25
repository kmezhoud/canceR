#'Dialog Box to specify phenotype (variable) used in last GSEA-R to get Summary Results. This function ask the user to specify the phenotype (variable).
#' @usage dialogSummary_GSEA(Variable,returnValOnCancel ="ID_CANCEL")
#' 
#' @param Variable phenotype
#' @param returnValOnCancel  "ID_CANCEL"
#' 
#' @return variables
#' @export
#' @examples 
#' load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))
#' \dontrun{
#' #Run.GSEA()
#' #getSummaryGSEA()
#' }
dialogSummary_GSEA <- function(Variable,returnValOnCancel = "ID_CANCEL") {
    
    dlg <- tktoplevel()
    tkwm.deiconify(dlg)
    tkgrab.set(dlg)
    tkfocus(dlg)
    title="GSEA Summary Results"
    tkwm.title(dlg, title)
    
    tkgrid(tklabel(dlg, text = " "))  
    ###select GSEA summary Report of  Phenotype 1 
    tl1<-tklistbox(dlg,height=1, width= 50 ,background="white")
    #tklabel(ttDialogGSEA, text="dd")
    
    if(length(table(Variable))==2){
        Classes <- names(table(Variable))
    }
    if(names(table(Variable))[1]==""){
        Classes <- names(table(Variable))[-1]
    }
    
    getSummary_location1 <- function(Variable){
        tkfocus(dlg)
        if(exists("location1", envir = myGlobalEnv)){
            tkdelete(tl1,0,1)
        } 
        
        myGlobalEnv$location1 <- tclvalue(tkgetOpenFile(filetypes = "{{TXT Files} {.txt}} {{All files} *}", title="Select Summary Results file")) # Very simple, isn't it?
        
        if(length(grep(paste(".SUMMARY.RESULTS.REPORT.", Classes[1], sep=""), myGlobalEnv$location1))==0){
            tkmessageBox(message= paste("Select Summary Results Report of",names(table(myGlobalEnv$PhenoData[,myGlobalEnv$curselect])[1])), icon="warning")
            stop(paste("Select Summary Results Report of ",Classes[1]))
            tkfocus(dlg)
        } else
            tkinsert(tl1,"end",myGlobalEnv$location1)
        tkfocus(dlg)
    }
    
    location1.but <- tkbutton(dlg, text = "browse", command = getSummary_location1)
    
    tklabellocation1 <-tklabel(dlg,text=paste("Phenotype 1: ",Classes[1]))
    
    tkgrid(tklabellocation1,tl1,location1.but, columnspan=1)
    tkgrid.configure(tklabellocation1,rowspan=20, columnspan=1,sticky="nsw")
    
    
    ###select GSEA summary Report of  Phenotype 1 
    tl2<-tklistbox(dlg,height=1, width= 50 ,background="white")
    
    
    getSummary_location2 <- function(){
        tkfocus(dlg)
        if(exists("location2", envir = myGlobalEnv)){
            tkdelete(tl2,0,1)
        }
        myGlobalEnv$location2 <- tclvalue(tkgetOpenFile(filetypes = "{{TXT Files} {.txt}} {{All files} *}", title="Select Summary Results file")) # Very simple, isn't it?
        
        if(length(grep(paste(".SUMMARY.RESULTS.REPORT.", Classes[2], sep=""), myGlobalEnv$location2))==0){
            tkmessageBox(message= paste("Select Summary Results Report of",Classes[2]), icon="warning")
            stop(paste("Select Summary Results Report of ",Classes[2]))
            tkfocus(dlg)
        } else
            tkinsert(tl2,"end",myGlobalEnv$location2)
        tkfocus(dlg)
    }
    
    location2.but <- tkbutton(dlg, text = "browse", command = getSummary_location2)
    
    tklabellocation2 <-tklabel(dlg,text=paste("Phenotype 2: ",Classes[2]))
    
    tkgrid(tklabellocation2,tl2,location2.but, columnspan=1)
    tkgrid.configure(tklabellocation2,rowspan=20, columnspan=1,sticky="nsw")
    
    ###############
    entryWidth = 20
    entryInit <- "0.25"
    
    textEntryVarTcl <- tclVar(paste(entryInit))
    textEntryWidget <- tkentry(dlg, width = paste(entryWidth),
                               textvariable = textEntryVarTcl)
    tkgrid(tklabel(dlg, text = " "))
    tkgrid(tklabel(dlg, text = "Specify FDR threshold"), textEntryWidget)
    tkgrid(tklabel(dlg, text = "       "))
    ReturnVal <- returnValOnCancel
    
    onOK <- function() {
        myGlobalEnv$ReturnVal <- tclvalue(textEntryVarTcl)
        tkgrab.release(dlg)
        
        tkdestroy(dlg)
        
    }
    onCancel <- function() {
        myGlobalEn$vReturnVal <- returnValOnCancel
        tkgrab.release(dlg)
        tkdestroy(dlg)
        
    }
    OK.but <- tkbutton(dlg, text = "   OK   ", command = onOK)
    Cancel.but <- tkbutton(dlg, text = " Cancel ", command = onCancel)
    tkgrid(Cancel.but, OK.but)
    tkgrid(tklabel(dlg, text = "    "))
    
    tkwait.window(dlg)
    
    return(myGlobalEnv$ReturnVal)
    
}