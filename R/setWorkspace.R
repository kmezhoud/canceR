#' Setting work Directory and output folders.At starting window, user needs to set work directory for output data. The function is foud in File menu.
#' @usage setWorkspace()
#' @return paths of output files
#' @export
#' @examples 
#' load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
#' \dontrun{
#' setWorkspace()
#' }
#' 
setWorkspace <- function(){
    
    
    ttWorkspace<-tktoplevel()
    tktitle(ttWorkspace) <- "Setting Workspace"
    
    
    ###select GSEA program file
    tlWorkspace<-tklistbox(ttWorkspace,height=1, width= 50 ,background="white")
    
    
    
    getWorkspace <- function(){
        tkfocus(ttWorkspace)
        if(exists("path_workspace", envir = myGlobalEnv)){
            tkdelete(tlWorkspace,0,1)
        }
        myGlobalEnv$path_workspace <- tk_choose.dir()
        
        if(!file.exists(myGlobalEnv$path_workspace)){
            Sys.chmod(dirname(myGlobalEnv$path_workspace), mode = "0777", use_umask = TRUE)
            dir.create(file.path(myGlobalEnv$path_workspace), showWarnings = FALSE)
            
        }
        
        
        tkinsert(tlWorkspace,"end",myGlobalEnv$path_workspace)
        tkfocus(ttWorkspace)
    }
    
    workspace.but <- tkbutton(ttWorkspace, text = "Browse", command = getWorkspace)
    
    tklabelWorkspace <-tklabel(ttWorkspace,text="Workspace")
    
    tkgrid(tklabelWorkspace,tlWorkspace,workspace.but, columnspan=1)
    tkgrid.configure(tklabelWorkspace,rowspan=20, columnspan=1,sticky="nsw")
    
    
    ### Set Results Folder
    
    tlResults<-tklistbox(ttWorkspace,height=1, width= 50 ,background="white")
    getResultsFolder <- function(){
        if(exists("pathResultsFolder")){
            tkdelete(tlResults,0,1)
        }
        pathResultsFolder <- paste(myGlobalEnv$path_workspace, "/Results/", sep="")
        
        if(!file.exists(pathResultsFolder)){
            Sys.chmod(dirname(myGlobalEnv$path_workspace), mode = "0777", use_umask = TRUE)
            dir.create(file.path(pathResultsFolder), showWarnings = FALSE)
            dir.create(file.path(paste(pathResultsFolder,"ProfileData",sep="")), showWarnings = FALSE)
            dir.create(file.path(paste(pathResultsFolder,"GSEAlm",sep="")), showWarnings = FALSE)
            dir.create(file.path(paste(pathResultsFolder,"Classifier",sep="")), showWarnings = FALSE)
            dir.create(file.path(paste(pathResultsFolder,"GSEA",sep="")), showWarnings = FALSE)
        }
        tkinsert(tlResults,"end",pathResultsFolder)
        tkfocus(ttWorkspace)
    }
    
    getResultsFolder.but <- tkbutton(ttWorkspace, text = "Set", command = getResultsFolder)
    
    tklabelResults <- tklabel(ttWorkspace,text="Results")
    
    tkgrid(tklabelResults,tlResults,getResultsFolder.but, columnspan=1)
    tkgrid.configure(tklabelResults,rowspan=20, columnspan=1,sticky="nsw")
    
    
    
    ### Set MSigDB folder
    tlMSigDB<-tklistbox(ttWorkspace,height=1, width= 50 ,background="white")
    
    getMSigDBFolder <- function(){
        if(exists("pathMSigDBFolder")){
            tkdelete(tlMSigDB,0,1)
        }
        
        pathMSigDBFolder <- paste(myGlobalEnv$path_workspace, "/Results/MSigDB/", sep="")
        
        
        if(!file.exists(pathMSigDBFolder)){
            Sys.chmod(dirname(myGlobalEnv$path_workspace), mode = "0777", use_umask = TRUE)
            dir.create(file.path(pathMSigDBFolder), showWarnings = FALSE)
            
        }
        
        
        tkinsert(tlMSigDB,"end",pathMSigDBFolder)
        tkfocus(ttWorkspace)
    }
    
    getMSigDBFolder.but <- tkbutton(ttWorkspace, text = "Set", command = getMSigDBFolder)
   
    tklabelMSigDB <-tklabel(ttWorkspace,text="MSigDB")
    
    tkgrid(tklabelMSigDB,tlMSigDB,getMSigDBFolder.but, columnspan=1)
    tkgrid.configure(tklabelMSigDB,rowspan=20, columnspan=1,sticky="nsw")
    
    
    ### Set gct_cls Folder
    
    tlgct_cls<-tklistbox(ttWorkspace,height=1, width= 50 ,background="white")
    
    getgct_clsFolder <- function(){
        if(exists("pathgct_clsFolder")){
            tkdelete(tlgct_cls,0,1)
        }
        
        pathgct_clsFolder <- paste(myGlobalEnv$path_workspace, "/Results/gct_cls/", sep="")
        
        
        if(!file.exists(pathgct_clsFolder)){
            Sys.chmod(dirname(myGlobalEnv$path_workspace), mode = "0777", use_umask = TRUE)
            dir.create(file.path(pathgct_clsFolder), showWarnings = FALSE)
            
        }
        tkinsert(tlgct_cls,"end",pathgct_clsFolder)
    }
    
    getgct_clsFolder.but <- tkbutton(ttWorkspace, text = "Set", command = getgct_clsFolder)
    
    tklabelgct_cls <- tklabel(ttWorkspace,text="gct,cls files")
    
    tkgrid(tklabelgct_cls,tlgct_cls,getgct_clsFolder.but, columnspan=1)
    tkgrid.configure(tklabelgct_cls,rowspan=20, columnspan=1,sticky="nsw")
    
   
    ##
    OK <- function(){
        
        setwd(myGlobalEnv$path_workspace)
        tkdestroy(ttWorkspace)
    }
    
    Ok.but <- tkbutton(ttWorkspace, text = " OK ", command = OK)
    
    Cancel <- function(){
        tkdestroy(ttWorkspace)
        
    }
    Cancel.but <- tkbutton(ttWorkspace, text= "Cancel", command = Cancel)
    tkgrid(Cancel.but, Ok.but)
    tkgrid.configure(Cancel.but, sticky="n",columnspan=2)
    tkgrid.configure(Ok.but, sticky="n",columnspan=2)
    
    tkwait.window(ttWorkspace)
    
}