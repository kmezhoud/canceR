#' get example of .gmt file from MSigDB (Broad Institute)
#' @usage getMSigDBExample()
#' @return path of GMT file
#' @export
#' @examples 
#'  readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#'  \dontrun{
#'  getMSigDBExample()
#'  }
getMSigDBExample <- function(){
    
    ifrm <- function(obj, env) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    ifrm(MSigDB, ENV)
    ifrm(MSigDBFile,ENV)
    ifrm(fname.MSigDB,ENV)
    
    
    police <- tkfont.create(family="arial", size=10)
    ttMSigDB <- tktoplevel()
    
    tktitle(ttMSigDB) <- paste("Select MSigDB")
    
    
    yscr1 <- tkscrollbar(ttMSigDB, repeatinterval=2,
                         command=function(...)tkyview(tl1,...))
    xscr1 <- tkscrollbar(ttMSigDB, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(tl1,...))
    
    xscr1Info <- tkscrollbar(ttMSigDB, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tl1info,...))
    
    
    tl1<-tklistbox(ttMSigDB,height=10, width= 30 ,selectmode="single",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")
    tl1info<-tklistbox(ttMSigDB,height=1, width= 30,selectmode="single",xscrollcommand=function(...)tkset(xscr1Info,...),background="white")
    tkconfigure(tl1info, foreground="blue", font=police)
    
    
    loadMSigDB <- function()
    {  
        curselectMSigDB <- as.numeric(tkcurselection(tl1))+1
        lcurselectMSigDB <- length(curselectMSigDB)
        
        fname.MSigDB <-  list.files(paste(path.package("canceR"),"/extdata/MSigDB", sep=""))[curselectMSigDB]
        ENV$fname.GMT <- paste(path.package("canceR"),"/extdata/MSigDB/", fname.MSigDB,sep="")
        
        if (lcurselectMSigDB ==0){
            msgSelectMSigDB<-"Select MSigBD"
            tkmessageBox(message=msgSelectMSigDB)
        } else {
            tkdelete(tl1info,0,1)
            tkinsert(tl1info,"end",fname.MSigDB)
            tkinsert(ENV$tlGMT,"end",ENV$fname.GMT)
            tkfocus(ENV$ttDialogGSEA)
            tkdestroy(ttMSigDB)
        }
        
    }
    
    
    Select.but <-tkbutton(ttMSigDB,text="select",command=loadMSigDB)
    
    
    tkgrid(tl1,yscr1)
    tkgrid.configure(yscr1,rowspan=20, columnspan=1,sticky="nsw")
    tkgrid(xscr1)
    tkgrid.configure(xscr1,rowspan=1,sticky="we")
    tkgrid(tl1info, columnspan=1)
    tkgrid(xscr1Info)
    tkgrid.configure(xscr1Info,rowspan=4,sticky="we")
    tkgrid(Select.but)
    
    for (i in 1:length(list.files(paste(path.package("canceR"),"/extdata/MSigDB", sep="")))){
        
        tkinsert(tl1,"end",list.files(paste(path.package("canceR"),"/extdata/MSigDB", sep=""))[i])
    }
}