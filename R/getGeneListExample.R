#' get Gene List from examples. User can select one from available gene list
#' @usage getGeneListExample()
#' @return Gene list path of file
#' @export
#' @examples
#' myGlobalEnv <- new.env(parent = emptyenv())
#' \dontrun{
#' getGeneListExample() 
#' }
getGeneListExample <- function(){
    
    
    ifrm <- function(obj, env = globalenv()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    ifrm(myGlobalEnv$GeneList)
    ifrm(myGlobalEnv$GeneListfile)
    ifrm(myGlobalEnv$fname.GeneList)
    
    
    police <- tkfont.create(family="arial", size=10)
    ttGeneList <- tktoplevel()
    
    
    tktitle(ttGeneList) <- paste("Select Gene list")
    
    
    # Take LABELS in ttGeneList
    label1 <- tklabel(ttGeneList, text= "Survival\n Event(Living/Deceased) \n and Time (Months)")
    
    
    yscr1 <- tkscrollbar(ttGeneList, repeatinterval=2,
                         command=function(...)tkyview(tl1,...))
    xscr1 <- tkscrollbar(ttGeneList, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(tl1,...))
    
    xscr1Info <- tkscrollbar(ttGeneList, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tl1info,...))
    
    
    tl1<-tklistbox(ttGeneList,height=10, width= 30 ,selectmode="single",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")
    tl1info<-tklistbox(ttGeneList,height=1, width= 30,selectmode="single",xscrollcommand=function(...)tkset(xscr1Info,...),background="white")
    tkconfigure(tl1info, foreground="blue", font=police)
    
    
    loadGeneList <- function()
    {  
        curselectGeneList <- as.numeric(tkcurselection(tl1))+1
        lcurselectGeneList <- length(curselectGeneList)
        
        fname.GeneList <-  list.files(paste(path.package("canceR"),"/extdata/GeneList", sep=""))[curselectGeneList]
        myGlobalEnv$GeneListfile <- paste(path.package("canceR"),"/extdata/GeneList/", fname.GeneList,sep="")
        if (lcurselectGeneList ==0){
            msgSelectGeneList<-"Select Gene list"
            tkmessageBox(message=msgSelectGeneList)
        } else {
            tkdelete(tl1info,0,1)
            tkinsert(tl1info,"end",fname.GeneList)
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            GeneList<-unique(read.table(myGlobalEnv$GeneListfile))
            myGlobalEnv$GeneList <- t(GeneList)
            tkmessageBox(message = paste("The file selected is", basename(fname.GeneList),"with", length(t(GeneList))," genes"),icon="info")
            tkfocus(ttGeneList)
            tkdestroy(ttGeneList)
        }
        
    }
    
    
    Select.but <-tkbutton(ttGeneList,text="select",command=loadGeneList)
    
    
    tkgrid(tl1,yscr1)
    tkgrid.configure(yscr1,rowspan=20, columnspan=1,sticky="nsw")
    tkgrid(xscr1)
    tkgrid.configure(xscr1,rowspan=1,sticky="we")
    tkgrid(tl1info, columnspan=1)
    tkgrid(xscr1Info)
    tkgrid.configure(xscr1Info,rowspan=4,sticky="we")
    tkgrid(Select.but)
    
    for (i in 1:length(list.files(paste(path.package("canceR"),"/extdata/GeneList", sep="")))){
        
        tkinsert(tl1,"end",list.files(paste(path.package("canceR"),"/extdata/GeneList", sep=""))[i])
    }
}