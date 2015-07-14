dialoggetGeneListMSigDB <- function(MSigDB){   

    if(exists("match_GS", envir = myGlobalEnv)){
        rm("match_GS", envir=myGlobalEnv)
    }
    
## function load matched Gene Sets   
loadMatchGS <- function(Word){
        
    
    # select row with matched "string"
    GSidx <- grep(Word, names(MSigDB), ignore.case=TRUE) 
    
    myGlobalEnv$match_GS <- names(MSigDB)[GSidx]
    
    
    ##Count the nomber of Matched Studies and return the number.
    nMatchGS <- paste("Query result: ",length(myGlobalEnv$match_GS), " Gene Sets were Matched.",sep="")
    #tkgrid(tklabel(ttGeneListMSigDB,text= nMatchGS ))
    tkdelete(tlInfo,0,1)
    tkinsert(tlInfo,"end",nMatchGS)
    
    tkdelete(tl1,0,1500)
    #tkdelete(tl1info,0,1)
    for (i in (1:length(myGlobalEnv$match_GS)))
    { 
        tkinsert(tl1,"end",myGlobalEnv$match_GS[i])
    }
    tkselection.set(tl1,2)  # Default selection.  Indexing starts at zero.

    #tkdelete(tl1info,0)
}



launchDialog <- function() {
    Word <- modalDialog("Search Gene Sets", "Search by Key Word", "")
    if (Word == "ID_CANCEL") return()
    loadMatchGS(Word)
    
}



    
    
###############################
## define font for selected variables
police <- tkfont.create(family="arial", size=10)

ttGeneListMSigDB <- tktoplevel()

tktitle(ttGeneListMSigDB) <- paste("Get Gene Set")

loadMatchGS.button <- tkbutton(ttGeneListMSigDB, text = "Search by key words", command = launchDialog)
tlInfo<-tklistbox(ttGeneListMSigDB,height=1, width= 40,selectmode="single",background="white")

tkgrid(tlInfo,loadMatchGS.button)
tkgrid.configure(tlInfo, column=0, sticky="w")
tkgrid.configure(loadMatchGS.button, column=0, sticky="e")
##LABELS 
label1 <- tklabel(ttGeneListMSigDB, text= "Select Gene Sets")

yscr1 <- tkscrollbar(ttGeneListMSigDB, repeatinterval=2,
                     command=function(...)tkyview(tl1,...))
xscr1 <- tkscrollbar(ttGeneListMSigDB, repeatinterval=2,orient="horizontal",
                     command=function(...)tkxview(tl1,...))

yscr1Info <- tkscrollbar(ttGeneListMSigDB, repeatinterval=2,orient="vertical",
                         command=function(...)tkxview(tl1info,...))
xscr1Info <- tkscrollbar(ttGeneListMSigDB, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(tl1info,...))


tl1<-tklistbox(ttGeneListMSigDB,height=10, width= 60 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")
tl1info<-tklistbox(ttGeneListMSigDB,height=5, width= 80,selectmode="multiple",xscrollcommand=function(...)tkset(xscr1Info,...),background="white")
tkconfigure(tl1info, foreground="blue", font=police)

### function load Gene Sets
myGlobalEnv$regex <-0
loadGeneSets <- function()
{  

    if(!exists("match_GS", envir=myGlobalEnv)){
    #tkdelete(tl1info,0,1)
    curselectGS <- as.numeric(tkcurselection(tl1))+1
    tkinsert(tl1info,"end",names(MSigDB)[curselectGS]) 
    myGlobalEnv$regex <- c(myGlobalEnv$regex,names(MSigDB)[curselectGS])
    }else{
        curselectGS <- as.numeric(tkcurselection(tl1))+1
        tkinsert(tl1info,"end",myGlobalEnv$match_GS[curselectGS]) 
        myGlobalEnv$regex <- c(myGlobalEnv$regex,myGlobalEnv$match_GS[curselectGS])
        
    }
    
}
myGlobalEnv$regex <- myGlobalEnv$regex[-1]

OKon <- function(){
    tkdestroy(ttGeneListMSigDB)
    
}
OKOn.but <- tkbutton(ttGeneListMSigDB, text="OK", command=OKon)
loadGS.but <-tkbutton(ttGeneListMSigDB,text="select",command=loadGeneSets)
tkgrid(label1)
tkgrid(tl1,yscr1)
tkgrid.configure(yscr1,rowspan=20, columnspan=1,sticky="nsw")
tkgrid(xscr1)
tkgrid.configure(xscr1,rowspan=2, column=0,sticky="we")
tkgrid( tl1info, yscr1Info)
tkgrid.configure(yscr1Info,rowspan=20, columnspan=1,sticky="nsw")
tkgrid(xscr1Info)
tkgrid.configure(xscr1Info,rowspan=4, column=0,sticky="we")
tkgrid(loadGS.but,OKOn.but)
tkgrid.configure(loadGS.but,column=0, sticky="nw")
tkgrid.configure(OKOn.but,column=0, sticky="ne")
for (i in 1:length(names(MSigDB))){
    
    tkinsert(tl1,"end",names(MSigDB)[i])
}

tkwait.window(ttGeneListMSigDB)
}