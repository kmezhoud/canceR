##set myGlobalEnv

myGlobalEnv <- new.env(parent = emptyenv())

canceR <- function(){
    
    ## Create project
    cgds<-CGDS("http://www.cbioportal.org/public-portal/")
    myGlobalEnv$cgds <- cgds
    ## Get all Cancer Studies using column 2 (description)
    Studies <- getCancerStudies(cgds)[,2]
    myGlobalEnv$Studies <- Studies
    
    ## first dialog START or CANCEL
    myGlobalEnv$ttMain <- tktoplevel()
    tktitle(myGlobalEnv$ttMain) <- "Search for Cancer Studies"
    #tkwm.resizable(myGlobalEnv$ttMain, FALSE, TRUE)
    #tkwm.geometry(myGlobalEnv$ttMain, "300x170")
    
    topMenu <- tkmenu(myGlobalEnv$ttMain)           # Create a menu
    tkconfigure(myGlobalEnv$ttMain, menu = topMenu) # Add it to the 'tt' window
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)
    HelpMenu<- tkmenu(topMenu, tearoff = FALSE)
    
    ## TopMenus
    tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
    tkadd(topMenu, "cascade", label = "Help", menu = HelpMenu)
    
    ## Second Menus
    # GeneMenu<- tkmenu(AnalysisMenu, tearoff = FALSE)
    
    quit <- function(){
        QuitMsg <- tkmessageBox(message="Are you sure want to quit?",icon="question",type="yesnocancel",default="yes")   
        if ( tclvalue(QuitMsg )== "yes") {
            tkdestroy(myGlobalEnv$ttMain)
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            rm(list=ls(), envir=.GlobalEnv)
        }
        else {tkfocus(myGlobalEnv$ttMain)}
    }
    
    
    tkadd(fileMenu, "command", label = "Set Workspace", command = function() setWorkspace())
    tkadd(fileMenu, "command", label = "Get Gene Exp", command = function() getGeneExpMatrix())
    tkadd(fileMenu, "command", label = "Get Clinical Data", command = function() getClinicalDataMatrix())
    tkadd(fileMenu, "command", label = "Quit", command = function() quit())
    tkadd(HelpMenu, "command", label="Manual", command= function()canceR_Vignette())
    tkadd(HelpMenu, "command", label= "Help", command = function() canceRHelp())
    tkadd(HelpMenu, "command", label = "About", command= function() about())
    
    
    
    loadAllStudies <- function(){
        #delete last search by key words
        if(exists("Li", envir = myGlobalEnv)){
            #Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            
            rm(Li, envir=myGlobalEnv)
        }
        
        A <- length(Studies)
        myGlobalEnv$A <- A
        
        nbrStudies <- paste("Query result: ",A, " Studies were loaded. Select one or more Studies to get Data Profiles.",sep="")
        #tkgrid(tklabel(myGlobalEnv$ttMain,text= nbrStudies ))
        ##Insert ListBox
        tkdelete(tlMain,0,150)
        tkdelete(tlInfo,0,1)
        for (i in (1:A))
        {
            tkinsert(tlMain,"end",Studies[i])
        }
        # Default selection.  Indexing starts at zero.
        tkselection.set(tlMain,2) 
        
        
        tkinsert(tlInfo,"end",text= nbrStudies )
        tkdelete(tlInfo,0)
    }
    
    loadAllStudies.button <- tkbutton(myGlobalEnv$ttMain, text = "Load all Studies", command = loadAllStudies)
    
    loadMatchStudies <- function(Word){
        #delete last load of all Studies
        if(exists("A", envir = myGlobalEnv)){
            #Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)  
            rm(A, envir=myGlobalEnv)
        }
        # select raw with matched "string"
        StudyIndex <- grep(Word, Studies, ignore.case=TRUE) 
        myGlobalEnv$StudyIndex <- StudyIndex
        
        match_Study_All <- getCancerStudies(myGlobalEnv$cgds)[myGlobalEnv$StudyIndex, 2]
        myGlobalEnv$match_Study_All <- match_Study_All
        
        ##Count the nomber of Matched Studies and return the number.
        Li <- length(StudyIndex) 
        myGlobalEnv$Li <- Li
        nbrMatchedStudies <- paste("Query result: ",Li, " Studies were Matched. Select one or more to get its features.",sep="")
        #tkgrid(tklabel(myGlobalEnv$ttMain,text= nbrMatchedStudies ))
        tkdelete(tlMain,0,150)
        tkdelete(tlInfo,0,1)
        for (i in (1:Li))
        { 
            tkinsert(tlMain,"end",match_Study_All[i])
        }
        tkselection.set(tlMain,2)  # Default selection.  Indexing starts at zero.
        
        tkinsert(tlInfo,"end",text= nbrMatchedStudies)
        tkdelete(tlInfo,0)
    }
    
    
    
    launchDialog <- function() {
        Word <- modalDialog("Search Studies", "Search by Key Word", "")
        if (Word == "ID_CANCEL") return()
        loadMatchStudies(Word)
        
    }
    
    
    loadMatchStudies.button <- tkbutton(myGlobalEnv$ttMain, text = "Search by key words", command = launchDialog)
    
    
    
    tkgrid(loadAllStudies.button,loadMatchStudies.button, column=0)
    tkgrid.configure(loadAllStudies.button, sticky="w")
    tkgrid.configure(loadMatchStudies.button, sticky="e")
    
    
    
    tlInfo<-tklistbox(myGlobalEnv$ttMain,height=1, width= 65 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscrInfo,...),background="white", foreground="blue")
    
    xscrInfo <- tkscrollbar(myGlobalEnv$ttMain, repeatinterval=2,orient="horizontal",
                            command=function(...)tkxview(tlInfo,...))
    
    tkgrid(tlInfo)
    
    ##Define ListBox of Studies
    tlMain<-tklistbox(myGlobalEnv$ttMain,height=18, width= 65 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...),background="white")
    yscr <- tkscrollbar(myGlobalEnv$ttMain, repeatinterval=2,
                        command=function(...)tkyview(tlMain,...))
    xscr <- tkscrollbar(myGlobalEnv$ttMain, repeatinterval=2,orient="horizontal",
                        command=function(...)tkxview(tlMain,...))
    
    tkgrid(tlMain,yscr)
    tkgrid.configure(yscr,rowspan=20,sticky="nsw")
    tkgrid(xscr)
    tkgrid.configure(xscr,rowspan=1,sticky="ew")
    
    police <- tkfont.create(family="arial", size=11)
    tkconfigure(tlMain, foreground="black", font=police)
    
    ##listbox Info
    
    loadCasesGenProfs <- function(){ 
        if(exists("A", envir = myGlobalEnv)){
            StudyChoice <- Studies[as.numeric(tkcurselection(tlMain))+1]
            myGlobalEnv$StudyChoice <- StudyChoice
            checked_StudyIndex <- as.numeric(tkcurselection(tlMain))+1
            myGlobalEnv$checked_StudyIndex <- checked_StudyIndex
            
        } else if (exists("Li", envir=myGlobalEnv)){
            StudyChoice <- myGlobalEnv$match_Study_All[as.numeric(tkcurselection(tlMain))+1]
            myGlobalEnv$StudyChoice <- StudyChoice
            checked_StudyIndex <- myGlobalEnv$StudyIndex[as.numeric(tkcurselection(tlMain))+1]
            myGlobalEnv$checked_StudyIndex <- checked_StudyIndex
        }
        
        #Identify checked Studies and its Index in cgds
        checked_Studies <- getCancerStudies(myGlobalEnv$cgds)[checked_StudyIndex,1] 
        myGlobalEnv$checked_Studies <- checked_Studies
        ###tkmessageBox if No Study was selected
        if(length(myGlobalEnv$checked_Studies) ==0){
            msgStudies <- paste("You need to select at less one Study")
            tkmessageBox(title = "Greetings from R TclTk",message=msgStudies,icon = "Info" )
        } else {
            
            getCasesGenProfs()
            
        }
    }
    
    getCasesGenProfs.but <-tkbutton(myGlobalEnv$ttMain,text="Get Cases and Genetic Profiles for selected Studies",command=loadCasesGenProfs)
    tkgrid(getCasesGenProfs.but)
    tkfocus(myGlobalEnv$ttMain)
    
}

