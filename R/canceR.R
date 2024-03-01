ENV <- new.env(parent = emptyenv())
#' main function
#' @usage
#' canceR()
#'
#' @return open the starting windows with cancer studies
#' @export
#'
#' @examples
#' ENV <- new.env(parent = emptyenv())
#' \dontrun{
#' canceR()
#'}
#' @import tkrplot
#' @import tcltk
#' @import cBioPortalData
#' @import R.oo
#' 
#'@importFrom graphics axis image layout legend lines par plot points text
#'@importFrom stats as.formula cor density dist hclust median na.exclude p.adjust pnorm sd setNames window
#'@importFrom utils browseURL capture.output memory.limit read.table write.table read.delim
#'@importFrom grDevices colors dev.cur dev.off graphics.off jpeg pdf png rainbow dev.new savePlot
#'@importFrom R.methodsS3 setMethodS3
#'

canceR <- function(){
    
    #if (!require("cgdsr")) devtools::install_version("cgdsr",version="1.3")
    
    ## connect to cBioPortal API
    ENV$cgds <- cBioPortalData::cBioPortal(
        hostname = "www.cbioportal.org",
        protocol = "https",
        api = "/api/v2/api-docs"
    )

    ## Get all Cancer Studies using column 2 (description)
    ENV$Studies <- cBioPortalData::getStudies(ENV$cgds) 
    ENV$Studies_name <- ENV$Studies |> pull('name')
    
    ## first dialog START or CANCEL
    ENV$ttMain <- tktoplevel()
    tktitle(ENV$ttMain) <- "Search for Cancer Studies"
    #tkwm.resizable(ENV$ttMain, FALSE, TRUE)
    #tkwm.geometry(ENV$ttMain, "300x170")
    
    topMenu <- tkmenu(ENV$ttMain)           # Create a menu
    tkconfigure(ENV$ttMain, menu = topMenu) # Add it to the 'tt' window
    fileMenu <- tkmenu(topMenu, tearoff = FALSE)
    HelpMenu<- tkmenu(topMenu, tearoff = FALSE)
    
    ## TopMenus
    tkadd(topMenu, "cascade", label = "File", menu = fileMenu)
    tkadd(topMenu, "cascade", label = "Help", menu = HelpMenu)
    
    ## Second Menus
    # GeneMenu<- tkmenu(AnalysisMenu, tearoff = FALSE)
    
    quit <- function(){
        QuitMsg <- tkmessageBox(message="Are you sure want to quit?",
                                icon="question",type="yesnocancel",default="yes")   
        if ( tclvalue(QuitMsg )== "yes") {
            tkdestroy(ENV$ttMain)
            Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
            rm(list=ls(), envir=.GlobalEnv)
        }
        else {tkfocus(ENV$ttMain)}
    }
    
    
    tkadd(fileMenu, "command", label = "Set Workspace", command = function() setWorkspace())
    tkadd(fileMenu, "command", label = "Get Gene Exp", command = function() getGeneExpMatrix())
    tkadd(fileMenu, "command", label = "Get Clinical Data", command = function() getClinicalDataMatrix())
    tkadd(fileMenu, "command", label = "Quit", command = function() quit())
    tkadd(HelpMenu, "command", label= "Vignette", command= function() canceR_Vignette())
    tkadd(HelpMenu, "command", label= "Issue", command = function() canceR_Issue())
    tkadd(HelpMenu, "command", label = "About", command= function() about())
    
    
    
    loadAllStudies <- function(){
        #delete last search by key words
        if(exists("Matched_index", envir = ENV)){
            rm("Matched_index", envir=ENV)
            rm("matched_studies", envir = ENV)
        }
        
        ENV$n_studies <- nrow(ENV$Studies)
        ENV$Studies_name <- ENV$Studies$name
        
        nbrStudies <- paste("Query result: ", ENV$n_studies,
                            " Studies were loaded. Select one or more Studies to get Data Profiles.",
                            sep="")
        #tkgrid(tklabel(ENV$ttMain,text= nbrStudies ))
        ##Insert ListBox
        tkdelete(tlMain,0,600)
        tkdelete(tlInfo,0,1)
        for(s in ENV$Studies_name)
        {
            tkinsert(tlMain,"end", s )
        }
        # Default selection.  Indexing starts at zero.

        tkselection.set(tlMain,{30;86})  #:356, 30
        
        
        tkinsert(tlInfo,"end",text= nbrStudies )
        tkdelete(tlInfo,0)
    }
    
    loadAllStudies.button <- tkbutton(ENV$ttMain,
                                      text = "Load all Studies", 
                                      command = loadAllStudies)
    
    loadMatchStudies <- function(Word){
        #delete last load of all Studies
        if(exists("n_studies", envir = ENV)){
            rm("n_studies", envir=ENV)
            rm("Studies_name", envir=ENV)
        }
        # select raw with matched "string"
        ENV$Matched_index <- grep(Word, ENV$Studies$name, ignore.case=TRUE) 
        
        #ENV$matched_studies <- ENV$Studies[ENV$Matched_index, "name"] |> pull()
        ENV$matched_studies <- ENV$Studies$name[ENV$Matched_index]
        
        ##Count the nomber of Matched Studies and return the number.
       nbrMatchedStudies_msg <- paste("Query result: ",
                                   length(ENV$Matched_index), 
                                   " Studies were Matched. Select one or more to get its features.",
                                   sep="")
        #tkgrid(tklabel(ENV$ttMain,text= nbrMatchedStudies_msg ))
        tkdelete(tlMain,0,600)
        tkdelete(tlInfo,0,1)
        for( m in ENV$matched_studies){ 
            tkinsert(tlMain,"end", m)
        }
        tkselection.set(tlMain,2)  # Default selection.  Indexing starts at zero.
        
        tkinsert(tlInfo,"end",text= nbrMatchedStudies_msg)
        tkdelete(tlInfo,0)
    }
    
    launchDialog <- function() {
        Word <- modalDialog("Search Studies", "Search by Key Word", "")
        if (Word == "ID_CANCEL") return()
        loadMatchStudies(Word)
        
    }
    
    
    loadMatchStudies.button <- tkbutton(ENV$ttMain,
                                        text = "Search by key words", 
                                        command = launchDialog)
    
    tkgrid(loadAllStudies.button,loadMatchStudies.button, column=0)
    tkgrid.configure(loadAllStudies.button, sticky="w")
    tkgrid.configure(loadMatchStudies.button, sticky="e")
    
    
    
    tlInfo<-tklistbox(ENV$ttMain,height=1, width= 65, selectmode="multiple",
                      xscrollcommand=function(...)tkset(xscrInfo,...),
                      background="white", foreground="blue")
    
    xscrInfo <- tkscrollbar(ENV$ttMain, repeatinterval=2,orient="horizontal",
                            command=function(...)tkxview(tlInfo,...))
    
    tkgrid(tlInfo)
    
    ##Define ListBox of Studies
    tlMain<-tklistbox(ENV$ttMain,height=18, width= 65 ,selectmode="multiple",
                      xscrollcommand=function(...)tkset(xscr,...),
                      yscrollcommand=function(...)tkset(yscr,...), background="white")
    yscr <- tkscrollbar(ENV$ttMain, repeatinterval=2,
                        command=function(...)tkyview(tlMain,...))
    xscr <- tkscrollbar(ENV$ttMain, repeatinterval=2,orient="horizontal",
                        command=function(...)tkxview(tlMain,...))
    
    tkgrid(tlMain,yscr)
    tkgrid.configure(yscr,rowspan=20,sticky="nsw")
    tkgrid(xscr)
    tkgrid.configure(xscr,rowspan=1,sticky="ew")
    
    police <- tkfont.create(family="arial", size=11)
    tkconfigure(tlMain, foreground="black", font=police)
    
    ##listbox Info
    
    loadCasesGenProfs <- function(){ 
        if(exists("n_studies", envir = ENV)){
            ENV$StudyChoice <- ENV$Studies_name[as.numeric(tkcurselection(tlMain))+1]
            ENV$checked_StudyIndex <- as.numeric(tkcurselection(tlMain))+1
            
        } else if(exists("matched_studies", envir=ENV)){
            ENV$StudyChoice <- ENV$matched_studies[as.numeric(tkcurselection(tlMain))+1]
            ENV$checked_StudyIndex <- ENV$Matched_index[as.numeric(tkcurselection(tlMain))+1]
        }

        #Identify checked Studies and its Index in cgds
        ENV$checked_Studies_id <- ENV$Studies[ENV$checked_StudyIndex,"studyId"] |> pull()

        ###tkmessageBox if No Study was selected
        if(length(ENV$checked_Studies_id) ==0){
            msgStudies <- paste("You need to select at less one Study")
            tkmessageBox(title = "Greetings from R TclTk", message=msgStudies, icon = "Info" )
        } else {
            
            getCasesGenProfs()
            
        }
    }
    
    getCasesGenProfs.but <-tkbutton(ENV$ttMain,
                                    text="Get Cases and Genetic Profiles for selected Studies",
                                    command=loadCasesGenProfs)
    tkgrid(getCasesGenProfs.but)
    tkfocus(ENV$ttMain)
    
}

