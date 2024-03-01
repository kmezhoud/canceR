#' Checkbox to select variables from clinical data
#' @usage dialogOptionPhenoTest(eSet)
#' @param eSet Expression Set
#' 
#' @return vectors: variables to test Survival status, AGE, p-value 
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/prad_michPhenoTest1021.rds", sep=""))
#' \dontrun{
#' dialogOptionPhenoTest(ENV$eSet)
#' }
#' 
dialogOptionPhenoTest <- function(eSet){
    #require(tcltk)
    
    ifrm <- function(obj, env = globalenv()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    ifrm(EventTime)
    ifrm(Category)
    ifrm(Continu)
    
    ## define font for selected variables
    police <- tkfont.create(family="arial", size=10)
    
    
    ENV$ttPheno <- tktoplevel()
    #tkwm.geometry(ENV$ttPheno,"180x250")
    
    tktitle(ENV$ttPheno) <- paste("Associate between a list of variable and the gene expression")
    
    
    # Take LABELS in ENV$ttPheno
    label1 <- tklabel(ENV$ttPheno, text= "Survival\n Event(Living/Deceased) \n and Time (Months)")
    label2 <- tklabel(ENV$ttPheno,text="Categorical \n select non numeric \n variable")
    label3 <- tklabel(ENV$ttPheno, text= "Continuous \n select numeric variable")
    label4 <- tklabel(ENV$ttPheno,text="p.adjust.method")
    #tkgrid(tklabel0,tklabel1, columnspan=2, pady = 10)
    
    
    yscr1 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,
                         command=function(...)tkyview(tl1,...))
    xscr1 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(tl1,...))
    
    xscr1Info <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tl1info,...))
    
    yscr2 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,
                         command=function(...)tkyview(tl2,...))
    xscr2 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(tl2,...))
    xscr2Info <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tl2info,...))
    
    yscr3 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,
                         command=function(...)tkyview(tl3,...))
    xscr3 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(tl3,...))
    xscr3Info <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tl3info,...))
    yscr4 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,
                         command=function(...)tkyview(tl4,...))
    xscr4 <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(tl4,...))
    xscr4Info <- tkscrollbar(ENV$ttPheno, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tl4info,...))  
    
    
    tl1<-tklistbox(ENV$ttPheno,height=5, width= 20 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")
    tl1info<-tklistbox(ENV$ttPheno,height=1, width= 20,selectmode="single",xscrollcommand=function(...)tkset(xscr1Info,...),background="white")
    tkconfigure(tl1info, foreground="blue", font=police)
    
    tl2<-tklistbox(ENV$ttPheno,height=5, width= 20 ,selectmode="single",xscrollcommand=function(...)tkset(xscr2,...),yscrollcommand=function(...)tkset(yscr2,...),background="white")
    tl2info<-tklistbox(ENV$ttPheno,height=1, width= 20,selectmode="single",xscrollcommand=function(...)tkset(xscr2Info,...),background="white")
    tkconfigure(tl2info, foreground="blue", font=police)
    
    tl3<-tklistbox(ENV$ttPheno,height=5, width= 20 ,selectmode="single",xscrollcommand=function(...)tkset(xscr3,...),yscrollcommand=function(...)tkset(yscr3,...),background="white")
    tl3info<-tklistbox(ENV$ttPheno,height=1, width= 20,selectmode="single",xscrollcommand=function(...)tkset(xscr3Info,...),background="white")
    tkconfigure(tl3info, foreground="blue", font=police)
    
    tl4<-tklistbox(ENV$ttPheno,height=5, width= 20 ,selectmode="single",xscrollcommand=function(...)tkset(xscr4,...),yscrollcommand=function(...)tkset(yscr4,...),background="white")
    tl4info<-tklistbox(ENV$ttPheno,height=1, width= 20,selectmode="single",xscrollcommand=function(...)tkset(xscr4Info,...),background="white")
    tkconfigure(tl4info, foreground="blue", font=police)
    
    
    loadSurvival <- function()
    {  
        curselectSurvival <- as.numeric(tkcurselection(tl1))+1
        lcurselectSurvival <- length(curselectSurvival)
        
        ENV$EventTime <-  names(Biobase::pData(eSet))[curselectSurvival]
        if (lcurselectSurvival !=2){
            msgSelectSurvival<-"Select two variables for Survival \n Event(OS_STATUT) \n Time(OS_MONTHS"
            tkmessageBox(message=msgSelectSurvival)
        } else {
            tkdelete(tl1info,0,1)
            tkinsert(tl1info,"end",ENV$EventTime)
            tkfocus()
        }
        
    }
    
    
    Survival.but <-tkbutton(ENV$ttPheno,text="select",command=loadSurvival)
    
    tkgrid(label1,tl1,yscr1)
    tkgrid.configure(yscr1,rowspan=20, columnspan=2,sticky="nsw")
    tkgrid(xscr1)
    tkgrid.configure(xscr1,rowspan=2, column=1,sticky="we")
    tkgrid(Survival.but, tl1info, columnspan=1)
    tkgrid(xscr1Info)
    tkgrid.configure(xscr1Info,rowspan=4, column=1,sticky="we")
    
    for (i in 1:length(names(Biobase::pData(eSet)))){
        
        tkinsert(tl1,"end",names(Biobase::pData(eSet))[i])
    }
    
    ###################################
    
    loadCategorical <- function()
    {  
        curselectCategorical <- as.numeric(tkcurselection(tl2))+1
        lcurselectCategorical <- length(curselectCategorical)
        
        ENV$Category <-  names(Biobase::pData(eSet))[curselectCategorical]
        
        if (lcurselectCategorical ==0){
            msgSelectCategorical<-"Select one variable for Category \n not numeric"
            tkmessageBox(message=msgSelectCategorical)
            tkfocus(ENV$ttPheno)
        }else if (all(is.na(Biobase::pData(eSet)[curselectCategorical] ))){
            msgNoData <- paste("No data are avaible for ",Category)
            tkmessageBox(message=msgNoData, icon="info")
        } else if (is.numeric(Biobase::pData(eSet)[curselectCategorical][1,])==TRUE){
            msgBadCategory<-"Select non numeric Variable"
            tkmessageBox(message=msgBadCategory)
        } else {
            tkdelete(tl2info,0,1)
            tkinsert(tl2info,"end",ENV$Category)
            tkfocus()
            ##Remove NA from selected variables 
            #Biobase::pData(eSet)[curselectCategorical] <-as.matrix(na.omit(Biobase::pData(eSet)[curselectCategorical]))
        }
        
    }
    
    
    Categorical.but <-tkbutton(ENV$ttPheno,text="select",command=loadCategorical)
    
    tkgrid(label2,tl2,yscr2)
    tkgrid.configure(yscr2,rowspan=20, columnspan=2,sticky="nsw")
    tkgrid(xscr2)
    tkgrid.configure(xscr2,rowspan=2, column=1,sticky="we")
    tkgrid(Categorical.but, tl2info, columnspan=1)
    tkgrid(xscr2Info)
    tkgrid.configure(xscr2Info,rowspan=4, column=1,sticky="we")
    
    for (i in 1:length(names(Biobase::pData(eSet)))){
        
        tkinsert(tl2,"end",names(Biobase::pData(eSet))[i])
    }
    
    ###################################
    
    loadContinuous <- function()
    {  
        curselectContinuous <- as.numeric(tkcurselection(tl3))+1
        lcurselectContinuous <- length(curselectContinuous)
        
        ENV$Continu <-  names(Biobase::pData(eSet))[curselectContinuous]
        if (lcurselectContinuous ==0){
            msgSelectContinuous<-"Select one or more Continuous variables \n  numeric"
            tkmessageBox(message=msgSelectContinuous)
        }
        if (is.character(Biobase::pData(eSet)[curselectContinuous][1,])==TRUE|is.factor(Biobase::pData(eSet)[curselectContinuous][1,])==TRUE){
            msgBadContinuous<-"Select non character/factor Variable"
            tkmessageBox(message=msgBadContinuous)
        } else {
            tkdelete(tl3info,0,1)
            tkinsert(tl3info,"end",ENV$Continu)
            tkfocus()
            ##Remove NA from selected variables 
            #Biobase::pData(eSet)[curselectContinuous] <-as.matrix(na.omit(Biobase::pData(eSet)[curselectContinuous]))
        }
        
    }
    
    
    Continuous.but <-tkbutton(ENV$ttPheno,text="select",command=loadContinuous)
    
    tkgrid(label3,tl3,yscr3)
    tkgrid.configure(yscr3,rowspan=20, columnspan=2,sticky="nsw")
    tkgrid(xscr3)
    tkgrid.configure(xscr3,rowspan=2, column=1,sticky="we")
    tkgrid(Continuous.but, tl3info, columnspan=1)
    tkgrid(xscr3Info)
    tkgrid.configure(xscr3Info,rowspan=4, column=1,sticky="we")
    
    for (i in 1:length(names(Biobase::pData(eSet)))){
        
        tkinsert(tl3,"end",names(Biobase::pData(eSet))[i])
    }
    
    
    ###################################
    
    tkconfigure(tl4info, foreground="blue", font=police)
    
    loadp.adjust.Method <- function()
    {  
        curselectp.adjust <- as.numeric(tkcurselection(tl4))+1
        lcurselectp.adjust <- length(curselectp.adjust)
        
        p.adjustMethod <- c('none','BH', 'BY','bonferroni' )
        
        if (lcurselectp.adjust ==0){
            msgSelectp.adjust<-"Select p.adjust Method"
            tkmessageBox(message=msgSelectp.adjust)
        } else {
            tkdelete(tl4info,0,1)
            tkinsert(tl4info,"end",p.adjustMethod[curselectp.adjust])
            ENV$p.adjustChoice <- p.adjustMethod[curselectp.adjust]
            tkfocus()
        }
        
    }
    
    
    p.adjust.but <-tkbutton(ENV$ttPheno,text="select",command=loadp.adjust.Method)
    
    tkgrid(label4,tl4,yscr4)
    tkgrid.configure(yscr4,rowspan=20, columnspan=2,sticky="nsw")
    tkgrid(xscr4)
    tkgrid.configure(xscr4,rowspan=2, column=1,sticky="we")
    tkgrid(p.adjust.but, tl4info, columnspan=1)
    tkgrid(xscr4Info)
    tkgrid.configure(xscr4Info,rowspan=4, column=1,sticky="we")
    
    p.adjustMethod <- c('none','BH', 'BY','bonferroni' )
    
    
    
    for (i in 1:length(p.adjustMethod)){
        
        tkinsert(tl4,"end",p.adjustMethod[i])
    }
    
    # Default selection.  Indexing starts at zero.
    tkselection.set(tl4,1) 
    
    onOK <- function(){
        if(exists("EventTime", envir = ENV)){
            survival <- matrix(c(ENV$EventTime[1],ENV$EventTime[2]), ncol=2, byrow=TRUE)  ##Statut: Living vs die, months: number of months
            colnames(survival)<-  c('time','event')  ## colnames of survival matrix 
            
            ENV$var2test <- list(survival=survival,categorical=ENV$Category, continuous=ENV$Continu) 
            
            print(paste("varibales to test:",ENV$var2test, sep=" "))
            print(paste("pVals adujustment choice:",ENV$p.adjustChoice, sep=" "))
        } else{
            
            ENV$var2test <- list(categorical=ENV$Category, continuous=ENV$Continu)
            print(paste("varibales to test:",ENV$var2test, sep=" "))
            print(paste("pVals adujustment choice:",ENV$p.adjustChoice, sep=" "))
        }
        tkdestroy(ENV$ttPheno)
        
    }
    
    Ok.but <-tkbutton(ENV$ttPheno,text=" OK ",command=onOK)
    tkgrid(Ok.but)
    tkgrid.configure(Ok.but,rowspan=4, column=1,sticky="we")
    
    
    
    
}
