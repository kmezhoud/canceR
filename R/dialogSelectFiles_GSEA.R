#' Dialog Box to  Select GCT, CLS, GMT and output Files for GSEA-R (Broad Institute) 
#' @usage
#' dialogSelectFiles_GSEA()
#' 
#' @return A vector with files paths
#' @export
#'
#' @examples
#' load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))
#' \dontrun{
#' dialogSelectFiles_GSEA()
#' }
#' 
#' 
dialogSelectFiles_GSEA <- function(){
    
    myGlobalEnv$ttDialogGSEA<-tktoplevel()
    
    tktitle(myGlobalEnv$ttDialogGSEA) <- "Choose files"
    
    
    
    ###select GSEA program file
    tlGSEA<-tklistbox(myGlobalEnv$ttDialogGSEA,height=1, width= 70 ,background="white")
    
    ### Select phenotype file (.cls)
    
    myGlobalEnv$tlCLS<-tklistbox(myGlobalEnv$ttDialogGSEA,height=1, width= 70 ,background="white")
    chooseCLS <- function(){
        if(exists("fname.CLS", envir = myGlobalEnv)){
            tkdelete(myGlobalEnv$tlCLS,0,1)
        }
        myGlobalEnv$fname.CLS <- tclvalue(tkgetOpenFile(filetypes = "{{CLS Files} {.cls}} {{All files} *}", title="load phenotype.cls file")) # Very simple, isn't it?
        #fname.CLS <<- file.choose()
        if(length(grep(".cls$", myGlobalEnv$fname.CLS))==0){
            tkmessageBox(message="Choose .cls format file", icon="warning")
            stop("Choose .cls format file")
            tkfocus(myGlobalEnv$ttDialogGSEA)
        } else
            tkinsert(myGlobalEnv$tlCLS,"end",myGlobalEnv$fname.CLS)
        tkfocus(myGlobalEnv$ttDialogGSEA)
    }
    
    chooseCLS.but <- tkbutton(myGlobalEnv$ttDialogGSEA, text = "browse", command = chooseCLS)
    
    Phenotype.cls.but<- tkbutton(myGlobalEnv$ttDialogGSEA, text = "Phenotype.cls", command = getGCTCLSExample)
    #tklabelCLS <- tklabel(myGlobalEnv$ttDialogGSEA,text="Phenotype.cls")
    
    tkgrid(Phenotype.cls.but,myGlobalEnv$tlCLS,chooseCLS.but, columnspan=1)
    tkgrid.configure(Phenotype.cls.but,rowspan=20, columnspan=1,sticky="nsw")
    
    ### Select gene Expression matrix (.gct file)
    myGlobalEnv$tlGCT<-tklistbox(myGlobalEnv$ttDialogGSEA,height=1, width= 70 ,background="white")
    chooseGCT <- function(){
        if(exists("fname.GCT", envir = myGlobalEnv)){
            tkdelete(myGlobalEnv$tlGCT,0,1)
        }
        myGlobalEnv$fname.GCT <- tclvalue(tkgetOpenFile(filetypes = "{{GCT Files} {.gct}} {{All files} *}", title="load Expression.gct file")) # Very simple, isn't it?
        #fname.GCT <<- file.choose()
        if(length(grep(".gct$", myGlobalEnv$fname.GCT))==0){
            tkmessageBox(message="Choose .gct format file", icon="warning")
            stop("Choose .gct format file")
            tkfocus(myGlobalEnv$ttDialogGSEA)
        } else
            tkinsert(myGlobalEnv$tlGCT,"end",myGlobalEnv$fname.GCT)
        tkfocus(myGlobalEnv$ttDialogGSEA)
    }
    
    chooseGCT.but <- tkbutton(myGlobalEnv$ttDialogGSEA, text = "browse", command = chooseGCT)
    
    tklabelGCT <-tklabel(myGlobalEnv$ttDialogGSEA,text="Expression.gct")
    
    tkgrid(tklabelGCT,myGlobalEnv$tlGCT,chooseGCT.but, columnspan=1)
    tkgrid.configure(tklabelGCT,rowspan=20, columnspan=1,sticky="nsw")
    
    ###Select gene Set (mSigDB) dataBase (.gmt file)
    
    myGlobalEnv$tlGMT<-tklistbox(myGlobalEnv$ttDialogGSEA,height=1, width= 70 ,background="white")
    chooseGMT <- function(){
        if(exists("fname.GMT", envir = myGlobalEnv)){
            tkdelete(myGlobalEnv$tlGMT,0,1)
        }
        myGlobalEnv$fname.GMT <- tclvalue(tkgetOpenFile(filetypes = "{{GMT Files} {.gmt}} {{All files} *}", title="load MSigDB.gmt file")) # Very simple, isn't it?
        #fname.GMT <<- file.choose()
        if(length(grep(".gmt$", myGlobalEnv$fname.GMT))==0){
            tkmessageBox(message="Choose .gmt format file", icon="warning")
            stop("Choose .gmt format file")
            tkfocus(myGlobalEnv$ttDialogGSEA)
        } else
            tkinsert(myGlobalEnv$tlGMT,"end",myGlobalEnv$fname.GMT)
        tkfocus(myGlobalEnv$ttDialogGSEA)
    }
    
    chooseGMT.but <- tkbutton(myGlobalEnv$ttDialogGSEA, text = "browse", command = chooseGMT)
    
    
    MSigDB.but <- tkbutton(myGlobalEnv$ttDialogGSEA, text = "MSigDB.gmt", command = getMSigDBExample)
    
    tkgrid(MSigDB.but,myGlobalEnv$tlGMT,chooseGMT.but, columnspan=1)
    tkgrid.configure(MSigDB.but,rowspan=20, columnspan=1,sticky="nsw")
    
    
    ### Select output Directory 
    
    tlOutput<-tklistbox(myGlobalEnv$ttDialogGSEA,height=1, width= 70 ,background="white")
    
    chooseOutput <- function(){
        if(exists("fname.Output", envir = myGlobalEnv)){
            tkdelete(tlOutput,0,1)
        }
        prefix<- paste(sub(".cls","" ,basename(myGlobalEnv$fname.CLS)),sub(".gmt","" ,basename(myGlobalEnv$fname.GMT)), sep="_")
        myGlobalEnv$fname.Output <- paste(getwd(),"/Results/GSEA/", prefix,"/", sep="")
        
        tkinsert(tlOutput,"end",myGlobalEnv$fname.Output)
        
        Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        if(!file.exists("Results/GSEA/")){
            dir.create(file.path(paste(getwd(), "/Results/GSEA", sep="")), showWarnings = FALSE)
            dir.create(file.path(paste(getwd(), "/Results/GSEA", sep="")), showWarnings = FALSE)
        } 
        setwd("./Results/GSEA/")
        if(!file.exists(basename(myGlobalEnv$fname.Output) )){
            
            dir.create(basename(myGlobalEnv$fname.Output),showWarnings=TRUE)
            
        }
        setwd("../../")
    }
    
    chooseOutput.but <- tkbutton(myGlobalEnv$ttDialogGSEA, text = "   Set   ", command = chooseOutput)
    
    tklabelOutput <- tklabel(myGlobalEnv$ttDialogGSEA,text="Output  folder")
    
    tkgrid(tklabelOutput,tlOutput,chooseOutput.but, columnspan=1)
    tkgrid.configure(tklabelOutput,rowspan=20, columnspan=1,sticky="nsw")
    
    
    OK <- function(){
        
        
        print(myGlobalEnv$fname.GCT)
        print(myGlobalEnv$fname.CLS)
        print(myGlobalEnv$fname.GMT)
        print(myGlobalEnv$fname.Output)
        
        #setwd(paste(getwd(), "/Results/GSEA", sep=""))
        tkconfigure(myGlobalEnv$ttDialogGSEA,cursor="watch")
        tkdestroy(myGlobalEnv$ttDialogGSEA)
    }
    
    Ok.but <- tkbutton(myGlobalEnv$ttDialogGSEA, text = " OK ", command = OK)
    
    Cancel <- function(){
        
        tkdestroy(myGlobalEnv$ttDialogGSEA)
        
    }
    Cancel.but <- tkbutton(myGlobalEnv$ttDialogGSEA, text= "Cancel", command = Cancel)
    tkgrid(Cancel.but, Ok.but)
    tkgrid.configure(Cancel.but, sticky="n",columnspan=2)
    tkgrid.configure(Ok.but, sticky="n",columnspan=2)
    
    tkwait.window(myGlobalEnv$ttDialogGSEA)
    
}