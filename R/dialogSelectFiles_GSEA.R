#' Dialog Box to  Select GCT, CLS, GMT and output Files for GSEA-R (Broad Institute) 
#' @usage
#' dialogSelectFiles_GSEA()
#' 
#' @return A vector with files paths
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' dialogSelectFiles_GSEA()
#' }
#' 
#' 
dialogSelectFiles_GSEA <- function(){
    
    ENV$ttDialogGSEA<-tktoplevel()
    
    tktitle(ENV$ttDialogGSEA) <- "Choose files"
    
    
    
    ###select GSEA program file
    tlGSEA<-tklistbox(ENV$ttDialogGSEA,height=1, width= 70 ,background="white")
    
    ### Select phenotype file (.cls)
    
    ENV$tlCLS<-tklistbox(ENV$ttDialogGSEA,height=1, width= 70 ,background="white")
    chooseCLS <- function(){
        if(exists("fname.CLS", envir = ENV)){
            tkdelete(ENV$tlCLS,0,1)
        }
        ENV$fname.CLS <- tclvalue(tkgetOpenFile(filetypes = "{{CLS Files} {.cls}} {{All files} *}", title="load phenotype.cls file")) # Very simple, isn't it?
        #fname.CLS <<- file.choose()
        if(length(grep(".cls$", ENV$fname.CLS))==0){
            tkmessageBox(message="Choose .cls format file", icon="warning")
            stop("Choose .cls format file")
            tkfocus(ENV$ttDialogGSEA)
        } else
            tkinsert(ENV$tlCLS,"end",ENV$fname.CLS)
        tkfocus(ENV$ttDialogGSEA)
    }
    
    chooseCLS.but <- tkbutton(ENV$ttDialogGSEA, text = "browse", command = chooseCLS)
    
    Phenotype.cls.but<- tkbutton(ENV$ttDialogGSEA, text = "Phenotype.cls", command = getGCTCLSExample)
    #tklabelCLS <- tklabel(ENV$ttDialogGSEA,text="Phenotype.cls")
    
    tkgrid(Phenotype.cls.but,ENV$tlCLS,chooseCLS.but, columnspan=1)
    tkgrid.configure(Phenotype.cls.but,rowspan=20, columnspan=1,sticky="nsw")
    
    ### Select gene Expression matrix (.gct file)
    ENV$tlGCT<-tklistbox(ENV$ttDialogGSEA,height=1, width= 70 ,background="white")
    chooseGCT <- function(){
        if(exists("fname.GCT", envir = ENV)){
            tkdelete(ENV$tlGCT,0,1)
        }
        ENV$fname.GCT <- tclvalue(tkgetOpenFile(filetypes = "{{GCT Files} {.gct}} {{All files} *}", title="load Expression.gct file")) # Very simple, isn't it?
        #fname.GCT <<- file.choose()
        if(length(grep(".gct$", ENV$fname.GCT))==0){
            tkmessageBox(message="Choose .gct format file", icon="warning")
            stop("Choose .gct format file")
            tkfocus(ENV$ttDialogGSEA)
        } else
            tkinsert(ENV$tlGCT,"end",ENV$fname.GCT)
        tkfocus(ENV$ttDialogGSEA)
    }
    
    chooseGCT.but <- tkbutton(ENV$ttDialogGSEA, text = "browse", command = chooseGCT)
    
    tklabelGCT <-tklabel(ENV$ttDialogGSEA,text="Expression.gct")
    
    tkgrid(tklabelGCT,ENV$tlGCT,chooseGCT.but, columnspan=1)
    tkgrid.configure(tklabelGCT,rowspan=20, columnspan=1,sticky="nsw")
    
    ###Select gene Set (mSigDB) dataBase (.gmt file)
    
    ENV$tlGMT<-tklistbox(ENV$ttDialogGSEA,height=1, width= 70 ,background="white")
    chooseGMT <- function(){
        if(exists("fname.GMT", envir = ENV)){
            tkdelete(ENV$tlGMT,0,1)
        }
        ENV$fname.GMT <- tclvalue(tkgetOpenFile(filetypes = "{{GMT Files} {.gmt}} {{All files} *}", title="load MSigDB.gmt file")) # Very simple, isn't it?
        #fname.GMT <<- file.choose()
        if(length(grep(".gmt$", ENV$fname.GMT))==0){
            tkmessageBox(message="Choose .gmt format file", icon="warning")
            stop("Choose .gmt format file")
            tkfocus(ENV$ttDialogGSEA)
        } else
            tkinsert(ENV$tlGMT,"end",ENV$fname.GMT)
        tkfocus(ENV$ttDialogGSEA)
    }
    
    chooseGMT.but <- tkbutton(ENV$ttDialogGSEA, text = "browse", command = chooseGMT)
    
    
    MSigDB.but <- tkbutton(ENV$ttDialogGSEA, text = "MSigDB.gmt", command = getMSigDBExample)
    
    tkgrid(MSigDB.but,ENV$tlGMT,chooseGMT.but, columnspan=1)
    tkgrid.configure(MSigDB.but,rowspan=20, columnspan=1,sticky="nsw")
    
    
    ### Select output Directory 
    
    tlOutput<-tklistbox(ENV$ttDialogGSEA,height=1, width= 70 ,background="white")
    
    chooseOutput <- function(){
        if(exists("fname.Output", envir = ENV)){
            tkdelete(tlOutput,0,1)
        }
        prefix<- paste(sub(".cls","" ,basename(ENV$fname.CLS)),sub(".gmt","" ,basename(ENV$fname.GMT)), sep="_")
        ENV$fname.Output <- paste(getwd(),"/Results/GSEA/", prefix,"/", sep="")
        
        tkinsert(tlOutput,"end",ENV$fname.Output)
        
        Sys.chmod(getwd(), mode = "0777", use_umask = TRUE)
        if(!file.exists("Results/GSEA/")){
            dir.create(file.path(paste(getwd(), "/Results/GSEA", sep="")), showWarnings = FALSE)
            dir.create(file.path(paste(getwd(), "/Results/GSEA", sep="")), showWarnings = FALSE)
        } 
        setwd("./Results/GSEA/")
        if(!file.exists(basename(ENV$fname.Output) )){
            
            dir.create(basename(ENV$fname.Output),showWarnings=TRUE)
            
        }
        setwd("../../")
    }
    
    chooseOutput.but <- tkbutton(ENV$ttDialogGSEA, text = "   Set   ", command = chooseOutput)
    
    tklabelOutput <- tklabel(ENV$ttDialogGSEA,text="Output  folder")
    
    tkgrid(tklabelOutput,tlOutput,chooseOutput.but, columnspan=1)
    tkgrid.configure(tklabelOutput,rowspan=20, columnspan=1,sticky="nsw")
    
    
    OK <- function(){
        
        
        print(ENV$fname.GCT)
        print(ENV$fname.CLS)
        print(ENV$fname.GMT)
        print(ENV$fname.Output)
        
        #setwd(paste(getwd(), "/Results/GSEA", sep=""))
        tkconfigure(ENV$ttDialogGSEA,cursor="watch")
        tkdestroy(ENV$ttDialogGSEA)
    }
    
    Ok.but <- tkbutton(ENV$ttDialogGSEA, text = " OK ", command = OK)
    
    Cancel <- function(){
        
        tkdestroy(ENV$ttDialogGSEA)
        
    }
    Cancel.but <- tkbutton(ENV$ttDialogGSEA, text= "Cancel", command = Cancel)
    tkgrid(Cancel.but, Ok.but)
    tkgrid.configure(Cancel.but, sticky="n",columnspan=2)
    tkgrid.configure(Ok.but, sticky="n",columnspan=2)
    
    tkwait.window(ENV$ttDialogGSEA)
    
}