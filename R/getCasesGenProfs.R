#' get Cases and Genetic Profiles of selected Studies.
#' @usage getCasesGenProfs()
#' @return This function is run by the "Get Cases and Genetic Profiles for selected Studies in starting window. This function needs to select at least one study and display Cases and genetic profiles in the main window.
#' @examples
#' ##Load Session
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
#' ## load Cases and Genetic Profiles 
#' \dontrun{
#' getCasesGenProfs()
#' }
#' @export
#' 
getCasesGenProfs <- function(){
    
    tclRequire("Tktable")
    
    if(exists("ttCasesGenProfs", envir = myGlobalEnv)){
        tkdestroy(myGlobalEnv$ttCasesGenProfs)
    }
    
    myGlobalEnv$ttCasesGenProfs<-tktoplevel()
    tktitle(myGlobalEnv$ttCasesGenProfs) <- "Cases list and Genetic Profiles"
    #tkwm.geometry(myGlobalEnv$ttCasesGenProfs, "550x420")
    
    ##MENU
    topMenu <- tkmenu(myGlobalEnv$ttCasesGenProfs)           # Create a menu
    tkconfigure(myGlobalEnv$ttCasesGenProfs, menu = topMenu) # Add it to the 'myGlobalEnv$ttCasesGenProfs' window
    
    OpenMenu <- tkmenu(topMenu, tearoff=FALSE)
    
    ClinMenu <- tkmenu(topMenu, tearoff = FALSE)
    MutMenu<- tkmenu(topMenu, tearoff = FALSE)
    ProfMenu<- tkmenu(topMenu, tearoff = FALSE)
    PhenoMenu <- tkmenu(topMenu, tearoff = FALSE)
    GSEAMenu <- tkmenu(topMenu, tearoff= FALSE)
    ClassMenu<- tkmenu(topMenu, tearoff=FALSE)
    PlotMenu<- tkmenu(topMenu, tearoff = FALSE)
    HelpMenu<- tkmenu(topMenu, tearoff = FALSE)
    GeneListMenu <- tkmenu(OpenMenu, tearoff=FALSE)
    Gene1 <- tkmenu(PlotMenu, tearoff= FALSE)
    Gene2 <- tkmenu(PlotMenu, tearoff= FALSE)
    Survival <- tkmenu(PlotMenu, tearoff= FALSE)
    get <- tkmenu(GSEAMenu, tearoff=FALSE)
    GSEAlm <- tkmenu(GSEAMenu, tearoff=FALSE)
    TreeClass <- tkmenu(ClassMenu, tearoff=FALSE)
    ##TopMenus
    #tkadd(topMenu,"command", label = "Gene List", command= function()getGeneList())
    tkadd(topMenu, "cascade", label = "Load", menu=OpenMenu)
    tkadd(OpenMenu, "cascade", label = "Gene list", menu=GeneListMenu)
    tkadd(topMenu, "cascade", label = "Clinical Data", menu=ClinMenu)
    tkadd(topMenu, "cascade", label = "Mutation", menu= MutMenu)
    tkadd(topMenu,"command", label = "Methylation", command= function()getMetDataMultipleGenes())
    
    tkadd(topMenu, "cascade", label = "Profiles", menu=ProfMenu)
    #tkadd(topMenu, "command", label = "Profile Data", command=function()getProfilesData())
    tkadd(topMenu, "cascade", label = "PhenoTest", menu=PhenoMenu)
    tkadd(topMenu, "cascade", label ="GSEA", menu=GSEAMenu)
    tkadd(topMenu, "cascade", label ="Classification", menu=ClassMenu)
    tkadd(topMenu, "cascade", label = "Plot", menu=PlotMenu)
    tkadd(topMenu, "cascade", label = "Help", menu = HelpMenu)
    tkadd(PlotMenu, "cascade", label= "1Gene", menu=Gene1)
    tkadd(PlotMenu, "cascade", label= "2Genes", menu=Gene2)
    tkadd(PlotMenu, "cascade", label= "Survival", menu=Survival)
    tkadd(PlotMenu, "command", label = "Circos", command = function() getCircos(dimension = "All"))
    
    tkadd(GSEAMenu, "cascade", label = "get", menu=get)
    #tkadd(GSEAMenu, "command", label = "Which MSig for gene list", command= function()Match_GeneList_MSigDB())
    #tkadd(GSEAMenu, "command", label = "get MSigDB for eSet", command= function()getMSigDB())
    tkadd(GSEAMenu, "cascade", label= "linear Model", menu=GSEAlm)
    
    #Second Menu
    tkadd(GeneListMenu, "command", label = "File", command = function() getGeneList())
    tkadd(GeneListMenu, "command", label = "Example", command = function() getGeneListExample())
    tkadd(GeneListMenu, "command", label = "MSigDB", command = function() getGeneListFromMSigDB())
    tkadd(ClinMenu, "command", label = "Multiple Cases", command = function() getClinicData_MultipleCases(getSummaryGSEAExists=0))
    #tkadd(ClinMenu, "command", label = "All", command = function() getClinicData_MultipleCases())
    #tkadd(ClinMenu, "command", label = "Single Case", command = function() getClinicData_SingleCase())
    #tkadd(ClinMenu, "command", label = "Quit", command = function() tkdestroy(myGlobalEnv$ttCasesGenProfs))
    tkadd(MutMenu, "command", label = "All", command=function()getMutData())
    tkadd(MutMenu, "command", label = "Specific", command= function()getSpecificMut())
    tkadd(ProfMenu, "command", label = "Single Gene", command= function() getProfilesDataSingleGene())
    tkadd(ProfMenu, "command", label = "Multiple Genes", command= function() getProfilesDataMultipleGenes(getSummaryGSEAExists=0))
    tkadd(ProfMenu, "command", label = "ListProfData", command= function() getListProfData())
    
    tkadd(Gene1, "command", label = "2 Gen. Profiles",  command = function() plot_1Gene_2GenProfs())
    tkadd(Gene2, "command", label = "1 Gen. Profile", command = function() plot_2Genes_1GenProf())
    tkadd(Survival, "command", label = "Kaplan-Meier", command = function() getSurvival(Coxph = 0))
    tkadd(Survival, "command", label = "CoxPH", command = function() getSurvival(Coxph = 1))
    tkadd(PhenoMenu, "command", label = "eSet", command = function() geteSet())
    tkadd(PhenoMenu, "command", label= "Pheno/Exp", command= function() getPhenoTest())
    
    #tkadd(get, "command", label = "CLS file", command= function()getCLSfile())
    #tkadd(get, "command", label = "GCT file", command= function()getGCTfile())
    tkadd(get, "command", label = "GCT,CLS files", command= function()getGCT_CLSfiles())
    tkadd(get, "command", label= "Results", command= function()Run.GSEA())
    tkadd(get, "command", label= "Summary", command= function()getSummaryGSEA())
    tkadd(GSEAlm, "command", label = "Which MSig for gene list", command= function()Match_GeneList_MSigDB())
    tkadd(GSEAlm, "command", label= "SubMSigDB/eSet", command= function()getMSigDB(myGlobalEnv$eSet,1))
    tkadd(GSEAlm, "command", label= "Phenotypes into Disease", command= function()getGSEAlm_Variables())
    tkadd(GSEAlm, "command", label= "Disease vs Disease", command= function()getGSEAlm_Diseases())
    
    
    #tkadd(GSEAMenu, "command", label= "GSEAlm", command= function()getGSEAlm())
    tkadd(ClassMenu, "command", label= "Genes/Diseases", command= function() getGenesClassifier())
    # tkadd(ClassMenu, "command", label= "Genes/Pheno Single", command= function() getGenesTree_SingleCase())
    #tkadd(ClassMenu, "command", label= "Genes/Pheno Multiple", command= function() getGenesTree_MultipleCases())
    tkadd(ClassMenu, "cascade", label= "Genes/Pheno Tree", menu=TreeClass)
    tkadd(TreeClass, "command", label= "Single", command= function()getGenesTree_SingleCase())
    tkadd(TreeClass, "command", label= "Multiple", command= function()getGenesTree_MultipleCases())
    
    tkadd(HelpMenu, "command", label="Manual", command= function()canceR_Vignette())
    tkadd(HelpMenu, "command", label= "Help", command = function() canceRHelp())
    tkadd(HelpMenu, "command", label = "About", command= function() about())
    
    
    # Take LABELS in myGlobalEnv$ttCasesGenProfs
    tklabel0 = tklabel(myGlobalEnv$ttCasesGenProfs, text= "The Cases (Samples)")
    tklabel1 =tklabel(myGlobalEnv$ttCasesGenProfs,text="The Genetic Profiles")
    tkgrid(tklabel0,tklabel1, columnspan=2, pady = 10)
    
    #Define Scrolls
    yscr1 <- tkscrollbar(myGlobalEnv$ttCasesGenProfs, repeatinterval=2,
                         command=function(...)tkyview(myGlobalEnv$tc,...))
    xscr1 <- tkscrollbar(myGlobalEnv$ttCasesGenProfs, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(myGlobalEnv$tc,...))
    
    yscr2 <- tkscrollbar(myGlobalEnv$ttCasesGenProfs, repeatinterval=2,
                         command=function(...)tkyview(myGlobalEnv$tl,...))
    xscr2 <- tkscrollbar(myGlobalEnv$ttCasesGenProfs, repeatinterval=2,orient="horizontal",
                         command=function(...)tkxview(myGlobalEnv$tl,...))
    
    xscr1Info <- tkscrollbar(myGlobalEnv$ttCasesGenProfs, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tInfoC,...))
    xscr2Info <- tkscrollbar(myGlobalEnv$ttCasesGenProfs, repeatinterval=2,orient="horizontal",
                             command=function(...)tkxview(tInfoG,...))
    
    
    #policeInfo <-tkfont.create(family="arial", size=10)
    
    
    
    #Define Frame and LISTBOX
    #frameOverall <- tkframe(myGlobalEnv$ttCasesGenProfs)
    #frame1 <- tkframe(myGlobalEnv$ttCasesGenProfs,relief="groove",borderwidth=5, height = 100, width = 500)
    myGlobalEnv$tc<-tklistbox(myGlobalEnv$ttCasesGenProfs,height=15, width= 68 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr1,...),yscrollcommand=function(...)tkset(yscr1,...),background="white")
    myGlobalEnv$tl<-tklistbox(myGlobalEnv$ttCasesGenProfs,height=15, width= 68 ,selectmode="multiple",xscrollcommand=function(...)tkset(xscr2,...),yscrollcommand=function(...)tkset(yscr2,...),background="white")
    
    tInfoC<-tklistbox(myGlobalEnv$ttCasesGenProfs,height=2, width= 68,selectmode="multiple",xscrollcommand=function(...)tkset(xscr1Info,...),background="white")
    #tkconfigure(tInfoC, foreground="blue", font=policeInfo)
    
    tInfoG<-tklistbox(myGlobalEnv$ttCasesGenProfs,height=2, width= 68,selectmode="multiple",xscrollcommand=function(...)tkset(xscr2Info,...),background="white")
    #tkconfigure(tInfoG, foreground="blue", font=policeInfo)
    
    
    tkgrid(yscr1,myGlobalEnv$tc,myGlobalEnv$tl,yscr2, columnspan=1)
    tkgrid.configure(yscr1,rowspan=20, columnspan=1,sticky="nsw")
    tkgrid.configure(yscr2,rowspan=20,sticky="nsw")
    
    tkgrid(xscr1,xscr2,columnspan=2)
    tkgrid.configure(xscr1,rowspan=2,columnspan=2,sticky="ew")
    tkgrid.configure(xscr2,rowspan=2,columnspan= 4,sticky="ew")
    
    tklabel2 = tklabel(myGlobalEnv$ttCasesGenProfs, text= "Selected Cases (Samples)")
    tklabel3 =tklabel(myGlobalEnv$ttCasesGenProfs,text="Selected Genetic Profiles")
    tkgrid(tklabel2,tklabel3, columnspan=2, pady = 10)
    
    tkgrid(tInfoC, tInfoG, columnspan=2)
    tkgrid(xscr1Info,xscr2Info,columnspan=2)
    tkgrid.configure(xscr1Info,rowspan=2,columnspan=2,sticky="ew")
    tkgrid.configure(xscr2Info,rowspan=2,columnspan= 3,sticky="ew")
    
    
    getCases()
    getGenProfs()
    
    
    
    loadSelectedCases <- function()
    {  
        curselectCases <- as.numeric(tkcurselection(myGlobalEnv$tc))+1
        lcurselectCases <- length(curselectCases)
        
        myGlobalEnv$curselectCases <- curselectCases
        myGlobalEnv$CaseChoice <- myGlobalEnv$CasesStudies[curselectCases+1]
        
        if (lcurselectCases ==0){
            msgSelectCase="Select at least one Case"
            tkmessageBox(message=msgSelectCase)
            
        } else {
            
            
            ## correspond each case  to its study
            StudyRefCase = 0
            for( k in 1: lcurselectCases){
                h<-1
                while (curselectCases[k] >  (myGlobalEnv$LCases[h]+1)){
                    curselectCases[k] <- curselectCases[k] - myGlobalEnv$LCases[h] - 1
                    h<- h+1
                }  
                StudyRefCase <- cbind(StudyRefCase,myGlobalEnv$checked_Studies_forCases[h])
            }
            myGlobalEnv$StudyRefCase <-StudyRefCase[-1]
            
            
            
            #redefine curselectCases
            myGlobalEnv$curselectCases <- as.numeric(tkcurselection(myGlobalEnv$tc))+1
            lcurselectCases <- length(myGlobalEnv$curselectCases)
            
            ##loop converting curselectCases value of case to index value of the same case
            
            LCases_sum<-0
            
            for( j in 1:lcurselectCases){
                for (i in 1:(myGlobalEnv$lchecked_Studies_forCases)){
                    
                    if (curselectCases[j] < (LCases_sum + myGlobalEnv$LCases[i]+ i)){
                        curselectCases[j] <- curselectCases[j]  - 1
                        break
                    } 
                    
                    LCases_sum<- LCases_sum+myGlobalEnv$LCases[i]+1
                }
            }
            myGlobalEnv$curselectCases_forStudy<-curselectCases
            tkdelete(tInfoC,0,1)
            tkinsert(tInfoC,"end",myGlobalEnv$curselectCases_forStudy)
            tkinsert(tInfoC,"end",myGlobalEnv$CaseChoice)
            tkfocus(myGlobalEnv$ttCasesGenProfs)
        }
        
    }
    
    loadSelectedGenProfs <- function(){
        curselectGenProfs = as.numeric(tkcurselection(myGlobalEnv$tl))+1
        lcurselectGenProfs = length(curselectGenProfs)
        
        myGlobalEnv$curselectGenProfs <- curselectGenProfs
        myGlobalEnv$GenProfChoice <- myGlobalEnv$GenProfsStudies[curselectGenProfs+1]
        
        
        if (lcurselectGenProfs ==0){
            msgSelectCase="Select at least one Genetic Profil"; tkmessageBox(message=msgSelectCase)
        } else {
            
            ## group selected Cases by study. To  avoid corresponding Genetic Profile and Case from different Study
            StudyRefGenProf = 0
            for( k in 1: lcurselectGenProfs){
                h=1
                
                while (curselectGenProfs[k] >  (myGlobalEnv$LGenProfs[h]+1)){
                    curselectGenProfs[k] = curselectGenProfs[k] - myGlobalEnv$LGenProfs[h] - 1
                    h= h+1
                } 
                #Cancer Reference (StudyRefGenProf) were compared before to get Profile Data
                StudyRefGenProf = cbind(StudyRefGenProf, myGlobalEnv$checked_Studies_forGenProf[h]) 
            }
            
            myGlobalEnv$StudyRefGenProf<-StudyRefGenProf[-1]
            
            
            ##loop converting curselectGenProfs value of Genetic profile to index value of the same genetic profile in cgds
            LGenProfs_sum<-0
            
            for( j in 1: lcurselectGenProfs){
                for (i in 1:(myGlobalEnv$lchecked_Studies_forGenProf)){
                    
                    
                    if (curselectGenProfs[j] < (LGenProfs_sum + myGlobalEnv$LGenProfs[i]+i)){
                        curselectGenProfs[j] <- curselectGenProfs[j] - 1
                        break
                    } 
                    LGenProfs_sum<- LGenProfs_sum+myGlobalEnv$LGenProfs[i]+1
                }
                
            }
            myGlobalEnv$curselectGenProfs_forStudy <- curselectGenProfs
            
            tkdelete(tInfoG,0,1)
            tkinsert(tInfoG,"end",myGlobalEnv$curselectGenProfs_forStudy)
            tkinsert(tInfoG,"end",myGlobalEnv$GenProfChoice)
            tkfocus(myGlobalEnv$ttCasesGenProfs)
        }
    }
    
    LoadCases.but <-tkbutton(myGlobalEnv$ttCasesGenProfs,text="   Load selected Cases   ",command=loadSelectedCases)
    
    LoadGenProf.but <-tkbutton(myGlobalEnv$ttCasesGenProfs,text="   Load selected Genetic Profiles   ",command=loadSelectedGenProfs)
    
    
    
    
    tkgrid(LoadCases.but,LoadGenProf.but, columnspan=2)
    
    tkgrid(tInfoC)
    
}