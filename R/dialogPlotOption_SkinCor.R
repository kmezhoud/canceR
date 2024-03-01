#' Checkbox to select variables for plotting
#' @usage
#' dialogPlotOption_SkinCor(s)
#' @param s  integer number of Studies
#' 
#' @return Dialog box with setting of correlation method
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/gbm_tcgaPlotTwoGenProf.rds", sep=""))
#' \dontrun{
#' dialogPlotOption_SkinCor(1)
#' }
#' @import tkrplot
#' 
dialogPlotOption_SkinCor <- function(s){
    
    ENV$ttDialSkinCor <- tktoplevel()
    tktitle(ENV$ttDialSkinCor) <- paste("Plot Option: Skin, Correlation, X/Y...")
    #tkwm.geometry(ENV$ttDialSkinCor, "300x170")
    
    frameSkin <- tkframe(ENV$ttDialSkinCor,relief="groove",borderwidth=2)
    
    
    frameCorrelation <- tkframe(ENV$ttDialSkinCor,relief="groove",borderwidth=2)
    
    frameAxes <- tkframe(ENV$ttDialSkinCor,relief="groove",borderwidth=2)
    
    tkgrid(frameSkin)
    tkgrid.configure(frameSkin, sticky="new")
    tkgrid(tklabel(frameSkin, text="Specify which plotting layout skin to use: "), columnspan=3)
    
    rbValue1 <- tclVar("cont") # le choix par défaut
    # création des 3 boutons radio
    rb1 <- tkradiobutton(frameSkin)
    ##rb2 works with one case and one gen.profile
    #rb2 <- tkradiobutton(frameSkin)
    rb3 <- tkradiobutton(frameSkin)
    rb4 <- tkradiobutton(frameSkin)
    
    # config des boutons radio. Une seule variable tcl pour 3 boutons
    tkconfigure(rb1,variable=rbValue1,value="cont", text="continu")
    #tkconfigure(rb2,variable=rbValue1,value="disc", text="discret")
    tkconfigure(rb3,variable=rbValue1,value="disc_cont", text="disc_cont")
    tkconfigure(rb4,variable=rbValue1,value="cna_mrna_mut", text="cna_mrna_mut")
    tkgrid(rb1, rb3, rb4)
    
    
    rbValue2 <- tclVar("NULL") # le choix par défaut
    
    tkgrid(frameCorrelation)
    tkgrid.configure(frameCorrelation, sticky="new")
    tkgrid(tklabel(frameCorrelation, text=" Specify correlation method:"), columnspan=4)
    rb5 <- tkradiobutton(frameCorrelation)
    rb6 <- tkradiobutton(frameCorrelation)
    rb7 <- tkradiobutton(frameCorrelation)
    rb8 <- tkradiobutton(frameCorrelation)
    #rbValue <- tclVar("pearson") # le choix par défaut
    # config des boutons radio. Une seule variable tcl pour 3 boutons
    tkconfigure(rb5,variable=rbValue2,value="pearson", text="pearson")
    tkconfigure(rb6,variable=rbValue2,value="kendall", text="kendall")
    tkconfigure(rb7,variable=rbValue2,value="spearman", text="spearman")
    tkconfigure(rb8,variable=rbValue2,value="NULL", text="NULL")
    tkgrid(rb8, rb5, rb6, rb7, columnspan=3)
    
    
    
    rbValue3 <- tclVar("0") # le choix par défaut
    
    tkgrid(frameAxes)
    tkgrid.configure(frameAxes, sticky="new")
    tkgrid(tklabel(frameAxes, text=" Choose wich genetic profile in X axe:"), columnspan=2)
    rb9 <- tkradiobutton(frameAxes)
    rb10 <- tkradiobutton(frameAxes)
    
    #rbValue <- tclVar("pearson") # le choix par défaut
    # config des boutons radio. Une seule variable tcl pour 3 boutons
    tkconfigure(rb9,variable=rbValue3,value="0", text=ENV$GenProfsStudies[(ENV$curselectGenProfs)+1][(s*2)+1])
    tkconfigure(rb10,variable=rbValue3,value="1", text=ENV$GenProfsStudies[(ENV$curselectGenProfs)+1][(s*2)+2])
    
    tkgrid(rb9, rb10, columnspan=3)
    
    
    
    OnOK <- function()
    {
        rbVal1 <- as.character(tclvalue(rbValue1))
        rbVal2 <- as.character(tclvalue(rbValue2))
        rbVal3 <- as.character(tclvalue(rbValue3))
        tkdestroy(ENV$ttDialSkinCor)
        if (rbVal1=="cont")
            #tkmessageBox(message=rbVal1)
            ENV$skin<-paste(rbVal1)
        
        if (rbVal1=="disc_cont")
            #tkmessageBox(message=rbVal1)
            ENV$skin<-paste(rbVal1)
        if (rbVal1=="cna_mrna_mut")
            #tkmessageBox(message=rbVal1)
            ENV$skin<-paste(rbVal1)
        if (rbVal2=="pearson")
            #tkmessageBox(message=rbVal2)
            ENV$correlation<-paste(rbVal2)
        if (rbVal2=="kendall")
            #tkmessageBox(message=rbVal2)
            ENV$correlation<-paste(rbVal2)
        if (rbVal2=="spearman")
            #tkmessageBox(message=rbVal2)
            ENV$correlation<-paste(rbVal2)
        if (rbVal2=="NULL")
            #tkmessageBox(message=rbVal2)
            ENV$correlation<-paste("NULL")
        
        if (rbVal3=="0")
            #tkmessageBox(message=rbVal2)
            ENV$Axes<-paste("0")
        
        if (rbVal3=="1")
            #tkmessageBox(message=rbVal2)
            ENV$Axes<-paste("1")
    }
    
    OK.but <- tkbutton(ENV$ttDialSkinCor,text="OK",command=OnOK)
    tkgrid(OK.but)
    
}