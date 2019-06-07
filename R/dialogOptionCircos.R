#' Checkbox to select dimensions
#' @usage
#' dialogOptionCircos()
#' 
#' @return a checkbox with all dimensions
#' @export
#'
#' @examples
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/Circos.rds", sep=""))
#' \dontrun{
#' dialogOptionCircos()
#' #getCircos(dimension ="All")
#' }
#' @import circlize
dialogOptionCircos <- function(){

ttCircos <- tktoplevel()
#tkwm.geometry(ttCircos,"180x250")
tktitle(ttCircos) <- paste("Circos plot Option")

## Dimension Frame

frameDimension <- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(frameDimension)
tkgrid.configure(frameDimension, sticky="new")
tkgrid(tklabel(frameDimension, text="Select Dimensions: "), columnspan=3)

## mRNA checkBox
cbValmRNA <- tclVar("0")
cbmRNA <- tkcheckbutton(frameDimension)
tkconfigure(cbmRNA,variable=cbValmRNA, text="mRNA")
## CNA checkBox
cbValCNA <- tclVar("0")
cbCNA <- tkcheckbutton(frameDimension)
tkconfigure(cbCNA,variable=cbValCNA, text="CNA")
## Met HM450 checkBox
cbValHM450 <- tclVar("0")
cbHM450 <- tkcheckbutton(frameDimension)
tkconfigure(cbHM450,variable=cbValHM450, text="HM450")
## Met HM27 checkBox
cbValHM27 <- tclVar("0")
cbHM27 <- tkcheckbutton(frameDimension)
tkconfigure(cbHM27,variable=cbValHM27, text="HM27")
## RPPA checkBox
cbValRPPA <- tclVar("0")
cbRPPA <- tkcheckbutton(frameDimension)
tkconfigure(cbRPPA,variable=cbValRPPA, text="RPPA")
## miRNA checkBox
cbValmiRNA <- tclVar("0")
cbmiRNA <- tkcheckbutton(frameDimension)
tkconfigure(cbmiRNA,variable=cbValmiRNA, text="miRNA")
## Mut checkBox
cbValMut <- tclVar("0")
cbMut <- tkcheckbutton(frameDimension)
tkconfigure(cbMut,variable=cbValMut, text="Mut")

tkgrid(cbmRNA, cbCNA, cbHM450, cbHM27)
tkgrid(cbRPPA, cbmiRNA, cbMut)

## mRNA frame
mRNA_Entry  = tclVar(.8)
frameExp<- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(tklabel(frameExp, text="Threshold of mRNA correlation :"))
tkgrid(frameExp)
tkgrid.configure(frameExp, sticky="new")

cbValThrmRNA <- tclVar("0")
cbThrmRNA <- tkcheckbutton(frameExp)
tkconfigure(cbThrmRNA,variable=cbValThrmRNA, text="")

ScaleExp <- tkscale(frameExp,length=200,from=0.2,to=1,showvalue=TRUE,
        variable=mRNA_Entry,resolution=.05,orient='horiz')
tkgrid(ScaleExp,cbThrmRNA)
tkgrid.configure(cbThrmRNA,sticky="s", column=1)

## CNA frame
CNA_Entry  = tclVar(.8)
frameCNA<- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(tklabel(frameCNA, text="Threshold of CNA correlation :"))
tkgrid(frameCNA)
tkgrid.configure(frameCNA, sticky="new")

cbValThrCNA <- tclVar("0")
cbThrCNA <- tkcheckbutton(frameCNA)
tkconfigure(cbThrCNA,variable=cbValThrCNA, text="")

ScaleCNA<-tkscale(frameCNA,length=200,from=0.5,to=1,showvalue=TRUE,
        variable=CNA_Entry,resolution=.05,orient='horiz')

tkgrid(ScaleCNA, cbThrCNA)
tkgrid.configure(cbThrCNA,sticky="s", column=1)

## Methylation HM450 frame
HM450_Entry  = tclVar(.8)
frameHM450<- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(tklabel(frameHM450, text="Threshold of Met. HM450 cor :"))
tkgrid(frameHM450)
tkgrid.configure(frameHM450, sticky="new")

cbValThrHM450 <- tclVar("0")
cbThrHM450 <- tkcheckbutton(frameHM450)
tkconfigure(cbThrHM450,variable=cbValThrHM450, text="")

ScaleHM450 <- tkscale(frameHM450,length=200,from=0.2,to=1,showvalue=TRUE,
                      variable=HM450_Entry,resolution=.05,orient='horiz')
tkgrid(ScaleHM450, cbThrHM450)
tkgrid.configure(cbThrHM450, sticky="s",column=1)

## Methylation HM27 frame
HM27_Entry  = tclVar(.8)
frameHM27<- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(tklabel(frameHM27, text="Threshold of Met. HM27 cor :"))
tkgrid(frameHM27)
tkgrid.configure(frameHM27, sticky="new")

cbValThrHM27 <- tclVar("0")
cbThrHM27 <- tkcheckbutton(frameHM27)
tkconfigure(cbThrHM27,variable=cbValThrHM27, text="")

ScaleHM27 <- tkscale(frameHM27,length=200,from=0.2,to=1,showvalue=TRUE,
                     variable=HM27_Entry,resolution=.05,orient='horiz')
tkgrid(ScaleHM27, cbThrHM27)
tkgrid.configure(cbThrHM27,sticky="s",column=1)

##  RPPA frame
RPPA_Entry  = tclVar(.8)
frameRPPA<- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(tklabel(frameRPPA, text="Threshold of RPPA correlation :"))
tkgrid(frameRPPA)
tkgrid.configure(frameRPPA, sticky="new")

cbValThrRPPA <- tclVar("0")
cbThrRPPA <- tkcheckbutton(frameRPPA)
tkconfigure(cbThrRPPA,variable=cbValThrRPPA, text="")

ScaleRPPA <- tkscale(frameRPPA,length=200,from=0.5,to=1,showvalue=TRUE,
                     variable=RPPA_Entry,resolution=.05,orient='horiz')

tkgrid(ScaleRPPA,cbThrRPPA)
tkgrid.configure(cbThrRPPA,sticky="s",column=1)

##  miRNA frame
miRNA_Entry  = tclVar(.8)
framemiRNA<- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(tklabel(framemiRNA, text="Threshold of miRNA correlation :"))
tkgrid(framemiRNA)
tkgrid.configure(framemiRNA, sticky="new")

cbValThrmiRNA <- tclVar("0")
cbThrmiRNA <- tkcheckbutton(framemiRNA)
tkconfigure(cbThrmiRNA,variable=cbValThrmiRNA, text="")

ScalemiRNA <- tkscale(framemiRNA,length=200,from=0.5,to=1,showvalue=TRUE,
                      variable=miRNA_Entry,resolution=.05,orient='horiz')

tkgrid(ScalemiRNA, cbThrmiRNA)
tkgrid.configure(cbThrmiRNA,sticky="s",column=1)

## Mutation frequency frame
#Mut_Entry  = tclVar(median(myGlobalEnv$Freq_DfMutData))
Mut_Entry  = tclVar("14")
frameMut<- tkframe(ttCircos,relief="groove",borderwidth=2)
tkgrid(tklabel(frameMut, text="Threshold of Mutation Frequency:"))
tkgrid(frameMut)
tkgrid.configure(frameMut, sticky="new")

cbValThrMut <- tclVar("0")
cbThrMut <- tkcheckbutton(frameMut)
tkconfigure(cbThrMut,variable=cbValThrMut, text="")

ScaleMut <- tkscale(frameMut,length=200,from=0,to=max(myGlobalEnv$Freq_DfMutData, na.rm=TRUE),showvalue=TRUE,
                    variable=Mut_Entry,resolution=1,orient='horiz')

tkgrid(ScaleMut, cbThrMut)
tkgrid.configure(cbThrMut,sticky="s",column=1)


okOn <- function(){
    ## checkBox for Dimensions
    cbValuemRNA <- as.character(tclvalue(cbValmRNA))
    cbValueCNA <- as.character(tclvalue(cbValCNA))
    cbValueHM450 <- as.character(tclvalue(cbValHM450))
    cbValueHM27 <- as.character(tclvalue(cbValHM27))
    cbValueRPPA <- as.character(tclvalue(cbValRPPA))
    cbValuemiRNA <- as.character(tclvalue(cbValmiRNA))
    cbValueMut <- as.character(tclvalue(cbValMut))
    
    
    if(cbValuemRNA=="1"){mRNA <- 1}else if(cbValuemRNA=="0"){mRNA <- 0}
    if(cbValueCNA=="1"){CNA <- 1}else if(cbValueCNA=="0"){CNA <- 0}
    if(cbValueHM450=="1"){HM450 <- 1}else if(cbValueHM450=="0"){HM450 <- 0}    
    if(cbValueHM27=="1"){HM27 <- 1}else if(cbValueHM27=="0"){HM27 <- 0}
    if(cbValueRPPA=="1"){RPPA <- 1}else if(cbValueRPPA=="0"){RPPA <- 0}
    if(cbValuemiRNA=="1"){miRNA <- 1}else if(cbValuemiRNA=="0"){miRNA <- 0}
    if(cbValueMut=="1"){Mut <- 1}else if(cbValueMut=="0"){Mut <- 0}
    
    myGlobalEnv$ReturnCBoxCircos <-c(mRNA,CNA,HM450,HM27,RPPA,miRNA,Mut)
    
    
    ## CheckBox for Thrshold
    cbvaluemRNA <- as.character(tclvalue(cbValThrmRNA))
    cbvalueCNA <- as.character(tclvalue(cbValThrCNA))
    cbvalueHM450 <- as.character(tclvalue(cbValThrHM450))
    cbvalueHM27 <- as.character(tclvalue(cbValThrHM27))
    cbvalueRPPA <- as.character(tclvalue(cbValThrRPPA))
    cbvaluemiRNA <- as.character(tclvalue(cbValThrmiRNA))
    cbvalueMut <- as.character(tclvalue(cbValThrMut))
    
    
    if(cbvaluemRNA=="1"){ThrmRNA <- 1}else if(cbvaluemRNA=="0"){ThrmRNA <- 0}
    if(cbvalueCNA=="1"){ThrCNA <- 1}else if(cbvalueCNA=="0"){ThrCNA <- 0}
    if(cbvalueHM450=="1"){ThrHM450 <- 1}else if(cbvalueHM450=="0"){ThrHM450 <- 0}    
    if(cbvalueHM27=="1"){ThrHM27 <- 1}else if(cbvalueHM27=="0"){ThrHM27 <- 0}
    if(cbvalueRPPA=="1"){ThrRPPA <- 1}else if(cbvalueRPPA=="0"){ThrRPPA <- 0}
    if(cbvaluemiRNA=="1"){ThrmiRNA <- 1}else if(cbvaluemiRNA=="0"){ThrmiRNA <- 0}
    if(cbvalueMut=="1"){ThrMut <- 1}else if(cbvalueMut=="0"){ThrMut <- 0}
    
    myGlobalEnv$ReturnCBoxThrCircos <-c(ThrmRNA,ThrCNA,ThrHM450,ThrHM27,ThrRPPA,ThrmiRNA,ThrMut)
    
    
   #Entry Threshold of mRNA correlation
    ThreshmRNA <- as.numeric(tclvalue(mRNA_Entry))
    
    #Entry Threshold of CNA correlation
    ThreshCNA <- as.numeric(tclvalue(CNA_Entry))
    
    #Entry Threshold of Met HM450 correlation
    ThreshHM450 <- as.numeric(tclvalue(HM450_Entry))
    
    #Entry Threshold of Met HM27 correlation
    ThreshHM27 <- as.numeric(tclvalue(HM27_Entry))
    
    #Entry Threshold of RPPA correlation
    ThreshRPPA <- as.numeric(tclvalue(RPPA_Entry))
    
    #Entry Threshold of miRNA correlation
    ThreshmiRNA <- as.numeric(tclvalue(miRNA_Entry))
    
    #Entry Threshold of miRNA correlation
    ThreshMut <- as.numeric(tclvalue(Mut_Entry))
    
    myGlobalEnv$ReturnThreshCircos <-c(ThreshmRNA,ThreshCNA,ThreshHM450,ThreshHM27,ThreshRPPA, ThreshmiRNA,ThreshMut) 
   
    tkdestroy(ttCircos)
}

Ok.but <- tkbutton(ttCircos, text= "OK", command= okOn)


tkgrid(Ok.but)
tkgrid.configure(Ok.but, sticky="n", column=0)

tkwait.window(ttCircos)

}
