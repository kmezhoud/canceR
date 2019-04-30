#' model plotting with tcltk
#' @usage plotModel(plotCommand, title= "TITLE",hscale=1, vscale=1 )
#' @param plotCommand plotcommand
#' @param title title of plot
#' @param hscale horizintal scale
#' @param vscale vertical scale
#' 
#' @return plot
#' @export
#' @examples 
#' load(paste(path.package("canceR"),"/data/gbm_tcgaPlotTwoGenProf.RData", sep=""))
#' \dontrun{
#' plot_1Gene_2GenProfs()
#' }
plotModel <- function(plotCommand, title= "TITLE",hscale=1, vscale=1 ){
    
    
    ttplot <- tktoplevel()
    tkwm.title(ttplot,title)
    img <- tkrplot::tkrplot(ttplot,fun=plotCommand,hscale=hscale,vscale=vscale)
    
    
    
    
    
    ####Save Graph Option
    rbValue3 <- tclVar("PNG") # le choix par défaut
    
    frameGraph <- tkframe(ttplot,relief="groove",borderwidth=2)
    tkgrid(frameGraph)
    tkgrid.configure(frameGraph, sticky="new")
    labelGraph<-(tklabel(frameGraph, text=" Specify format file:"))
    rb9 <- tkradiobutton(frameGraph)
    rb10 <- tkradiobutton(frameGraph)
    rb11 <- tkradiobutton(frameGraph)
    
    # config des boutons radio. Une seule variable tcl pour 3 boutons
    tkconfigure(rb9,variable=rbValue3,value="SVG", text="SVG")
    tkconfigure(rb10,variable=rbValue3,value="JPG", text="JPG")
    tkconfigure(rb11,variable=rbValue3,value="PNG", text="PNG")
    
    tkgrid(labelGraph,rb9, rb10, rb11,columnspan=4)
    tkgrid.configure(rb9,rb10,rb11, sticky="e")
    
    
    
    
    
    Save <- function(){
        rbVal3 <- as.character(tclvalue(rbValue3))
        tkdestroy(ttplot)
        if (rbVal3=="SVG")
            #tkmessageBox(message=rbVal2)
            myGlobalEnv$graph<-paste(rbVal3)
        if (rbVal3=="JPG")
            #tkmessageBox(message=rbVal2)
            myGlobalEnv$graph<-paste(rbVal3)
        if (rbVal3=="PNG")
            #tkmessageBox(message=rbVal2)
            myGlobalEnv$graph<-paste(rbVal3)
        
        #fileName <- tclvalue(tkgetSaveFile(initialfile = "plot.svg",filetypes = "{{SVG Files} {.svg}} {{JPEG Files} {.jpg .jpeg}} {{PNG Files} {.png}}  {{All files} *}")) 
        if (myGlobalEnv$graph=="SVG"){
            fileName <- tclvalue(tkgetSaveFile(initialfile = "plot.svg",filetypes = "{{SVG Files} {.svg}}")) 
        } else if (myGlobalEnv$graph=="JPG"){
            fileName <- tclvalue(tkgetSaveFile(initialfile = "plot.jpg",filetypes = "{{JPEG Files} {.jpg .jpeg}}")) 
        } else if (myGlobalEnv$graph=="PNG"){
            fileName <- tclvalue(tkgetSaveFile(initialfile = "plot.png",filetypes = "{{PNG Files} {.png}} ")) 
            
        }
        if (!nchar(fileName)) {
            tkmessageBox(message = "No file was selected!")
        } else {
            tkmessageBox(message = paste("The file selected was", fileName))
        }
        ##myGlobalEnv$graph file type Option
        if(myGlobalEnv$graph=="SVG"){
            #library(RSvgDevice) Doesn't work with windows OS
            #RSvgDevice::devSVG(fileName,width = 600 / 300, height = 600 / 300)
            if(Sys.info()['sysname'] != "windows"){
            #require(RSvgDevice)
            devSVG(fileName,width = 600 / 300, height = 600 / 300)
            plotCommand()
            graphics.off()
            }
        } else if (myGlobalEnv$graph=="JPG"){
            jpeg(fileName, width=7, height=7,units="in", res=300)
            plotCommand()
            dev.off()
        } else if (myGlobalEnv$graph=="PNG"){
            png(fileName,width=7, height=7,units="in", res=300)
            plotCommand()
            dev.off()
        }
    }
    
    tkgrid(frameGraph)
    tkgrid.configure(frameGraph, sticky="e")
    
    tkgrid(img)
    Save.but <- tkbutton(ttplot,text="Save",command=Save)
    
    tkgrid(Save.but)
    #tkgrid(copy.but, Save.but, column="0")
    #tkgrid.configure(copy.but, sticky="w")
    tkgrid.configure(Save.but,sticky="e")
    
}