#' Display matrix in tcltk table
#' @usage
#' displayInTable(tclarray,title="",height=-1,width=-1,nrow=-1,ncol=-1)
#'
#' @param tclarray a dataframe
#' @param title title of the table
#' @param height -1
#' @param width -1
#' @param nrow -1
#' @param ncol -1
#' 
#' @return display a Table
#' @export
#'
#' @examples
#' data(ClinicalData)
#' \dontrun{
#' getInTable(Table= ClinicalData, title= "Clinical Data")
#' }
displayInTable <- function(tclarray,title="",height=-1,width=-1,nrow=-1,ncol=-1)
{
    #require(tcltk)
    tt <- tktoplevel()
    tkwm.resizable(tt, TRUE, TRUE)
    #tkwm.geometry(tt, "900x900")
    tclRequire("Tktable")
    tkwm.title(tt,title)
    table1 <- tkwidget(tt,"table",rows=nrow,cols=ncol,titlerows=1,titlecols=1,
                       height=height+1,width=width+3,
                       xscrollcommand=function(...) tkset(xscr,...),yscrollcommand=function(...) tkset(yscr,...))
    xscr <-tkscrollbar(tt,orient="horizontal", command=function(...)tkxview(table1,...))
    yscr <- tkscrollbar(tt,command=function(...)tkyview(table1,...))
    
    tkgrid(table1,yscr)
    tkgrid.configure(yscr,sticky="nsw")
    tkgrid(xscr,sticky="new")
    tkconfigure(table1,variable=tclarray,background="white",selectmode="extended")
    tkconfigure(table1,selectmode="extended",rowseparator="\"\n\"",colseparator="\"\t\"")
    
    
    ##To control whether rows and/or columns can be resized
    tkconfigure(table1,resizeborders="both") 
    return (table1)
}
