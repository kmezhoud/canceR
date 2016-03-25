#' get text in tcltk windows
#' @usage getTextWin(text)
#' @param text string
#' 
#' @return tcltk windows with text
#' @export
#' @examples 
#' text <- "mytext"
#' \dontrun{
#' getTextWin(text) 
#' }
getTextWin <- function(text){
    #require(tcltk)
    tt  <- tktoplevel()
    xscr <- tkscrollbar(tt, repeatinterval=5,orient="horizontal",
                        command=function(...)tkxview(txt,...))
    yscr <- tkscrollbar(tt, repeatinterval=5,
                        command=function(...)tkyview(txt,...))
    txt <- tktext(tt,bg="white",font="courier",
                  xscrollcommand=function(...)tkset(xscr,...),yscrollcommand=function(...)tkset(yscr,...),
                  wrap="none")
    tkgrid(txt,yscr)
    tkgrid(xscr)
    tkgrid.configure(yscr,sticky="ns")
    tkgrid.configure(xscr,sticky="ew")
    for (i in (1:100)) tkinsert(txt,"end",paste(text))
    tkconfigure(txt, state="")
    tkfocus(txt)
    
}