#' about canceR
#' @usage about()
#' @return dialig box with text
#' @export
#'
#' @examples
#' \dontrun{
#' about()
#'}
#'


about <-
    function() {
        
        fontinfo <- tkfont.create(family="times",size=11)
        fontinfo2 <- tkfont.create(family="times",size=13,weight="bold")
        
        abt <- tktoplevel()
        text1 <-tkframe(abt ,borderwidth=2)
        text2 <-tkframe(abt ,borderwidth=2)
        text3 <-tkframe(abt ,borderwidth=2)
        
        
        tkwm.title(abt ," About   ")
        tkgrid(tklabel(abt ,text="    About   ",font=fontinfo2))
        
        tkgrid(tklabel(text1 ,text=" The canceR package is developed by Karim Mezhoud Team (kmezhoud@gmail.com) 
 aimed to make easier access to the Cancer Data hosted by the Computational Biology Center 
at Memorial-Sloan-Kettering Cancer Center (MSKCC) at http://www.cbioportal.org/public-portal/.",font=fontinfo))
        
        tkgrid(tklabel(text2 ,text=" The canceR package is a Graphical User Interface (GUI) developed for users 
with no or limited knowledge about R programming so they can run all functions from cgdsr package
and perform modeling analysis depending on others packages (see depends).",font=fontinfo))
        
        tkgrid(tklabel(text3 ,text=" Use the cbioportal@googlegroups.com mailing list to obtain additional support.",font=fontinfo))
        #tkgrid(tklabel(text3 ,text=" More detail information of the canceR project can be found in: http://www.bioinformatics.tn",font=fontinfo))
        
        tkgrid.configure(text1 ,sticky="new")
        tkgrid.configure(text2 ,sticky="new")
        tkgrid.configure(text3 ,sticky="new")
        
    }
