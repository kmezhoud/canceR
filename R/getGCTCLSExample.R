#' get GCT and CLS example files.
#' @usage getGCTCLSExample()
#' @return GCT and CLS files
#' @export
#' @examples
#' ## Load workspace
#' readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
#' \dontrun{
#' getGCTCLSExample()
#' }
getGCTCLSExample <- function(){
    #require(tcltk)
    
    ifrm <- function(obj, env = ENV()) {
        obj <- deparse(substitute(obj))
        if(exists(obj, envir = env)) {
            rm(list = obj, envir = env)
        }
    }
    ifrm(fname.GCT, ENV)
    ifrm(fname.CLS,ENV)
    
    ENV$fname.GCT <- paste(path.package("canceR"),"/extdata/gct_cls/", "Lung_Bost_collapsed_symbols_common_Mich_Bost.gct",sep="")
    ENV$fname.CLS <- paste(path.package("canceR"),"/extdata/gct_cls/", "Lung_Boston.cls",sep="")
    
    
    
    tkinsert(ENV$tlGCT,"end",ENV$fname.GCT)
    tkinsert(ENV$tlCLS,"end",ENV$fname.CLS)
    tkfocus(ENV$ttDialogGSEA)
    
}