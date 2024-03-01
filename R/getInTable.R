#' get dataframe in TK/TCL table
#' @usage
#' getInTable(table,title)
#' @param table Dataframe
#' @param title string a title of the table
#' @return display a Table
#' @export
#'
#' @examples
#' #data(ClinicalData)
#' \dontrun{
#' getInTable(Table= ClinicalData, title= "Clinical Data")
#' }

getInTable <- function(table,title){
matrix <-rbind(colnames(table), table)
matrix1 <- cbind(row.names(matrix), matrix)
matrix1 <- t(t(matrix1))

# Define a Tcl array and initialize it to that matrix :
tclArray1 <- tclArray()
for (i in (1:length(matrix1[,1]))){
    for (j in (1:length(matrix1[1,]))){
        tclArray1[[i-1,j-1]] <- matrix1[i,j]
    }
}
#Table_Title<- paste("SD:",StudyRefCase[k],"_","GenProf:",curselectGenProfs_forStudy[k],"_","CASE:",curselectCases_forStudy[k],".gct")
table1 <- displayInTable(tclArray1,title= title ,nrow=nrow(matrix1),ncol=ncol(matrix1),height=40,width=50)

}