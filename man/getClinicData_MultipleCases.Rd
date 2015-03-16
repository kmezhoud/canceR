\name{getClinicData_MultipleCases}
\alias{getClinicData_MultipleCases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get Clinical Data for Multiple Cases
}
\description{
User needs to select at least one case to run this function. Get clinical data for more one or multiple cases.
}
\usage{
getClinicData_MultipleCases(getSummaryGSEAExists)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{getSummaryGSEAExists}{
 if equal to 0, the clinical data is displayed in table. if the argument is equal to 1, the clinical data is used to summarise GSEA analysis results.
}
}


\examples{
\dontrun{
##Load Session
load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case
myGlobalEnv$curselectCases <- 2
## get Clinical data
##getClinicData_MultipleCases(getSummaryGSEAExists = 0)
}
}