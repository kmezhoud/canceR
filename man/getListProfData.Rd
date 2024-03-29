% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getListProfData.R
\name{getListProfData}
\alias{getListProfData}
\title{Get list of data frame with profiles data (CNA,mRNA, Methylation, Mutation...)}
\usage{
getListProfData(checked_Studies, geneList)
}
\arguments{
\item{checked_Studies}{checked studies in corresponding panel (input$StudiesIDCircos, input$StudiesIDReactome).}

\item{geneList}{GeneList with Hugo Symbol}
}
\value{
A LIST of profiles data (CNA, mRNA, Methylation, Mutation, miRNA, RPPA).
        Each dimension content a list of studies.
}
\description{
Get list of data frame with profiles data (CNA,mRNA, Methylation, Mutation...)
}
\examples{
readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
\dontrun{
getListProfData()
head(ENV$ProfData$Expression)
}

}
