% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getMetDataMultipleGenes.R
\name{getMetDataMultipleGenes}
\alias{getMetDataMultipleGenes}
\title{get Methylation data for multiple genes}
\usage{
getMetDataMultipleGenes()
}
\value{
a a dataframe with mean and median of methylation rate (threshold of silencing gene)
}
\description{
get Methylation data for multiple genes
}
\examples{
readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
\dontrun{
getMetDataMultipleGenes()
}
}
