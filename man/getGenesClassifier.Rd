% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getGenesClassifier.R
\name{getGenesClassifier}
\alias{getGenesClassifier}
\title{get Genes Classifier}
\usage{
getGenesClassifier()
}
\value{
a data frma with genes classes
}
\description{
get Genes Classifier
}
\examples{
x <- 0
\dontrun{
readRDS(paste(path.package("canceR"),"/extdata/rdata/brca_tcga73genes.rds", sep=""))
getGenesClassifier()
}
}
