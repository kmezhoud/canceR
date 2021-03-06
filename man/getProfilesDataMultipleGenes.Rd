% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getProfilesDataMultipleGenes.R
\name{getProfilesDataMultipleGenes}
\alias{getProfilesDataMultipleGenes}
\title{get Profles Data of multiple genes}
\usage{
getProfilesDataMultipleGenes(getSummaryGSEAExists)
}
\arguments{
\item{getSummaryGSEAExists}{if equal to 0, the clinical data is displayed in table. if the argument is equal to 1, the clinical data is used to summarise GSEA analysis results.}
}
\value{
a file with a dataframe of profle data
}
\description{
get Profles Data of multiple genes
}
\examples{
readRDS(paste(path.package("canceR"),"/extdata/rdata/prad_michPhenoTest1021.rds", sep=""))
\dontrun{
getProfilesDataMultipleGenes(getSummaryGSEAExists = 0)
}

}
