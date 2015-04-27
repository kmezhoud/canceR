\name{getProfilesDataMultipleGenes}
\alias{getProfilesDataMultipleGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get Profiles Data of Multiple Genes.
}
\description{
This function get Profile data for Gene list and save table in workspace/Results/ProfileData
}
\usage{
getProfilesDataMultipleGenes(getSummaryGSEAExists)
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
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case from Breast Cancer
##myGlobalEnv$curselectCases <- 9
##Select Genetic Profile from Breast Cancer
##myGlobalEnv$curselectGenProfs <- 4
##getProfilesDataMultipleGenes(getSummaryGSEAExists=0)
}
}