\name{getMetDataMultipleGenes}
\alias{getMetDataMultipleGenes}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get Methylation Data for Multiple Genes.
}
\description{
Get methylation data for gene list. User needs to specify correlation rate  threshold between Methylation and Silencing of genes(default rate: 0.8).
}

\seealso{
dialogMetOption
}

\examples{
\dontrun{
##Load Session
load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case from Prostate Cancer
myGlobalEnv$curselectCases <- 19
##Select Genetic Profile from Prostate Cancer
myGlobalEnv$curselectGenProfs <- 17
## get  Methylation data for 73 Genes list
getMetDataMultipleGenes()
}
}
