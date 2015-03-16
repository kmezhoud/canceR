\name{getMutData}
\alias{getMutData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get Mutation Data for gene list.
}
\description{
User needs to select gene list, all samples with sequencing data from cases and Mutations from Genetic profiles
}

\examples{
\dontrun{
##Load Session
load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case
myGlobalEnv$curselectCases <- 2
##Select Genetic Profile
myGlobalEnv$curselectGenProfs <- 9
## get Mutation data for 73 Genes list
getMutData()
}
}