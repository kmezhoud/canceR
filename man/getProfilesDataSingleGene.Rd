\name{getProfilesDataSingleGene}
\alias{getProfilesDataSingleGene}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get Profiles Data  for a Single Gene.
}
\description{
Get profile data for single gene. Users must writea rigth Symbol gene in dialog box.
}

\seealso{
modalDialog
}

\examples{
## Not run
##Load Session
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case from Breast Cancer
myGlobalEnv$curselectCases <- 9
##Select Genetic Profile from Breast Cancer
myGlobalEnv$curselectGenProfs <- 4
## get Specific Mutation data for 73 Genes list
##getProfilesDataSingleGene()
## End(Not run)
}