\name{dialogOptionGSEAlm}
\alias{dialogOptionGSEAlm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dialog Box Option  for GSEAlm
}
\description{
Specifying Options for GSEA linear modeling: Variables/Phenotypes, Permutation and p-value
}
\usage{
dialogOptionGSEAlm(k, ClinicalData)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{k}{
%%     ~~Describe \code{k} here~~
}
  \item{ClinicalData}{
%%     ~~Describe \code{ClinicalData} here~~
}
}

\seealso{
getGSEAlm_Variables()
}
\examples{
\dontrun{
##Load Session
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case from Breast Cancer
##myGlobalEnv$curselectCases <- 9
##Select Genetic Profile from Breast Cancer
myGlobalEnv$curselectGenProfs <- 4
## get GSEAlm data for 73 Genes list
##library(GSEAlm)
#getGSEAlm_Variables()
##dialogOptionGSEAlm(k, ClinicalData)
}
}
