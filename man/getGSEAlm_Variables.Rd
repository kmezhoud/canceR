\name{getGSEAlm_Variables}
\alias{getGSEAlm_Variables}

\title{
Get Gene Set Enrichment Analysis linear modeling for two phenotypes (variables) in the same disease. 
}
\description{
Get Gene Set Enrichment Analysis linear modeling for two phenotypes (variables) in the same disease. Users needs to browse MSigDB file from  ~R/x86_64-unknown-linux-gnu-library/3.2/canceR/extdata/MSigDB/c2.cp.reactome.v4.0.symbols.gmt. And check DFS_STATUS. The results change depending the profile data and cbio portal updates.

}
\usage{
getGSEAlm_Variables()
}

\examples{
\dontrun{
##Load Session
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case from Breast Cancer
##myGlobalEnv$curselectCases <- 9
##Select Genetic Profile from Breast Cancer
##myGlobalEnv$curselectGenProfs <- 4
## get GSEAlm data for 73 Genes list
##library(GSEAlm)
##getGSEAlm_Variables()
}
}