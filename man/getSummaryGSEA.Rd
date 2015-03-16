\name{getSummaryGSEA}
\alias{getSummaryGSEA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
get Summary  results from GSEA-R (Broad Institute)
}
\description{
This function select only significant  modulated gene set and display them in table. user needs to specify save files from Run.GSEA function in /Results/GSEA/Disease/.
}



\references{

Karim Mezhoud  canceR: A Graphical User Interface for accessing and modeling the MSKCC Cancer Genomics Data.
}
\author{
Karim Mezhoud
}


\examples{
## Not run
##library(canceR)
## Load workspace
##load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))
##Run.GSEA()
##getSummaryGSEA()
## End(Not run)
}