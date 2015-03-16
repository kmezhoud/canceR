\name{getGCT_CLSfiles}
\alias{getGCT_CLSfiles}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ get Profile (GCT file) and Phenotype (CLS file) Data from Disease.
}
\description{
This function load gene expression data frame and clinical data and built GCT, CLS files and save them in /Results/GCT_CLS folder.
}

\examples{
\dontrun{
library(canceR)
## Load workspace
load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))

##getGCT_CLSfiles()
}
}