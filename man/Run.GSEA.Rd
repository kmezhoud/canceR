\name{Run.GSEA}
\alias{Run.GSEA}

\title{
The  main function to run GSEA-R from Broad Institute
}
\description{
this function runs the GSEA-R version of Broad Institute at (\url{http://www.broadinstitute.org/gsea/index.jsp})
}

\usage{
Run.GSEA()
}

\examples{
\dontrun{
library(canceR)
## Load workspace
load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))
##Run.GSEA()
}
}