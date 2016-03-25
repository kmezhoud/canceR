\name{GSEA}
\alias{GSEA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GSEA-R (Broad Institute)
}
\description{
 See \url{http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/R-GSEA_Readme}
}

\author{
Subramanian, Tamayo, et al. (2005, PNAS 102, 15545-15550) and Mootha,
Lindgren, et al. (2003, Nat Genet 34, 267-273)
}
\value{
GSEA
}


\examples{
\dontrun{
library(canceR)
## Load workspace
load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))
##Run.GSEA()
}
}