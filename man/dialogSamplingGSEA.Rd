\name{dialogSamplingGSEA}
\alias{dialogSamplingGSEA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dialog  Box for Sampling patients from expression profile data used for GSEA-R (Broad Institute)
}
\description{
Samoling Cases  from cbioportal for GSEA.
}
\usage{
dialogSamplingGSEA(Lchecked_Cases, entryWidth = 10, returnValOnCancel = "ID_CANCEL")
}

\arguments{
  \item{Lchecked_Cases}{
\code{Number of checked Cases} 
}
  \item{entryWidth}{
     \code{entryWidth=10} 
}
  \item{returnValOnCancel}{
\code{returnValOnCancel="ID_CANCEL"}
}
}


\examples{
\dontrun{
##library(canceR)
## Load workspace
##load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))
##Run.GSEA()
##dialogSamplingGSEA(1,entryWidth=10,returnValOnCancel = "ID_CANCEL")
}
}