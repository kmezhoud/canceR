\name{dialogSummary_GSEA}
\alias{dialogSummary_GSEA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dialog Box to specify phenotype (variable) used in last GSEA-R to get Summary Results.
}
\description{
This function ask the user to specify the phenotype (variable).
}
\usage{
dialogSummary_GSEA(Variable, returnValOnCancel = "ID_CANCEL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Variable}{
%%     ~~Describe \code{Variable} here~~
}
  \item{returnValOnCancel}{
%%     ~~Describe \code{returnValOnCancel} here~~
}
}

\examples{
\dontrun{
library(canceR)
## Load workspace
load(paste(path.package("canceR"),"/data/ucec_tcga_pubGSEA1021.RData", sep=""))
##Run.GSEA()
##getSummaryGSEA()

}
}