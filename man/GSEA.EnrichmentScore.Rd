\name{GSEA.EnrichmentScore}
\alias{GSEA.EnrichmentScore}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GSEA.EnrichmentScore
}
\description{
GSEA.EnrichmentScore
}
\usage{
GSEA.EnrichmentScore(gene.list, gene.set, weighted.score.type = 1, correl.vector = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{gene.list}{
%%     ~~Describe \code{gene.list} here~~
}
  \item{gene.set}{
%%     ~~Describe \code{gene.set} here~~
}
  \item{weighted.score.type}{
%%     ~~Describe \code{weighted.score.type} here~~
}
  \item{correl.vector}{
%%     ~~Describe \code{correl.vector} here~~
}
}
\value{
GSEA.EnrichmentScore
}

\examples{
\dontrun{
library(canceR)
## Load workspace
readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
##Run.GSEA()
}
}