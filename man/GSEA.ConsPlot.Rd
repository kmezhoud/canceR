\name{GSEA.ConsPlot}
\alias{GSEA.ConsPlot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GSEA.ConsPlot
}
\description{
GSEA.ConsPlot
}
\usage{
GSEA.ConsPlot(V, col.names, main = " ", sub = " ", xlab = " ", ylab = " ")
}
%GSEA.ConsPlot(Itable, col.names = set.table.phen1[1:max.sets.phen1, 2], main = " ", sub=paste("Leading Subsets Overlap ", doc.string, " - ", phen1, sep=""), xlab=" ", ylab=" ")
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{V}{
   \code{V="Itable"} 
}
  \item{col.names}{
     \code{col.names = colnames} 
}
  \item{main}{
     \code{main= " "} 
}
  \item{sub}{
\code{sub = " "} 
}
  \item{xlab}{
    \code{xlab= " "} 
}
  \item{ylab}{
    \code{ylab = " "} 
}
}
\value{
GSEA.ConsPlot
}
\examples{
\dontrun{
library(canceR)
## Load workspace
readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
##Run.GSEA()
}
}