\name{getCircos}
\alias{getCircos}

\title{
Get Circos plot of multiple dimensions.
}
\description{
This function plot correlation rate between diseases of multiple dimensions (mRNA, CNA, Met,RPPA, miRNA, Mut) 
}
\usage{
getCircos(dimension)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dimension}{
\code{All} 
}
}

\author{
Karim Mezhoud
}


\examples{
\dontrun{
library(canceR)
##Load Session
load(paste(path.package("canceR"),"/data/Circos.RData", sep=""))
##getListProfData()
getCircos(dimension ="All")
}
}