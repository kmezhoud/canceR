\name{getCor_ExpCNAMet}
\alias{getCor_ExpCNAMet}

\title{
Get gene correlation for multiple dimensions.
}
\description{
Get Correlation of the same gene between two diseases from matrices of mRNA, CNA and Methylation Datadescription}

\usage{
getCor_ExpCNAMet(ListMatrix, dimension)
}

\arguments{
  \item{ListMatrix}{
 \code{ is a List of numeric matrices }
}
  \item{dimension}{
         \code{Exp: get correlation for mRNA Profiles Data}
         \code{CNA: get correlation for CNA Profiles Data}
         \code{Met: get correlation for Methylation Profiles Data}
         \code{miRNA: get correlation for miRNA Profiles Data}
         \code{RPPA: get correlation for RPPA Profiles Data} 
}        
}

\author{
Karim Mezhoud
}

\seealso{
getCircos(dimension= "All")
}
\examples{
\dontrun{
library(canceR)
load(paste(path.package("canceR"),"/data/Circos.RData", sep=""))
getListProfData()
getCor_ExpCNAMet(myGlobalEnv$ListProfData$CNA, dimension="CNA")
head(myGlobalEnv$Cor_CNA)
head(myGlobalEnv$Cor_Exp)
}
}

