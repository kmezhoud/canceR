\name{dialogMetOption}
\alias{dialogMetOption}

\title{
dialog box to specify Methylation rate threshold of silencing genes.
}
\description{
dialog box to specify Methylation rate threshold of silencing genes.
}


\arguments{
  \item{ProfData}{

}
  \item{k: integer}{

}
}

\seealso{
getMetDataMultipleGenes
}

\examples{
## Not run
##Load Session
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case from Prostate Cancer
##myGlobalEnv$curselectCases <- 19
##Select Genetic Profile from Prostate Cancer
##myGlobalEnv$curselectGenProfs <- 17
## get  Methylation data for 73 Genes list
##getMetDataMultipleGenes()
##dialogMetOption(myGlobalEnv$ProfData,1)
## End(Not run)
}