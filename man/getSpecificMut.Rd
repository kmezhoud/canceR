\name{getSpecificMut}
\alias{getSpecificMut}

\title{
get Specific Mutation using Amino Acid changes.
}
\description{
This function search only mutation with specific Amino Acid changes using Regular Expression. User needs to specify Amino Acid change in dialog box.
}

\seealso{
dilogSpecificMut
}

\examples{
\dontrun{
##Load Session
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case
##myGlobalEnv$curselectCases <- 2
##Select Genetic Profile
##myGlobalEnv$curselectGenProfs <- 9
## get Specific Mutation data for 73 Genes list
##getSpecificMut()
}
}
