\name{modalDialog}
\alias{modalDialog}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Dialog box to specify Gene Symbol.
}
\description{
This function get Profile Data for one gene Symbol.
}
\usage{
modalDialog(title, question, entryInit, entryWidth = 40, returnValOnCancel = "ID_CANCEL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{question}{
%%     ~~Describe \code{question} here~~
}
  \item{entryInit}{
%%     ~~Describe \code{entryInit} here~~
}
  \item{entryWidth}{
%%     ~~Describe \code{entryWidth} here~~
}
  \item{returnValOnCancel}{
%%     ~~Describe \code{returnValOnCancel} here~~
}
}

\seealso{
getProfilesDataSingleGene
}

\examples{
\dontrun{
##Load Session
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case from Breast Cancer
##myGlobalEnv$curselectCases <- 9
##Select Genetic Profile from Breast Cancer
##myGlobalEnv$curselectGenProfs <- 4
## get Specific Mutation data for 73 Genes list
##getProfilesDataSingleGene()
}
}