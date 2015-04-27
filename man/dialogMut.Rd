\name{dialogMut}
\alias{dialogMut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dialog box to specify Animo Acid changes using regular expression 
}
\description{
dialog box to specify Animo Acid changes using regular expression.
}
\usage{
dialogMut(title, question, entryInit, entryWidth = 40, returnValOnCancel = "ID_CANCEL")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{title}{
%%     ~~Describe \code{title} here~~
}
  \item{question}{
 \code{question= enter gene Symbol} 
}
  \item{entryInit}{
        \code{entryInit= P53} 
}
  \item{entryWidth}{
\code{entryWidth = 10} 
}
  \item{returnValOnCancel}{
     \code{returnValOnCancel= ID_CANCEL} h
}
}

\seealso{
getMutData
}
\examples{
\dontrun{
## library(canceR)
##Load Session
##load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case
myGlobalEnv$curselectCases <- 2
##Select Genetic Profile
myGlobalEnv$curselectGenProfs <- 9
## get Mutation data for 73 Genes list
##getMutData()
##dialogMut("title", "question", "entryInit", entryWidth = 40, returnValOnCancel = "ID_CANCEL")
}
}