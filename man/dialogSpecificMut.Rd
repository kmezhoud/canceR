\name{dialogSpecificMut}
\alias{dialogSpecificMut}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
dialog box tp Specify Mutation using Regular Expression.
}
\description{
Search specific mutation using regular expression.
}

%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MutData}{
%%     ~~Describe \code{MutData} here~~
}
  \item{c}{
%%     ~~Describe \code{c} here~~
}
}

\seealso{
getSpecificMut
}

\examples{
## Not run

##Load Session
load(paste(path.package("canceR"),"/data/brca_tcga73genes.RData", sep=""))
## Select Case
myGlobalEnv$curselectCases <- 2
##Select Genetic Profile
myGlobalEnv$curselectGenProfs <- 9
## get Specific Mutation data for 73 Genes list
##getSpecificMut()
##dialogSpecificMut()
## End(Not run)
}
