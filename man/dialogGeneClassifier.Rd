\name{dialogGeneClassifier}
\alias{dialogGeneClassifier}

\title{
dialogGeneClassifier
}
\description{
Dialog box to get gene classifier options.
}
\usage{
dialogGeneClassifier(Lchecked_Cases, entryWidth = 10, returnValOnCancel = "ID_CANCEL")
}

\arguments{
  \item{Lchecked_Cases}{
\code{Number of checked Cases} 
}
  \item{entryWidth}{
 \code{10} 
}
  \item{returnValOnCancel}{
\code{"ID_CANCEL"} 
}
}
\value{
Value
}

\examples{
## Not run
## Load workspace
load(paste(path.package("canceR"),"/data/brca_tcgaClassifier223.RData", sep=""))
##getGenesClassifier()
##dialogGeneClassifier
## End(Not run)
}