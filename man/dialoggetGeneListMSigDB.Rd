\name{dialoggetGeneListMSigDB}
\alias{dialoggetGeneListMSigDB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
 Multi-select choice of gene sets from loaded MSigDB
}
\description{
This function
}
\usage{
dialoggetGeneListMSigDB(MSigDB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{MSigDB}{
\code{dialoggetGeneListMSigDB(MSigDB)}
}
}

\value{

 \item{myGlobalEnv$regex}{A list of Gene Sets}
}

\author{
Karim Mezhoud
}

\seealso{
getGeneListMSigDB()
}
\examples{
\dontrun{
library(canceR)
getGeneListMSigDB()
head(myGlobalEnv$GeneListMSigDB)
head(myGlobalEnv$GeneList)
}
}

