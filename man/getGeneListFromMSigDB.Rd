\name{getGeneListFromMSigDB}
\alias{getGeneListFromMSigDB}

\title{
Get Gene List from MSigDB.gmt file.
}
\description{
This function builts data frame with two columns (Gene Setsan and Genes). This function deletes duplicate genes through gene sets. The input file is MSigDb.gmt file.  
}
\usage{
getGeneListFromMSigDB()
}


\author{
Karim Mezhoud
}

\value{

\item{myGlobalEnv$GeneListMSigDB }{Data frame of genes sets and its genes}
}

\examples{
\dontrun{
library(canceR)
getGeneListFromMSigDB()
head(myGlobalEnv$GeneListMSigDB)
}
}

