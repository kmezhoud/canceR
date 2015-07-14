\name{getListProfData}
\alias{getListProfData}

\title{
get a list of Profile Data of every available dimensions;
}
\description{
This function load matrices of every dimension (Exp, CNA, Met, RPPA,miRNA,Mut) and save them in a list for every disease. 
}
\usage{
getListProfData()
}
\author{
Karim Mezhoud
}
\value{

\item{myGlobalEnv$ProfData$Expression }{Matrices of mRNA expression}
\item{myGlobalEnv$ProfData$CNA }{Matrices of Copy Number Alteration}
\item{myGlobalEnv$Met }{Matrices of Methylation Data}
\item{myGlobalEnv$ProfData$RPPA }{Matrices of Reverse Phase Protein Activities}
\item{myGlobalEnv$ProfData$miRNA }{Matrices of miRNA expression}
\item{myGlobalEnv$Mut }{Matrices of Mutation Data}

}
\examples{
\dontrun{
library(canceR)
getListProfData()
head(myGlobalEnv$ProfData$Expression)
}
}
