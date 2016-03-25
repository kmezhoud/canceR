\name{ClinicalData}
\alias{ClinicalData}
\docType{data}
\title{
ClinicalData
}
\description{
Example of Clinical Data
}
\usage{data("ClinicalData")}
\format{
  A data frame with 770 observations on the following 4 variables.
  \describe{
    \item{\code{DFS_MONTHS}}{a numeric vector}
    \item{\code{DFS_STATUS}}{a factor with levels \code{DiseaseFree} \code{Recurred/Progressed}}
    \item{\code{OS_MONTHS}}{a numeric vector}
    \item{\code{OS_STATUS}}{a factor with levels \code{DECEASED} \code{LIVING}}
  }
}

\source{
cbioportal
}


\keyword{datasets}

\value{
a dataframe with clinical data
}
\examples{

data("ClinicalData")

}