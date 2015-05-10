\name{dialogOptionCircos}
\alias{dialogOptionCircos}

\title{
Select with dimensions to plot
}
\description{
This dialg Box allow user to select which dimensions to plot and gene Symbol using Threshold of correlation rate of every dimension.
}
\usage{
dialogOptionCircos()
}

\author{
Karim Mezhoud
}

\seealso{
getCircos(dimension="All")
}
\examples{
\dontrun{
library(canceR)
dialogOptionCircos()
getCircos()
myGlobalEnv$ReturnCBoxCircos
myGlobalEnv$ReturnCBoxThrCircos
myGlobalEnv$ReturnThreshCircos
}
}
