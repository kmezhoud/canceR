\name{getSurvival}
\alias{getSurvival}

\title{
Survival plot
}
\description{
This function plot Kaplan-Meier curves and Cox proportional Hazard model.
}
\usage{
getSurvival(Coxph)
}

\arguments{
  \item{Coxph }{
 if Coxph = 0 : plot Kaplan-Meier curves
 else Coxph= 1 : plot Cox Proportional Hazard Model 
}
}


\examples{
\dontrun{
load("~/CGDS-R/canceR/data/gbm_tcgaPlotTwoGenProf.RData")

getSurvival(Coxph = 1)
getSurvival(Coxph = 0)
}
}

