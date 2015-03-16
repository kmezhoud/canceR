\name{dialogOptionPhenoTest}
\alias{dialogOptionPhenoTest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Dialog Box to specify  Variables for PhenoTest function.
}
\description{
Specifying PhenoTest Options.
}


\arguments{
  \item{eSet}{
 Expression Set function to built eSet from gene expression and clinical data.
}
}

\seealso{
PhenoTest package
}


\examples{
\dontrun{
library(canceR)
library(Biobase)
library(phenoTest)
## Load workspace
load(paste(path.package("canceR"),"/data/prad_michPhenoTest1021.RData", sep=""))

getPhenoTest()
## Empty Survival raviables
## Select DSF_STATUS for categorical variable
## Select PSA Level for Numeric variale
## BH 
##dialogOptionPhenoTest()
}
}