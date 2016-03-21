\name{geNetClassifier-package}
\alias{geNetClassifier-package}
\docType{package}
\title{classify diseases and build associated gene networks using gene expression profiles}
\description{
Comprehensive package to automatically train a multi-class SVM classifier based on gene expression data.
Provides transparent selection of gene markers, their coexpression networks, and an interface to query the classifier.
}
\details{
\tabular{ll}{
Package: \tab geNetClassifier\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2013-02-28\cr
License: \tab GPL (>=2)\cr
LazyLoad: \tab yes\cr
Depends:  \tab R (>= 2.10.1), Biobase (>= 2.5.5), EBarrays, minet, methods\cr
Imports: \tab e1071, ipred, graphics\cr
Suggests: \tab leukemiasEset\cr
Enhances: \tab RColorBrewer, igraph
}
}
\author{Sara Aibar, Celia Fontanillo and Javier De Las Rivas \cr
Bioinformatics and Functional Genomics Group. Centro de Investigacion del Cancer (CIC-IBMCC, CSIC/USAL). Salamanca. Spain.\cr
Maintainer: Sara Aibar <saibar@usal.es>
}
%\references{
%~~ Literature or other references for background information ~~
%}
\keyword{ package }
\keyword{ classif }
\seealso{
% TO DO: % The detailed user guide: \code{vignette("geNetClassifier-Vignette")} \cr 
% A comprehensive usage sample: \code{vignette("geNetClassifier-UsageSample")} \cr
Main functions included in this package:
\itemize{
\item{\code{\link[geNetClassifier:geNetClassifier]{geNetClassifier}} }
\item{\code{\link[geNetClassifier:queryGeNetClassifier]{queryGeNetClassifier}} }
}
Query stats funtions:
\itemize{
\item{\code{\link[geNetClassifier:querySummary]{querySummary}} }
\item{\code{\link[geNetClassifier:externalValidation.probMatrix]{externalValidation.probMatrix}} }
\item{\code{\link[geNetClassifier:externalValidation.stats]{externalValidation.stats}} }
\item{\code{\link[geNetClassifier:plotAssignments]{plotAssignments}} }
}
Plots and genes info:
\itemize{
\item{\code{\link[geNetClassifier:calculateGenesRanking]{calculateGenesRanking}} }
\item{\code{\link[geNetClassifier:plotNetwork]{plotNetwork}} }
\item{\code{\link[geNetClassifier:plotDiscriminantPower]{plotDiscriminantPower}} }
\item{\code{\link[geNetClassifier:plotExpressionProfiles]{plotExpressionProfiles}} }
}
Classes:
\itemize{
\item{\code{\link[geNetClassifier:GenesRanking-class]{GenesRanking}} }
\item{\code{\link[geNetClassifier:GenesNetwork-class]{GenesNetwork}} }
\item{\code{\link[geNetClassifier:GeNetClassifierReturn-class]{GeNetClassifierReturn}} }
\item{\code{\link[geNetClassifier:GeneralizationError-class]{GeneralizationError}} }
}
Related data sets:
\itemize{
\item{\code{\link[leukemiasEset:leukemiasEset]{leukemiasEset}} }
}
}
%\examples{
%#TO DO.-
%}
