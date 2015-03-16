\name{getGenProfs}
\alias{getGenProfs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get Genetic Profile from selected Studies
}
\description{
The Genetic Profiles are subdivided by the type of assays as, CNA, Mutation, Methylation, Gene expression.
}

\examples{
\dontrun{
##library(canceR)
##library(cgdsr)
## Create CGDS object
##cgds<-CGDS("http://www.cbioportal.org/public-portal/")
# Get list of cancer studies at server
Studies <- getCancerStudies(cgds)[,2]
# Get available case lists (collection of samples) for a given cancer study
##mycancerstudy <- getCancerStudies(cgds)[2,1]
##mycaselist <- getCaseLists(cgds,mycancerstudy)[1,1]
# Get available genetic profiles
##mygeneticprofile <- getGeneticProfiles(cgds,mycancerstudy)[4,1]
##getGenProfs()
}
}