\name{getCases}
\alias{getCases}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ 
Get cases for selected Studies
}
\description{
The Cases are the descrption of the samples from patients. The samples can be subdivided by the type of assays as, sequencing, CNA, Mutation, Methylation.
}

\seealso{
cgdsr package

} 
\examples{
\dontrun{
# Create CGDS object
cgds<-CGDS("http://www.cbioportal.org/public-portal/")
# Get list of cancer studies at server
Studies <- getCancerStudies(cgds)[,2]
# Get available case lists (collection of samples) for a given cancer study
mycancerstudy <- getCancerStudies(cgds)[2,1]
mycaselist <- getCaseLists(cgds,mycancerstudy)[1,1]
##getCases()
}
}
