\name{GSEA.Analyze.Sets}
\alias{GSEA.Analyze.Sets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
GSEA.Analyze.Sets
}
\description{
http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/R-GSEA_Readme
}
\value{
GSEA.Analyze.Sets
}
\usage{
GSEA.Analyze.Sets(directory,topgs="",non.interactive.run= FALSE,height=12,width=17)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{directory}{
\code{directory= fname.Output} 
}
  \item{topgs}{
\code{topgs = 20} 
}
  \item{non.interactive.run}{
      \code{non.interactive.run= FALSE} 
}
  \item{height}{
      \code{height=16} 
}
  \item{width}{
      \code{width=16} 
}
}


\references{
 
 http://www.broadinstitute.org/cancer/software/gsea/wiki/index.php/Main_Page.
}
\author{
Subramanian, Tamayo, et al. (2005, PNAS 102, 15545-15550) and Mootha,
Lindgren, et al. (2003, Nat Genet 34, 267-273)
}



\examples{
\dontrun{
## Load workspace
readRDS(paste(path.package("canceR"),"/extdata/rdata/ucec_tcga_pubGSEA1021.rds", sep=""))
##Run.GSEA()
}
}