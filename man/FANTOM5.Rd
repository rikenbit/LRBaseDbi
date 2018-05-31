\name{FANTOM5}
\alias{FANTOM5}
\docType{data}
\title{
Data to construct user's original LRBaseDb package
}
\description{
Correspondance between Ligand Entrez Gene ID and Receptor Entrez Gene ID of FANTOM5 project used for demo data. This is to demonstrate how to construct user's original \code{LRBaseDb} package.
}

\usage{data(FANTOM5)}

\details{
\itemize{
 \item 1st Column: Ligand Entrez gene ID
 \item 2nd Column: Receptor Entrez gene ID
 \item 3rd Column: Source ID. In this case, PMID is used.
 \item 4th Column: Source Database. In this case, FANTOM5
}
}
\examples{
data(FANTOM5)
head(FANTOM5)
}

\keyword{datasets}