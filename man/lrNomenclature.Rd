\name{lrNomenclature}
\docType{methods}

\alias{generic,lrNomenclature}
\alias{lrNomenclature,LRBaseDb-method}
\alias{lrNomenclature}

\title{A function to return the scientific name}
\description{This function returns the scientific name of
species used in the package.}

\usage{lrNomenclature(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The scientific name of organisms
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("lrNomenclature")
}