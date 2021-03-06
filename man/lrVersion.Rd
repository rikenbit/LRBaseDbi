\name{lrVersion}
\docType{methods}

\alias{generic,lrVersion}
\alias{lrVersion,LRBaseDb-method}
\alias{lrVersion}

\title{A function to return the LRBase version of package}
\description{This function returns the version of LRBase used in the package.}

\usage{lrVersion(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The version of LR database
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("lrVersion")
}