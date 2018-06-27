\name{packageName}
\docType{methods}

\alias{generic,packageName}
\alias{packageName,LRBaseDb-method}
\alias{packageName}

\title{A function to return the package name}
\description{This function returns the package name}

\usage{packageName(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The name of LRBase.XXX.eg.db-type package
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("packageName")
}