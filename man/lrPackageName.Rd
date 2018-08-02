\name{lrPackageName}
\docType{methods}

\alias{generic,lrPackageName}
\alias{lrPackageName,LRBaseDb-method}
\alias{lrPackageName}

\title{A function to return the package name}
\description{This function returns the package name}

\usage{lrPackageName(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The name of LRBase.XXX.eg.db-type package
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("lrPackageName")
}