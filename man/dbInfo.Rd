\name{dbInfo}
\docType{methods}

\alias{generic,dbInfo}
\alias{dbInfo,LRBaseDb-method}
\alias{dbInfo}

\title{A function to return the database information}
\description{This function returns the database information,
which is METADATA table in sqlite file.}

\usage{dbInfo(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The information of LRBase.XXX.eg.db-type package
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("dbInfo")
}