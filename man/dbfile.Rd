\name{dbfile}
\docType{methods}

\alias{generic,dbfile}
\alias{dbfile,LRBaseDb-method}
\alias{dbfile}

\title{A function to return the directory that the sqlite3 file stored}
\description{This function returns the directory that the sqlite file stored.}

\usage{dbfile(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The directory path to the sqlite3 file
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("dbfile")
}