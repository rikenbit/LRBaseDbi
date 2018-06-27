\name{columns}
\docType{methods}

\alias{generic,columns}
\alias{columns,LRBaseDb-method}
\alias{columns}

\title{A function to return the columns of LRBase that user can retrieve}
\description{This function returns the columns of LRBase that
user can retrieve.}

\usage{columns(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The columns of sqlite database
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("columns")
}