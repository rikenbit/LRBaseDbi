\name{dbschema}
\docType{methods}

\alias{generic,dbschema}
\alias{dbschema,LRBaseDb-method}
\alias{dbschema}

\title{A function to return the schema of sqlite-database}
\description{This function returns the schema of sqlite-database.}

\usage{dbschema(x, file = "", show.indices = FALSE)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
\item{file}{\code{LRBaseDb} file}
\item{show.indices}{\code{LRBaseDb} show.indices}
}
\value{
The schema for constructing sqlite database file
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("dbschema")
}