\name{lrListDatabases}
\docType{methods}

\alias{generic,lrListDatabases}
\alias{lrListDatabases,LRBaseDb-method}
\alias{lrListDatabases}

\title{A function to return the list of databases used in the package}
\description{This function returns the list of databases used in the package.}

\usage{lrListDatabases(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The list of source databases
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("lrListDatabases")
}