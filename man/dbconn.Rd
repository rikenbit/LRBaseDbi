\name{dbconn}
\docType{methods}

\alias{generic,dbconn}
\alias{dbconn,LRBaseDb-method}
\alias{dbconn}

\title{A function to return the connection to sqlite3 file}
\description{This function returns the connection to sqlite file.}

\usage{dbconn(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The database connection to sqlite3 file
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("dbconn")
}