\name{keytypes}
\docType{methods}

\alias{generic,keytypes}
\alias{keytypes,LRBaseDb-method}
\alias{keytypes}

\title{A function to return the columns that user can specified
in select function}
\description{This function returns the columns that user can specified in
select function.}

\usage{keytypes(x)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
}
\value{
The columns that user can specify as key
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("keytypes")
}