\name{keys}
\docType{methods}

\alias{generic,keys}
\alias{keys,LRBaseDb-method}
\alias{keys}

\title{A function defined by AnnotationDbi to return the keys}
\description{This function returns the keys.}

\usage{keys(x, keytype, ...)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
\item{keytype}{\code{LRBaseDb} What kind of key user want to specify}
\item{...}{\code{LRBaseDb} Other parameter}
}
\value{
The keys for using select functions
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("keys")
}