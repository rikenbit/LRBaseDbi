\name{select}
\docType{methods}

\alias{generic,select}
\alias{select,LRBaseDb-method}
\alias{select}

\title{select function defined by AnnotationDbi-class}
\description{This function returns the corresponding rows that user specified}

\usage{select(x, keys, columns, keytype, ...)}

\arguments{
\item{x}{\code{LRBaseDb} object such as \code{LRBase.Mmu.eg.db}}
\item{keys}{\code{LRBaseDb} keys vector}
\item{columns}{\code{LRBaseDb} columns the user want to retrieve}
\item{keytype}{\code{LRBaseDb} What kind of key the user want to specify}
\item{...}{\code{LRBaseDb} Other parameter}
}
\value{
select function defined by AnnotationDbi package
}
\author{Koki Tsuyuzaki}
\examples{
showMethods("select")
}