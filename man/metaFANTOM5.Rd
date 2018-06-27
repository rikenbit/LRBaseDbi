\name{metaFANTOM5}
\alias{metaFANTOM5}
\docType{data}
\title{Metadata to construct user's original LRBaseDb package}
\description{Meta data to construct user's custom LRBaseDb}

\usage{data(metaFANTOM5)}

\details{
    \itemize{
        \item {SOURCEDATE}: The date the source data is retrived
        \item {SOURCENAME}: Type of source data
        \item {SOURCEURL}: The URL of source data
        \item {DBSCHEMA}: Database schema
        \item {DBSCHEMAVERSION}: The version of database schema
        \item {ORGANISM}: The scientific name
        \item {SPECIES}: The common name of the species
        \item {package}: The pacakge name
        \item {Db type}: The type of name (or class name)
        \item {LRVERSION}: The version for numbering of ligand-receptor data
    }
}
\value{
The test data is loaded
}
\examples{
data(metaFANTOM5)
head(metaFANTOM5)
}

\keyword{datasets}