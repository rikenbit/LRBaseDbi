\name{LRBaseDb-class}
\docType{class}

\alias{LRBaseDb-class}
\alias{class:LRBaseDb}
\alias{LRBaseDb}

\title{LRBaseDb objects}
\description{
    \code{LRBaseDb} is the simple class for providing the relationship between
    Entrez gene IDs and LRBase IDs. It provides the database connection and
    easily accessible with \code{columns}, \code{keytypes}, \code{keys} and
    \code{select}. Some users may use additional functions such as
    \code{dbconn}, \code{dbfile}, \code{dbschema}, \code{dbInfo},
    and \code{species} for much
    complex data acquisition.

    \code{columns} shows which kinds of data can be returned for the
    \code{LRBaseDb} object.

    \code{keytypes} allows the user to discover which keytypes can be
    passed in to \code{select} or \code{keys} and the \code{keytype}
    argument.

    \code{keys} returns keys for the database contained in the
    \code{LRBaseDb} object .  This method is already documented in the keys
    manual page but is mentioned again here because it's usage with
    \code{select} is so intimate.  By default it will return the primary
    keys for the database, but if used with the \code{keytype} argument,
    it will return the keys from that keytype.

    \code{select} will retrieve the data as a data.frame based on
    parameters for selected \code{keys}, \code{columns}, and \code{keytype}
    arguments.

    \code{dbconn} returns the connection with database in the package.

    \code{dbfile} returns the absolute path sqlite file is saved.

    \code{dbschema} returns the database schema.

    \code{dbInfo} returns the many meta information about the package.

    \code{species} returns the species name.

}
\usage{
LRBaseDb
}
\arguments{
    \item{LRBaseDb}{the \code{LRBaseDb} object. But in practice this will mean
    an object derived from an \code{LRBaseDb} object such as a
    \code{LRBase.Hsa.eg.db}, \code{LRBase.Mmu.eg.db} or many other
    LRBase.XXX.eg.db (XXX means abbreviation of species name).}
}

\value{
the \code{LRBaseDb} object
}

\author{Koki Tsuyuzaki}

\seealso{
    \code{\link[DBI:dbConnect]{dbConnect}}
}

\examples{
ls("package:LRBaseDbi")
}