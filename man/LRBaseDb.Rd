\name{LRBaseDb-class}
\docType{class}

\alias{LRBaseDb-class}
\alias{class:LRBaseDb}
\alias{LRBaseDb}

% select and select related methods
\alias{columns}
\alias{keytypes}
\alias{keys}
\alias{select}
\alias{dbconn}
\alias{dbfile}
\alias{dbschema}
\alias{dbInfo}
\alias{species}
\alias{lrNomenclature}
\alias{lrListDatabases}
\alias{lrVersion}

\alias{columns,LRBaseDb-method}
\alias{keytypes,LRBaseDb-method}
\alias{keys,LRBaseDb-method}
\alias{select,LRBaseDb-method}
\alias{dbconn,LRBaseDb-method}
\alias{dbfile,LRBaseDb-method}
\alias{dbschema,LRBaseDb-method}
\alias{dbInfo,LRBaseDb-method}
\alias{species,LRBaseDb-method}
\alias{lrNomenclature,LRBaseDb-method}
\alias{lrListDatabases,LRBaseDb-method}
\alias{lrVersion,LRBaseDb-method}

\title{LRBaseDb objects}
\description{
    \code{LRBaseDb} is the simple class for providing the relationship between
    Entrez gene IDs and LRBase IDs. It provides the database connection and
    easily accessible with \code{columns}, \code{keytypes}, \code{keys} and
    \code{select}. Some users may use additional functions such as
    \code{dbconn}, \code{dbfile}, \code{dbschema}, \code{dbInfo},
    \code{species}, \code{lrNomenclature}, \code{lrListDatabases},
    and \code{lrVersion} for much complex data acquisition.

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

    \code{lrNomenclature} returns the scientific name.

    \code{lrListDatabases} returns the list of databases to correspond Gene ID of ligend gene and Gene ID of receptor gene.

    \code{lrVersion} returns the version of LRBaseDb.
}
\usage{
  columns(x)
  keytypes(x)
  keys(x, keytype, ...)
  select(x, keys, columns, keytype, ...)
  dbconn(x)
  dbfile(x)
  dbschema(x, file = "", show.indices = FALSE)
  dbInfo(x)
  species(object)
  lrNomenclature(x)
  lrListDatabases(x)
  lrVersion(x)
}
\arguments{
  \item{x}{the \code{LRBaseDb} object converted by \code{LRBaseDbi::LRBaseDb}.}

  \item{object}{same as x}

  \item{keys}{the keys to select records for from the database.  All possible keys are returned by using the \code{keys} method.}

  \item{columns}{the columns or kinds of things that can be retrieved from the database. As with \code{keys}, all possible columns are returned by using the \code{columns} method.}

  \item{keytype}{the keytype that matches the keys used. For the
    \code{select} methods, this is used to indicate the kind of ID being used with the keys argument. For the \code{keys} method this is used to indicate which kind of keys are desired from \code{keys}}

  \item{...}{other arguments.}

  \item{file}{The \code{file} argument must be a connection, or a character string naming the file to print to (see the \code{file} argument of the \code{\link[base:cat]{cat}} function for the details).}

  \item{show.indices}{The CREATE INDEX statements are not shown by default. Use \code{show.indices=TRUE} to get them.}
}

\value{
  \code{keys}, \code{columns}, \code{keytypes}, \code{dbfile}, \code{dbInfo}, \code{species}, and \code{lrNomenclature} each return a character vector or possible values. \code{select}, \code{dbschema}, \code{lrListDatabases}, and \code{lrVersion} each return a data.frame. \code{dbconn} returns database connection.
}

\author{Koki Tsuyuzaki}

\seealso{
    \code{\link[DBI:dbConnect]{dbConnect}}
}

\examples{
# library("LRBaseDbi")
# library("AnnotationHub")

# # Data retrieval from AnnotationHub
# ah <- AnnotationHub()
# dbfile <- query(ah, c("LRBaseDb", "Sus scrofa", "v001"))[[1]]

# # Constructor
# LRBase.Ssc.eg.db <- LRBaseDbi::LRBaseDb(dbfile)

# # show
# LRBase.Ssc.eg.db

# # dbconn
# dbconn(LRBase.Ssc.eg.db)

# # dbfile
# dbfile(LRBase.Ssc.eg.db)

# # dbschema
# dbschema(LRBase.Ssc.eg.db)

# # dbInfo
# dbInfo(LRBase.Ssc.eg.db)

# # species
# species(LRBase.Ssc.eg.db)

# # lrNomenclature
# lrNomenclature(LRBase.Ssc.eg.db)

# # lrListDatabases
# lrListDatabases(LRBase.Ssc.eg.db)

# # lrVersion
# lrVersion(LRBase.Ssc.eg.db)

# # columns
# cols <- columns(LRBase.Ssc.eg.db)

# # keytypes
# kts <- keytypes(LRBase.Ssc.eg.db)

# # keys
# ks <- keys(LRBase.Ssc.eg.db, keytype="GENEID_L")[seq(10)]

# # select
# out <- select(LRBase.Ssc.eg.db,
#     columns=cols,
#     keys=ks,
#     keytype="GENEID_L")
}