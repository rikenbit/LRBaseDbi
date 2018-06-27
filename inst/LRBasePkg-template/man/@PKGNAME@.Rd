\name{@PKGNAME@}
\alias{@PKGNAME@}
\title{Annotation package that provides correspondence between
Ligand-Receptor genes as Entrez Gene ID}
\description{This data represents a collection of annotation packages
that can be used as a single object named as package name. This object
can be used with the standard four accessor method for all AnnotationDbi
objects.  Namely: \code{columns}, \code{keytypes}, \code{keys} and
\code{select}.  Users are encouraged to read the vignette from the
\code{LRBaseDbi} package for more details.}
\author{@AUTHOR@}
\usage{@PKGNAME@}
\value{LRBase.XXX.eg.db-type package}
\examples{
@PKGNAME@
cls <- columns(@PKGNAME@)
cls
kts <- keytypes(@PKGNAME@)
kt <- kts[2]
kts
ks <- head(keys(@PKGNAME@, keytype=kts[2]))
ks
res <- select(@PKGNAME@, keys=ks, columns=cls, keytype=kt)
head(res)

dbconn(@PKGNAME@)
dbfile(@PKGNAME@)
dbschema(@PKGNAME@)
dbInfo(@PKGNAME@)
species(@PKGNAME@)
nomenclature(@PKGNAME@)
}
