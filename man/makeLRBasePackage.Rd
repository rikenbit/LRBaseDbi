\name{makeLRBasePackage}

\alias{makeLRBasePackage}

\title{
  Making LRBaseDb packages from corresponding table as single data frame.
}
\description{
 \code{makeLRBasePackage} is a method that generates a package
  that will load an appropriate \code{LRBaseDb} object that
  will in turn point to existing annotation packages.
}
\usage{
makeLRBasePackage(pkgname,
					data,
          metadata,
          organism,
					version,
					maintainer,
					author,
					destDir,
					license="Artistic-2.0")
}
\arguments{
  \item{pkgname}{The package name.}
  \item{data}{Data frame contains Ligand GENEID (e.g., 100036770), Receptor GENEID (e.g., 100036770), SOURCEID (PMID), and SOURCEDB (e.g., FANTOM5)}
  \item{metadata}{Data frame contains metadata of the package}
  \item{organism}{The name of the organism this package represents}
  \item{version}{The version number of this package}
  \item{maintainer}{The maintainer who creates the package (must include email to be valid)}
  \item{author}{The creator of the package}
  \item{destDir}{A path where the package source should be assembled.}
  \item{license}{The license and it's version}
}

\details{
  The purpose of this method is to create a special package that will
  depend on existing annotation packages and which will load a special
  \code{LRBaseDb} object that will allow proper dispatch of
  special select methods.  These methods will allow the user to easily
  query across multiple annotation resources via information contained
  by the \code{LRBaseDb} object. Because the end result will
  be a package that treats all the data mapped together as a single
  source, the user is encouraged to take extra care to ensure that the
  different packages used are from the same build etc.
}

\value{A special package to load an \link{LRBaseDb} object.}

\author{Koki Tsuyuzaki}

\seealso{
  \code{\link{LRBaseDb}}
}

\examples{
## makeLRBasePackage enable users to construct
## user's own custom LRBase package

## this is test data which means the relationship between
## Entrez gene IDs of Pseudomonas aeruginosa FANTOM5
## and its LRBase IDs.
data(FANTOM5)
head(FANTOM5)

# We are also needed to prepare meta data as follows.
data(metaFANTOM5)
metaFANTOM5

## sets up a temporary directory for this example
## (users won't need to do this step)
destination <- tempfile()
dir.create(destination)

## makes an Organism package for human called Homo.sapiens
makeLRBasePackage(pkgname = "FANTOM5.Hsa.eg.db",
					data = FANTOM5,
          metadata = metaFANTOM5,
					organism = "Homo sapiens",
					version = "0.99.0",
					maintainer = "Koki Tsuyuzaki <k.t.the-answer@hotmail.co.jp>",
					author = "Koki Tsuyuzaki",
					destDir = destination,
					license="Artistic-2.0")
}
