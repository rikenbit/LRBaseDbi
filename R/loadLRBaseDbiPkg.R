##
## This is for LRBase.XXX.eg.db
##

.loadLRBaseDbiPkg <- function (pkgname) {

  ## Inherit class, Instantiation
  obj <- LRBaseDb(pkgname)

  ## Export object
  ns <- asNamespace(pkgname)
  assign(pkgname, obj, envir=ns)
  namespaceExport(ns, pkgname)
}
