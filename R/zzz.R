.onAttach <- function(libname, pkgname){
    msg <- paste(
    "LRBase.XXX.eg.db-type packages are deprecated",
    "since Bioconductor 3.14.",
    "Use AnnotationHub instead. For details,",
    "check the vignette of LRBaseDbi"
    )
    packageStartupMessage(msg)
}
