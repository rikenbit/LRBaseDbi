##
## Definition of Classes
##

# Reference class
.LRBaseDb <- setRefClass("LRBaseDb", contains="AnnotationDb")

## Constructor
LRBaseDb <- function(pkgname){
    ## Inherit class, Instantiation
    .dbconn <- dbConnect(
        SQLite(),
        paste0(
            system.file(c("inst", "extdata"), package=pkgname),
            paste0("/", pkgname, ".sqlite")
        )
    )
    obj <- .LRBaseDb$new(conn=.dbconn, packageName=pkgname)
    return(obj)
}
