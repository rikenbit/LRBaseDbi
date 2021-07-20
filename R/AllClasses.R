##
## Definition of Classes
##

# Reference class
.LRBaseDb <- setRefClass("LRBaseDb",
    contains="AnnotationDb",
    fields=list(
      conn="SQLiteConnection",
      dbfile="character"))

## Constructor
LRBaseDb <- function(dbfile){
    ## Inherit class, Instantiation
    .dbconn <- dbConnect(
        SQLite(),
        dbfile
    )
    obj <- .LRBaseDb$new(conn=.dbconn, dbfile=dbfile)
    return(obj)
}
