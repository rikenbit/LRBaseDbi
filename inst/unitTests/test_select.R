library("RSQLite")

# check select method for LRBaseDb class"

.LRBaseDb <- setRefClass("LRBaseDb", contains="AnnotationDb")

.dbconn <- dbConnect(
    SQLite(),
    paste0(
        system.file(c("inst", "DBschemas"), package="LRBaseDbi"),
            "/LRBase.XXX.eg.db.sqlite"
    )
)

obj <- .LRBaseDb$new(conn=.dbconn, packageName="test")

checkTrue("LRBaseDb" %in% is(obj))
checkTrue("AnnotationDb" %in% is(obj))
