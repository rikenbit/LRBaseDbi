# check select method for LRBaseDb class"

.LRBaseDb <- setRefClass("LRBaseDb", contains="AnnotationDb")

.dbconn <- RSQLite::dbConnect(
            RSQLite::SQLite(),
            paste0(
              system.file(c("inst", "DBschemas"), package="LRBaseDbi"),
              "/LRBase.XXX.eg.db.sqlite"
            )
          )

obj <- .LRBaseDb$new(conn=.dbconn, packageName="test")

checkEquals(length(is(obj)), 8)
