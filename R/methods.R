##
## Definition of Methods
##

## helper for vector dividing
.div <- function(x,d=1) {
  y <- list()
  delta <- ceiling(length(x) / d)
  for(i in 1:d){
    y[[i]] <- as.vector(na.omit(x[((i-1)*delta+1):(i*delta)]))
  }
  return(y)
}

setMethod("show",
  "LRBaseDb",
    function(object) {
        print("##### class ####")
        print(class(object))
        print("##### connection #####")
        print(object$conn)
        print("##### package name #####")
        print(object$packageName)
    }
)

# columns
setMethod("columns",
  "LRBaseDb",
  function(x) {
    return(dbGetQuery(dbconn(x),"PRAGMA TABLE_INFO(DATA);")$name)
  }
)

# keytypes
setMethod("keytypes",
  "LRBaseDb",
  function(x) {
    return(dbGetQuery(dbconn(x),"PRAGMA TABLE_INFO(DATA);")$name)
  }
)

# keys
setMethod("keys",
  "LRBaseDb",
  function(x, keytype){
    query <- paste0("SELECT ", keytype, " FROM DATA;")
    k     <- unlist(unique(dbGetQuery(x$conn, query)))
    names(k) <- NULL
    return(k)
  }
)

# select
setMethod("select",
  "LRBaseDb",
  function(x, keys, columns, keytype) {
    if (length(columns) > 1) {
      c <- columns[1]
for (i in 2:(length(columns))){
    c <- paste(c, columns[i], sep = ",")
}
    } else {
      c <- columns
    }

    keys <- paste0('"', keys, '"')
    ke <- paste(keytype, keys, sep ="=")
    kee <- c()
    if (length(ke) > 1)  {
      if(length(ke) >= 1000) {
        ke_loc <- .div(1:length(ke), ceiling(length(keys)/500))
        for(j in 1:ceiling(length(keys)/500)){
          kee[j] <- paste(ke[ke_loc[[j]]], sep="",collapse=" OR ")
        }
      } else {
        kee <- paste(ke, sep="", collapse=" OR ")
      }
    } else {
      kee <- ke
    }

    # SQLs
    kk <- c()
    for(i in 1:length(kee)) {
      query <- paste0("SELECT ", c, " FROM DATA WHERE ", kee[i])
      k <- dbGetQuery(x$conn, query)
      kk <- rbind(kk, k)
    }
    return(unique(kk))
  }
)


## dbconn
setMethod("dbconn",
  "LRBaseDb",
  function(x){
    return(x$conn)
  }
)

## dbfile
setMethod("dbfile",
  "LRBaseDb",
  function(x){
    return(
      paste0(
        system.file(c("inst", "extdata"), package=x$packageName),
        paste0("/", x$packageName, ".sqlite")
      )
    )
  }
)

## dbschema
setMethod("dbschema",
  "LRBaseDb",
  function(x){
  return(dbGetQuery(x$conn, "SELECT * FROM sqlite_master;")$sql)
  }
)

## dbInfo
setMethod("dbInfo",
  "LRBaseDb",
  function(x){
    return(dbGetQuery(x$conn, "SELECT * FROM METADATA;"))
  }
)

## species
setMethod("species",
  "LRBaseDb",
  function(object) {
    return(dbGetQuery(object$conn, 'SELECT value FROM METADATA where name = "SPECIES";')[1,])
  }
)

## packageName
setMethod("packageName",
  "LRBaseDb",
  function(x){
    return(x$packageName)
  }
)

## nomenclature
setMethod("nomenclature",
  "LRBaseDb",
  function(x) {
    return(dbGetQuery(x$conn, 'SELECT value FROM METADATA where name = "ORGANISM";')[1,])
  }
)

## listDatabases
setMethod("listDatabases",
  "LRBaseDb",
  function(x) {
    return(dbGetQuery(x$conn, 'SELECT DISTINCT SOURCEDB FROM DATA;'))
  }
)

## lrVersion
setMethod("lrVersion",
  "LRBaseDb",
  function(x){
    return(dbGetQuery(x$conn, 'SELECT * FROM METADATA where name = "lrVERSION";')[1,])
  }
)
