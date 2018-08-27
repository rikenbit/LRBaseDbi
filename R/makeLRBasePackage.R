##
## This is for constracting original LRBase.XXX.eg.db packages by end-users
##

makeLRBasePackage <- function(pkgname, data, metadata, organism, version,
    maintainer, author, destDir, license="Artistic-2.0"){

    # Validate of data
    .validateColNames1(data)
    .validateColNames2(metadata)

    ## there should only be one template
    template_path <- system.file("LRBasePkg-template", package="LRBaseDbi")

    ## We need to define some symbols in order to have the
    ## template filled out correctly.
    symvals <- list(
        PKGTITLE=paste("An annotation package for the LRBaseDb object"),
        PKGDESCRIPTION=paste("Contains the LRBaseDb object",
            "to access data from several related annotation packages."),
        PKGVERSION=version,
        AUTHOR=author,
        MAINTAINER=maintainer,
        LIC=license,
        ORGANISM=organism,
        ORGANISMBIOCVIEW=gsub(" ","_",organism),
        PKGNAME=pkgname
    )

    .isSingleString <- function (x){
        is.character(x) && length(x) == 1L && !is.na(x)
    }

    ## Should never have duplicates
    if (any(duplicated(names(symvals))))
        stop("'symvals' contains duplicated symbols")
    ## All symvals should by single strings (non-NA)
    is_OK <- vapply(symvals, .isSingleString, TRUE)
    if (!all(is_OK)) {
        bad_syms <- paste(names(is_OK)[!is_OK], collapse="', '")
        stop("values for symbols '", bad_syms, "' are not single strings")
    }

    ## create Package structure
    createPackage(pkgname = pkgname,
        destinationDir = destDir,
        originDir = template_path,
        symbolValues = symvals,
        unlink = TRUE
    )

    # copy vignette
    .pathRmd <- function(){
        LIBPATHS = .libPaths()
        LRPATH = sapply(LIBPATHS, function(x){
            file.exists(paste0(x, "/LRBaseDbi/doc/LRBaseDbi.Rnw"))
        })
        LRPATH = names(LRPATH[which(LRPATH)])
        if(length(LRPATH) != 0){
            paste0(LRPATH[1], "/LRBaseDbi/doc/LRBaseDbi.Rnw")
        }else{
            stop("The library path is not found!\n")
        }
    }

    dir.create(paste0(destDir, "/", pkgname, "/vignettes/"),
        showWarnings = FALSE, recursive = TRUE)
    template_rnw <- .pathRmd()
    new_rnw <- unlist(read.delim(template_rnw, header=FALSE))
    new_rnw <- gsub("LRBaseDbi", pkgname, new_rnw)
    sink(paste0(destDir, "/", pkgname, "/vignettes/", pkgname, ".Rnw"))
    for(i in seq_along(new_rnw)){
        cat(paste0(new_rnw[i], "\n"))
    }
    sink()

    ## move template to dest
    template_sqlite <- paste0(system.file("DBschemas", package = "LRBaseDbi"),
        "/LRBase.XXX.eg.db.sqlite")
    dir.create(paste0(destDir, "/", pkgname, "/inst/extdata"),
        showWarnings = FALSE, recursive = TRUE)
    dest_sqlitepath <- paste0(destDir, "/", pkgname, "/inst/extdata/")
    file.copy(from = template_sqlite, to = dest_sqlitepath,
        overwrite=TRUE)

    ## rename
    old_dest_sqlite <- paste0(dest_sqlitepath, "LRBase.XXX.eg.db.sqlite")
    new_dest_sqlite <- paste0(dest_sqlitepath, pkgname, ".sqlite")
    file.rename(from = old_dest_sqlite, to = new_dest_sqlite)

    # ## connection
    conn <- dbConnect(SQLite(), dbname = new_dest_sqlite)

    ## insert metadata into moved sqlite database
    dbWriteTable(conn, name="METADATA", value=metadata, overwrite=TRUE)

    ## insert data and metadata into moved sqlite database
    dbWriteTable(conn, name="DATA", value=data, overwrite=TRUE)

    # disconnection
    dbDisconnect(conn)
}

.validateColNames1 <- function(data){
    if(ncol(data) != 4){
        stop("Data should has 4 columns!")
    }
    if(colnames(data)[1] != "GENEID_L"){
        stop("Please specify the name of 1st column as 'GENEID_L'")
    }
    if(colnames(data)[2] != "GENEID_R"){
        stop("Please specify the name of 1st column as 'GENEID_R'")
    }
    if(colnames(data)[3] != "SOURCEID"){
        stop("Please specify the name of 1st column as 'SOURCEID'")
    }
    if(colnames(data)[4] != "SOURCEDB"){
        stop("Please specify the name of 1st column as 'SOURCEDB'")
    }
}

.validateColNames2 <- function(metadata){
    if(ncol(metadata) != 2){
        stop("Meta data should has 2 columns!")
    }
    if(colnames(metadata)[1] != "NAME"){
        stop("Please specify the name of 1st column as 'NAME'")
    }
    if(colnames(metadata)[2] != "VALUE"){
        stop("Please specify the name of 2nd column as 'VALUE'")
    }
}
