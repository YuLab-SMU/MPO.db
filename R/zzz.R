
datacache <- new.env(hash=TRUE, parent=emptyenv())

MPO <- function() showQCData("MPO", datacache)
MPO_dbconn <- function() dbconn(datacache)
MPO_dbfile <- function() dbfile(datacache)
MPO_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache,
    file=file, show.indices=show.indices)
MPO_dbInfo <- function() dbInfo(datacache)



make_MPO.db <- function() {
    ah <- suppressMessages(AnnotationHub())
    # AHxx need to change after uploading data
    dbfile <- ah[["AHxx", verbose=FALSE]]  
    conn <- AnnotationDbi::dbFileConnect( dbfile )
    db <- new("MPO.db", conn=conn)
    db
}



.onLoad <- function(libname, pkgname)
{
    dbfile <- system.file("extdata", "MPO.sqlite", package=pkgname,
        lib.loc=libname)
    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)

    MPODb <- setRefClass("MPODb", contains="GODb")
    ## Create the OrgDb object
    sPkgname <- sub(".db$","",pkgname)
    txdb <- loadDb(system.file("extdata", paste(sPkgname,
        ".sqlite",sep=""), package=pkgname, lib.loc=libname),
        packageName=pkgname)
    dbObjectName <- getFromNamespace("dbObjectName", "AnnotationDbi")
    dbNewname <- dbObjectName(pkgname,"MPODb")
    ns <- asNamespace(pkgname)
    makeCachedActiveBinding("MPO.db", make_MPO.db, env=ns)
    namespaceExport(ns, "MPO.db")
    assign(dbNewname, txdb, envir=ns)
    namespaceExport(ns, dbNewname)

    ## Create the AnnObj instances
    ann_objs <- createAnnObjs.MPO_DB("MPO", "MPO", dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, pkgname)
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(MPO_dbconn())
}

