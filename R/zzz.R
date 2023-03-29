
datacache <- new.env(hash=TRUE, parent=emptyenv())

MPO <- function() showQCData("MPO", datacache)
MPO_dbconn <- function() dbconn(datacache)
MPO_dbfile <- function() dbfile(datacache)
MPO_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache,
    file=file, show.indices=show.indices)
MPO_dbInfo <- function() dbInfo(datacache)



make_MPO.db <- function() {
    ah <- suppressMessages(AnnotationHub())
    MPODb <- setRefClass("MPODb", contains="GODb")
    dbfile <- ah[["AH111553", verbose=FALSE]]  
    conn <- AnnotationDbi::dbFileConnect(dbfile$conn@dbname)
    db <- new("MPO.db", conn=conn)
    db
}



.onLoad <- function(libname, pkgname)
{
    MPODb <- setRefClass("MPODb", contains="AnnotationDb")
    ns <- asNamespace(pkgname)
    makeCachedActiveBinding("MPO.db", make_MPO.db, env=ns)
    namespaceExport(ns, "MPO.db")
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(MPO_dbconn())
}

