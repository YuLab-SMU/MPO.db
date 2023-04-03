
datacache <- new.env(hash=TRUE, parent=emptyenv())

MPO <- function() showQCData("MPO", datacache)
MPO_dbconn <- function() dbconn(datacache)
MPO_dbfile <- function() dbfile(datacache)
MPO_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache,
    file=file, show.indices=show.indices)
MPO_dbInfo <- function() dbInfo(datacache)


.onLoad <- function(libname, pkgname)
{
    MPODb <- setRefClass("MPODb", contains="GODb")
    ah <- suppressMessages(AnnotationHub())
    txdb <- ah[["AH111553", verbose=FALSE]] 
    dbfile <- txdb$conn@dbname
    txdb <- loadDb(dbfile, packageName=pkgname)

    ## To avoid error reason: replacement has 70029 rows, data has 0
    save(txdb, file = "txdb.Rdata")
    on.exit(file.remove("txdb.Rdata"))
    ##############

    assign("dbfile", dbfile, envir=datacache)
    dbconn <- dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)
    MPODb <- setRefClass("MPODb", contains="GODb")
    ## Create the OrgDb object
    sPkgname <- sub(".db$","",pkgname)

  
    dbObjectName <- getFromNamespace("dbObjectName", "AnnotationDbi")
    dbNewname <- dbObjectName(pkgname,"MPODb")
    ns <- asNamespace(pkgname)
    assign(dbNewname, txdb, envir=ns)
    namespaceExport(ns, dbNewname)

    ## Create the AnnObj instances
    ann_objs <- createAnnObjs.MPO_DB(sPkgname, sPkgname, dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, pkgname)
}

.onUnload <- function(libpath)
{
    dbFileDisconnect(MPO_dbconn())
}

