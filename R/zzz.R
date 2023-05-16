make_MPO.db <- function() {datacache
    ah <- suppressMessages(AnnotationHub())
    dbfile <- ah[["AH111553", verbose=FALSE]]  
    conn <- AnnotationDbi::dbFileConnect(dbfile)
    db <- new("MPODb", conn=conn)
    db
}


datacache <- new.env(hash=TRUE, parent=emptyenv())
MPO <- function() showQCData("MPO", datacache)
MPO_dbconn <- function() dbconn(datacache)
MPO_dbfile <- function() dbfile(datacache)
MPO_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache,
    file=file, show.indices=show.indices)
MPO_dbInfo <- function() dbInfo(datacache)
.onLoad <- function(libname, pkgname) {
    ns <- asNamespace(pkgname)
    makeCachedActiveBinding("MPO.db", make_MPO.db, env=ns)
    namespaceExport(ns, "MPO.db")
    ah <- suppressMessages(AnnotationHub())
    dbfile <- ah[["AH111553", verbose=FALSE]]  
    dbconn <- AnnotationDbi::dbFileConnect(dbfile)
    assign("dbconn", dbconn, envir=datacache)
    ann_objs <- createAnnObjs.MPO_DB("MPO", "MPO", dbconn, datacache)
    mergeToNamespaceAndExport(ann_objs, "MPO.db")

}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("MPO.db version ", packageVersion(pkgname))
}

.onUnload <- function(libpath) {
    MPO.db$finalize()
}


# datacache <- new.env(hash=TRUE, parent=emptyenv())
# # datacache <- asNamespace("MPO.db")

# MPO <- function() showQCData("MPO", datacache)
# MPO_dbconn <- function() dbconn(datacache)
# MPO_dbfile <- function() dbfile(datacache)
# MPO_dbschema <- function(file="", show.indices=FALSE) dbschema(datacache,
#     file=file, show.indices=show.indices)
# MPO_dbInfo <- function() dbInfo(datacache)

# .onLoad <- function(libname, pkgname)
# {

    
#     ah <- AnnotationHub()
#     txdb <- ah[["AH111553", verbose=FALSE]] 
#     dbfile <- txdb
#     MPODb <- setRefClass("MPODb", contains="AnnotationDb")
#     txdb <- loadDb(dbfile, packageName=pkgname)

#     ## To avoid error reason: replacement has 70029 rows, data has 0
#     # save(txdb, file = "txdb.Rdata")
#     # on.exit(file.remove("txdb.Rdata"))
#     ##############

#     assign("dbfile", dbfile, envir=datacache)
#     dbconn <- dbFileConnect(dbfile)
#     assign("dbconn", dbconn, envir=datacache)

#     ## Create the OrgDb object
#     sPkgname <- sub(".db$","",pkgname) 
#     dbNewname <- pkgname
#     ns <- asNamespace(pkgname)
#     assign(dbNewname, txdb, envir=ns)
#     namespaceExport(ns, dbNewname)

#     ## Create the AnnObj instances
#     ann_objs <- createAnnObjs.MPO_DB(sPkgname, sPkgname, dbconn, datacache)
#     mergeToNamespaceAndExport(ann_objs, pkgname)
# }

# .onUnload <- function(libpath)
# {
#     dbFileDisconnect(MPO_dbconn())
# }

