\name{MPOMAPCOUNTS} 

\alias{MPOMAPCOUNTS}


\title{Number of mapped keys for the maps in package MPO.db}

\description{
    MPOMAPCOUNTS provides the "map count" (i.e. the
    count of mapped keys) for each map in package MPO.db.
}

\details{    
    This "map count" information is precalculated and stored in the
    package annotation DB. This allows some quality control and is used by
    the \code{\link[AnnotationDbi:AnnDbPkg-checker]{checkMAPCOUNTS}}
    function defined in AnnotationDbi to compare and validate different
    methods (like \code{count.mappedkeys(x)} or
    \code{sum(!is.na(as.list(x)))}) for getting the "map count" of a given
    map.
}

\examples{
    # Convert the object to a list
    xx <- as.list(MPOCHILDREN)
}

\value{
    \code{MPO_dbconn}: a DBIConnection object representing an
    open connection to the package annotation DB.

    \code{MPO_dbfile}: a character string with the path to the
    package annotation DB.

    \code{MPO_dbschema}: none (invisible \code{NULL}).

    \code{MPO_dbInfo}: none (invisible \code{NULL}).
}

\seealso{
    \code{\link[AnnotationDbi:Bimap-keys]{mappedkeys}},
    \code{\link[AnnotationDbi:Bimap-keys]{count.mappedkeys}},
    \code{\link[AnnotationDbi:AnnDbPkg-checker]{checkMAPCOUNTS}}
}

\keyword{datasets}

