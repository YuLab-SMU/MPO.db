\name{MPOALIAS}
\alias{MPOALIAS}
\title{Map from MPO id to MPO alias}
\description{
    MPOALIAS is an R object that provides mapping
    from MPO id to MPO alias
}
\details{
    Mappings were based on data provided by: Mouse Phenotype Ontology
    With a date stamp from the source of: 20230302
}

\examples{
    # Convert the object to a list
    xx <- as.list(MPOALIAS)
}

\value{
    \code{MPO_dbconn}: a DBIConnection object representing an
    open connection to the package annotation DB.

    \code{MPO_dbfile}: a character string with the path to the
    package annotation DB.

    \code{MPO_dbschema}: none (invisible \code{NULL}).

    \code{MPO_dbInfo}: none (invisible \code{NULL}).
}

\references{
\url{http://hdo-wiki.nubic.northwestern.edu/index.php/Main_Page} 
}
\keyword{datasets}

