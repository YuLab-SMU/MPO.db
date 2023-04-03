\name{MPOSYNONYM}
\alias{MPOSYNONYM}
\title{Map from MPO synonyms to MPO terms}
\description{
    MPOSYNONYM is an R object that provides mapping
    from MPO synonyms to MPO terms
}
\details{
    Mappings were based on data provided by: Mouse Phenotype Ontology
    With a date stamp from the source of: 20230302


}

\examples{
    # Convert the object to a list
    xx <- as.list(MPOSYNONYM)
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
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno} 
}


\keyword{datasets}

