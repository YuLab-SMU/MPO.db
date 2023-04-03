\name{MPOmetadata}
\alias{MPOmetadata}
\title{Annotation of MPO Meta Data}
\description{
    This data set gives information of MPO meta data.
}
\details{
    Mappings were based on data provided by: Mouse Phenotype Ontology
    With a date stamp from the source of: 20230302
}

\value{
    \code{MPO_dbconn}: a DBIConnection object representing an
    open connection to the package annotation DB.

    \code{MPO_dbfile}: a character string with the path to the
    package annotation DB.

    \code{MPO_dbschema}: none (invisible \code{NULL}).

    \code{MPO_dbInfo}: none (invisible \code{NULL}).
}

\examples{
    # Convert the object to a data.frame
    library(AnnotationDbi)
    xx <- toTable(MPOmetadata)
}
\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno}
}
\keyword{datasets}

