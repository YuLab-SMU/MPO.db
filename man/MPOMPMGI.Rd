\name{MPOMPMGI}
\alias{MPOMPMGI}
\title{Annotation of  MPO Ids to Gene ENTREZID Ids} 
\description{
    This data set describes associations between MPO 
    ids and gene ENTREZID ids, based on the MGI database. 
}
\details{
    Each MPO id is mapped to a vector of gene ENTREZID ids.
    Mappings were based on data provided by: MGI database.
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
    # Convert the object to a list
    xx <- as.list(MPOMPMGI)
}

\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno} 
}
\keyword{datasets}
