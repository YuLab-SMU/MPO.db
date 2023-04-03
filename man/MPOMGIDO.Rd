\name{MPOMGIDO}
\alias{MPOMGIDO}
\title{Annotation of Gene ENTREZID ID to DO id} 
\description{
    This data set describes associations between gene ENTREZID id 
    and DO ids, based on mh_mapping_initiative and HumanDiseaseOntology.
}
\details{
    Each gene ENTREZID id is mapped to a vector of DO ids.
    Mappings were based on data provided by: MGI, mh_mapping_initiative(Github),
    and HumanDiseaseOntology(Github).
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
    xx <- as.list(MPOMGIDO)
}

\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno} 
and \url{https://github.com/DiseaseOntology/HumanDiseaseOntology} 
and \url{https://github.com/mapping-commons/mh_mapping_initiative} 
}
\keyword{datasets}
