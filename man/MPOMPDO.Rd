\name{MPOMPDO}
\alias{MPOMPDO}
\title{Annotation of  MPO Identifiers to DO ID} 
\description{
    This data set describes associations between MPO 
    ids and DO ids, based on mh_mapping_initiative and HumanDiseaseOntology. 
    The format is an R
    object mapping the MPO ids to DO ids.
}
\details{
    Each MPO id is mapped to a vector of DO ids.
    Mappings were based on data provided by: mh_mapping_initiative and 
    HumanDiseaseOntology.
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
    xx <- as.list(MPOMPDO)
}

\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno} 
and \url{https://github.com/DiseaseOntology/HumanDiseaseOntology} 
and \url{https://github.com/mapping-commons/mh_mapping_initiative} 
}
\keyword{datasets}
