\name{MPOTERM}
\alias{MPOTERM}
\title{Annotation of MPO Ids to MPO Terms}
\description{
    This data set gives mappings between MPO ids and their
    respective terms.
}
\details{
    Each MPO identifier is mapped to a \code{MPOTerms} object that has 2 slots:
    do_id: MPO Identifier;
    Term: The term for that MPO id


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
    # Convert the object to a list
    xx <- as.list(MPOTERM)
}

\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno}
}
\keyword{datasets}

