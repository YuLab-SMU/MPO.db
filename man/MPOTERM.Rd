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

\examples{
    # Convert the object to a list
    xx <- as.list(MPOTERM)
}
\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno}
}
\keyword{datasets}

