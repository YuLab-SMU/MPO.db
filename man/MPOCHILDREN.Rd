\name{MPOCHILDREN}
\alias{MPOCHILDREN}
\title{Annotation of MPO Identifiers to their Children} 
\description{
    This data set describes associations between MPO 
    terms and their ancesto terms, based on the directed acyclic
    graph (DAG) defined by the Mouse Phenotype Ontology Consortium. The format is an R
    object mapping the MPO terms to all ancestor terms, where an
    ancestor term is a more general MPO term that precedes
    the given MPO term in the DAG (in other words, the parents, and all
    their parents, etc.).
}
\details{
    Each MPO    term is mapped to a vector of ancestor MPO    terms.
    Mappings were based on data provided by: Mouse Phenotype Ontology
    With a date stamp from the source of: 20230302


}

\examples{
    # Convert the object to a list
    xx <- as.list(MPOCHILDREN)
}

\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno} 
}
\keyword{datasets}
