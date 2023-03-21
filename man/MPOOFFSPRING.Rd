\name{MPOOFFSPRING}
\alias{MPOOFFSPRING}
\title{Annotation of MPO Ids to their Offspring}
\description{
    This data set describes associations between MPO
    ids and their offspring ids, based on the directed acyclic
    graph (DAG) defined by the Mouse Phenotype Ontology Consortium.
    The format is an R
    object mapping the MPO terms to all offspring terms, where an
    ancestor term is a more specific MPO term that is preceded
    by the given MPO term in the DAG (in other words, the children and all
    their children, etc.).
}
\details{
    Each MPO term is mapped to a vector of offspring MPO    terms.
    Mappings were based on data provided by: Mouse Phenotype Ontology
    With a date stamp from the source of: 20230302
}

\examples{
    # Convert the object to a list
    xx <- as.list(MPOOFFSPRING)
}
\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno}
}
\keyword{datasets}

