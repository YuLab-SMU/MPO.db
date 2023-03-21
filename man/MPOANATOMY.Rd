\name{MPOANATOMY}
\alias{MPOANATOMY}
\title{Annotation of MPO id to their anatomy}
\description{
    This data set describes associations between MPO
    terms and their ancestor terms, based on the directed acyclic
    graph (DAG) defined by the Mouse Phenotype Ontology Consortium.
    The format is an R
    object mapping the MPO terms to all ancestor terms, where an
    ancestor term is a more general MPO term that precedes
    the given MPO term in the DAG (in other words, the parents, and all
    their parents, etc.).
}
\details{
    Each MPO id is mapped to an anatomy.
    Mappings were based on data provided by: MGI database.
}

\examples{
    # Convert the object to a list
    xx <- as.list(MPOANATOMY)
}

\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno}
}
\keyword{datasets}

