\name{MPOPARENTS}
\alias{MPOPARENTS}
\title{Annotation of MPO Identifiers to their Parents}
\description{
    This data set describes associations between MPO
    terms and their direct parent terms, based on the directed acyclic
    graph (DAG) defined by the Mouse Phenotype Ontology Consortium.
    The format is an R
    object mapping the MPO terms to all direct parent terms, where a
    direct parent term is a more general MPO term that immediately precedes
    the given MPO term in the DAG.
}
\details{
    Each MPO    term is mapped to a named vector of MPO    terms. The name
    associated with the parent term will be either \emph{isa}, \emph{partof},
    where \emph{isa} indicates that the child term is a more specific version
    of the parent, and    \emph{partof} indicate that the
    child term is a part of the parent.


    Mappings were based on data provided by: Mouse Phenotype Ontology
    With a date stamp from the source of: 20230302


}
\examples{
    xx <- as.list(MPOPARENTS)
}
\references{
\url{http://www.informatics.jax.org/downloads/reports/index.html#pheno}
}
\keyword{datasets}

