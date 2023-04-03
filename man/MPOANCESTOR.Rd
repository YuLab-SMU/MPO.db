\name{MPOANCESTOR}
\alias{MPOANCESTOR}
\title{Annotation of MPO Identifiers to their Ancestors}
\description{
    This data set describes associations between MPO
    ids and their ancestor ids, based on the directed acyclic
    graph (DAG) defined by the Mouse Phenotype Ontology Consortium.
    The format is an R
    object mapping the MPO terms to all ancestor terms, where an
    ancestor term is a more general MPO term that precedes
    the given MPO term in the DAG (in other words, the parents, and all
    their parents, etc.).
}
\details{
    Each MPO    term is mapped to a vector of ancestor MPO  terms.
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
    xx <- as.list(MPOANCESTOR)
}

\references{
\url{http://do-wiki.nubic.northwestern.edu/index.php/Main_Page}
}
\keyword{datasets}

