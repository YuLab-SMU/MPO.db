\name{MPO.db}
\alias{MPO.db}
\alias{MPO}
\title{Bioconductor annotation data package}
\description{
    Welcome to the MPO.db annotation Package. The purpose of this package
    is to provide detailed information about the latest version of the
    Mouse Phenotype Ontology. It also provides the gene annotation of
    phenotype and disease.
    This package is updated biannually.
    You can learn what objects this package supports with the following command:
    \code{ls("package:MPO.db")}
    Each of these objects has their own manual page detailing where
    relevant data was obtained along with some examples of how to use it.
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
    ls("package:MPO.db")
}


\keyword{datasets}

