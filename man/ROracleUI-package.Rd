\name{ROracleUI-package}
\alias{ROracleUI-package}
\alias{ROracleUI}
\docType{package}
\title{Convenient Tools for Working with Oracle Databases}
\description{
  Easy-to-use functions to explore Oracle databases and import data into
  \R. User interface for the \pkg{ROracle} package.
}
\details{
  \emph{Explore database:}
  \tabular{ll}{
    \code{\link{tables}} \tab list tables\cr
    \code{\link{views}}  \tab list views
  }
  \emph{Examine table:}
  \tabular{ll}{
    \code{\link{desc}} \tab describe table or view
  }
  \emph{Import data:}
  \tabular{ll}{
    \code{\link{sql}}     \tab import data       \cr
    \code{\link{to_char}} \tab format Oracle time
  }
  \emph{Internal:}
  \tabular{ll}{
    \code{\link{as.POSIXct.data.frame}} \tab coerce to POSIX time
  }
}
\author{Arni Magnusson}
\references{
  The official Oracle manuals are available at
  \url{http://www.oracle.com/technology/documentation/index.html}.
}
\seealso{
  The functions that do the actual work are described in the \pkg{DBI}
  and \pkg{ROracle} packages.
}
% Programming
\keyword{database}
\keyword{utilities}
