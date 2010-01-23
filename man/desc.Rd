\name{desc}
\alias{desc}
\title{Describe Oracle Table}
\description{
  Show the column names of an Oracle table (or view) and various column
  features, not unlike the Oracle SQL*Plus \command{DESC} command. Also
  show the number of rows when the table was last analyzed.
}
\usage{
desc(table, tolower=TRUE, dots=FALSE)
}
\arguments{
  \item{table}{Oracle table name, often in the \samp{owner.table}
    format.}
  \item{tolower}{whether output table strings should be lowercased.}
  \item{dots}{whether underscores in column names should be replaced
    with dots, converting \samp{col_name} to \samp{col.name}.}
}
\value{
  A data frame with named rows and the following columns:
  \item{name}{Oracle column name.}
  \item{Sclass}{storage mode in \R.}
  \item{type}{Oracle type.}
  \item{len}{Oracle length.}
  \item{precision}{Oracle precision.}
  \item{scale}{Oracle scale.}
  \item{isVarLength}{whether the variable has varying length in Oracle.}
  \item{nullOK}{whether the variable can be null.}
  Furthermore, the data frame contains two attributes: \code{rows} (the
  number of rows when the table was last analyzed) and \code{analyzed}
  (when the table was last analyzed). These attributes are not available
  for all Oracle tables.
}
\author{Arni Magnusson}
\note{
  See the Oracle manuals for details about type, length, precision,
  scale, and nulls.
}
\seealso{
  \code{desc} is to Oracle tables as \code{ll} (in package \pkg{gdata})
  is to \R data frames.

  \code{\link{ROracleUI}} gives an overview of the package.
}
\examples{
\dontrun{

desc("dual", tolower=FALSE)

desc("all_users")
}
}
% Basics
\keyword{classes}
% Programming
\keyword{database}
\keyword{utilities}
