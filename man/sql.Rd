\name{sql}
\alias{sql}
\title{Import Data from Oracle}
\description{Run SQL query returning an \R data frame.}
\usage{
sql(query, tolower=TRUE, dots=TRUE, posix=TRUE, useBytes=TRUE,
    warnings=FALSE, debug=FALSE, ...)
}
\arguments{
  \item{query}{string containing SQL query or the name of a file
    containing a query.}
  \item{tolower}{whether column names should be lowercased.}
  \item{dots}{whether underscores in column names should be replaced
    with dots, converting \samp{col_name} to \samp{col.name}.}
  \item{posix}{whether to try coercing Oracle \samp{date} columns to
    class \code{POSIXct}.}
  \item{useBytes}{passed to \code{gsub}.}
  \item{warnings}{whether to show warnings, e.g. when Oracle columns are
    of type \samp{LONG}.}
  \item{debug}{whether to return the finalized SQL query string, instead
    of submitting it to Oracle.}
  \item{\dots}{passed to \code{readLines}.}
}
\details{
  The \code{query} is not required to end with a semicolon. In fact,
  semicolons are removed internally before submitting the query to
  Oracle.

  \code{posix=TRUE} can only coerce Oracle \samp{date} columns to
  \code{POSIXct} if the time format is recognized by
  \code{\link{as.POSIXct}}. Specifically, \code{sql} calls
  \code{\link{as.POSIXct.data.frame}} to try coercing. To achieve such a
  time format, the \code{\link{to_char}} shorthand notation can be
  helpful.

  The arguments \code{useBytes} and \code{\dots} enable the user to
  solve character encoding problems within the SQL query. If the query
  contains non-ASCII characters, \code{\link{gsub}} and
  \code{\link{readLines}} (called by \code{sql}) may convert the query
  to a different encoding than the Oracle database expects.

  \code{debug=TRUE} is helpful for solving problems, and also to save
  complex queries (possibly to a file) for later use.
}
\value{
  Data frame containing the imported data, or a simple string if
  \code{debug=TRUE}.
}
\author{Arni Magnusson}
\seealso{
  \code{sql} is to Oracle tables as \code{\link{read.table}} is to text
  files.

  \code{\link{to_char}}, \code{\link{as.POSIXct}}, and
  \code{\link{as.POSIXct.data.frame}} are discussed in the details
  above.

  \code{\link{ROracleUI}} gives an overview of the package.
}
\examples{
\dontrun{


## 1 Basic queries

# Pass query as a string
sql("SELECT username,created FROM all_users WHERE rownum<=10")

# Pass query as a file
write(c("SELECT username, created",
        "FROM all_users",
        "WHERE rownum <= 10;"), "query.sql")
sql("query.sql")


## 2 Excerpt from the help(to_char) examples

# Default behavior is to import as strings:
x <- sql("SELECT username,created FROM all_users")
summary(x)  # date column is of class "character"

# Format date column inside query and coerce automatically to POSIX time class:
y <- sql(paste("SELECT username,", to_char(created), "FROM all_users"))
summary(y)  # date column is of class "POSIXct"

}
}
% Programming
\keyword{database}
\keyword{utilities}
