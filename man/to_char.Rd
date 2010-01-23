\name{to_char}
\alias{to_char}
\title{Format Oracle Time within an SQL Query}
\description{
  Shorthand notation to paste Oracle \samp{to_char(...)} expressions
  inside queries.
}
\usage{
to_char(colname, fmt="YYYY-MM-DD HH24:MI:SS", nls=NULL, obj=FALSE)
}
\arguments{
  \item{colname}{column name.}
  \item{fmt}{format string.}
  \item{nls}{optional NLS parameter, for example
    \code{"NLS_DATE_LANGUAGE=english"}.}
  \item{obj}{whether the column name was passed as an \R object name.}
}
\details{
  The default behavior (\code{obj=FALSE}) allows users to pass unquoted
  column names,
  \preformatted{  sql(paste("SELECT", to_char(day), "FROM table"))}
  which is slightly easier to read and write than the equivalent call:
  \preformatted{  sql(paste("SELECT", to_char("day"), "FROM table"))}
  If the column name is passed as an \R object, \code{x<-"day"}, then
  \code{obj=TRUE} is required:
  \preformatted{  sql(paste("SELECT", to_char(x,obj=TRUE), "FROM table"))}
}
\value{
  String that can be pasted inside an SQL query. For example,
  \preformatted{  to_char(day)}
  returns
  \preformatted{  "to_char(day,'YYYY-MM-DD HH24:MI:SS') AS day"}
}
\author{Arni Magnusson}
\note{
  By default, the \pkg{ROracle} package imports Oracle \samp{date}
  columns into \R as strings. The format of these strings depends on the
  locale and other system settings, and the default format may not be
  all that useful in \R. The \code{to_char} function can be used to
  specify the exact format.

  The formats \code{"YYYY-MM-DD"} and \code{"YYYY-MM-DD HH24:MI:SS"} can
  be coerced to a POSIX time object using the \R function
  \code{\link{as.POSIXct}}, and \code{\link{sql}} performs this coercion
  automatically by default.

  The Oracle SQL function \samp{to_char(x,fmt,nlsparam)} is a versatile
  tool. See the Oracle manuals for details.
}
\seealso{
  \code{to_char} is to Oracle \samp{date} columns as
  \code{\link{format.POSIXct}} is to \R POSIX time objects.

  \code{to_char} requires familiarity with the \code{\link{paste}}
  function.

  \code{\link{ROracleUI}} gives an overview of the package.
}
\examples{
to_char(day)


\dontrun{


## 1 Import Oracle date column into R as POSIX time class

# Default behavior is to import as strings:
x <- sql("SELECT username,created FROM all_users")
summary(x)  # date column is of class "character"

# Format date column inside query and coerce automatically to POSIX time class:
y <- sql(paste("SELECT username,", to_char(created), "FROM all_users"))
summary(y)  # date column is of class "POSIXct"

# Equivalent query, not using shorthand:
z <- sql("SELECT username,to_char(created,'YYYY-MM-DD HH24:MI:SS') AS created
          FROM all_users")
summary(z)

identical(y,z)


## 2 NLS date example

# Save parameters to use in many queries:
fmt <- "DD Mon YYYY"
nls <- "NLS_DATE_LANGUAGE=english"
sql(paste("SELECT username,", to_char(created,fmt,nls), "FROM all_users"))

# Equivalent query, not using shorthand:
sql("SELECT username, to_char(created,'DD Mon YYYY','NLS_DATE_LANGUAGE=english')
     AS created FROM all_users")

}
}
% Basics
\keyword{chron}
% Programming
\keyword{database}
\keyword{utilities}
