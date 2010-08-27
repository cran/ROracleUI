\name{as.POSIXct.data.frame}
\alias{as.POSIXct.data.frame}
\title{Coerce Data Frame Columns to POSIX Time}
\description{
  Test whether each column in a data frame can be coerced to class
  \code{POSIXct}, and perform the coercion where appropriate.
}
\usage{
\method{as.POSIXct}{data.frame}(x, \dots)
}
\arguments{
  \item{x}{data frame, possibly containing columns that can be coerced
    to class \code{POSIXct}.}
  \item{\dots}{passed to \code{as.POSIXct}.}
}
\value{
  A data frame like \code{x}, except some of the columns may have been
  coerced to class \code{POSIXct}.
}
\note{
  A column must pass three sequential tests before being coerced:
  \enumerate{
    \item{The resulting object must of class \code{POSIXct}.}
    \item{Its length must equal the length of the original column.}
    \item{The number of \code{NA} values must equal the number of
      \code{NA} values in the original column.}
  }
}
\seealso{
  \code{\link{as.POSIXct}} invokes \code{as.POSIXct.data.frame} if the
  main argument is a data frame.

  The \code{\link{POSIXct}} class is a compact and reliable date-time
  class.

  The default behavior of \code{\link{sql}} is to coerce all appropriate
  columns imported from Oracle to class \code{POSIXct}.

  \code{\link{ROracleUI}} gives an overview of the package.
}
\examples{
x <- data.frame(Event=c("start","end"), Date=c("1935-01-08","1977-08-16"),
                stringsAsFactors=FALSE)
summary(x)  # date column is of class "character"

y <- as.POSIXct(x)
summary(y)  # date column is of class "POSIXct"
}
% Basics
\keyword{chron}
% Programming
\keyword{utilities}
