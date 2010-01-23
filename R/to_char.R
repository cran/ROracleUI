to_char <- function(colname, fmt="YYYY-MM-DD HH24:MI:SS", nls=NULL, obj=FALSE)
{
  if(!obj)
    colname <- as.character(substitute(colname))

  if(!is.null(nls))
    nls <- paste(",'", nls, "'", sep="")

  output <- paste("to_char(", colname, ",'", fmt, "'", nls, ") AS ", colname, sep="")

  return(output)
}
