sql <- function(query, tolower=TRUE, dots=TRUE, posix=TRUE, useBytes=TRUE, warnings=FALSE, debug=FALSE, ...)
{
  on.exit(suppressWarnings(dbUnloadDriver(dbDriver("Oracle"))))

  ## 1  Turn off warnings temporarily, if requested
  if(!warnings)
  {
    owarn <- options(warn=-1)
    on.exit(options(owarn), add=TRUE)
  }

  ## 2  Prepare query
  if(file.exists(query))
  {
    query <- paste(readLines(query,...), collapse=" ")      # read file into one string...
    query <- gsub("[ \t]+", " ", query, useBytes=useBytes)  # ...with single spaces
  }
  query <- gsub(";", "", query, useBytes=useBytes)  # ROracle chokes on semicolons
  if(debug)
    return(query)

  ## 3  Run query
  output <- dbGetQuery(dbConnect("Oracle"), query)

  ## 4  Format output
  attr(output,"row.names") <- seq_len(nrow(output))  # reduce storage size of row names
  if(tolower)
    names(output) <- tolower(names(output))
  if(dots)
    names(output) <- chartr("_", ".", names(output))
  if(posix)
    output <- as.POSIXct(output)

  return(output)
}
