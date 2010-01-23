desc <- function(table, tolower=TRUE, dots=FALSE)
{
  on.exit(suppressWarnings(dbUnloadDriver(dbDriver("Oracle"))))

  ## 1  Fetch table description
  query <- paste("SELECT * FROM", table)
  output <- dbColumnInfo(dbSendQuery(dbConnect("Oracle"), query))

  ## 2  Handle row and column names
  attr(output,"row.names") <- seq_len(nrow(output))
  if(tolower)
    output <- transform(output, name=tolower(name), type=tolower(type))
  if(dots)
    output$name <- chartr("_", ".", output$name)

  ## 3  Add row count info
  splitname <- toupper(unlist(strsplit(table, "\\.")))
  select.from <- "SELECT num_rows,last_analyzed FROM all_tables"
  if(length(splitname) == 1)
    where <- paste("WHERE table_name='", splitname, "'", sep="")
  else
    where <- paste("WHERE owner='", splitname[1], "' AND table_name='", splitname[2], "'", sep="")
  query <- paste(select.from, where)
  rows.date <- dbGetQuery(dbConnect("Oracle"), query)
  attr(output, "rows") <- rows.date$NUM_ROWS
  attr(output, "analyzed") <- rows.date$LAST_ANALYZED

  ## 4  Show on screen
  cat("\n")
  print(output, right=FALSE)
  cat("\n")
  if(nrow(rows.date) > 0)
  {
    indent <- rep(" ", nchar(nrow(output))+1)
    cat(indent, attr(output,"rows"), " rows on ", attr(output,"analyzed"), "\n\n", sep="")
  }

  invisible(output)
}
