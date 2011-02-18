as.POSIXct.data.frame <- function(x, ...)
{
  if(is.list(x))
  {
    as.data.frame(lapply(x, as.POSIXct.data.frame, ...))
  }
  else
  {
    attempt <- try(as.POSIXct(x,...), silent=TRUE)

    if(!("POSIXct" %in% class(attempt)))
      output <- x
    else if(length(attempt) != length(x))
      output <- x
    else if(sum(is.na(attempt)) != sum(is.na(x)))
      output <- x
    else
      output <- attempt

    return(output)
  }
}
