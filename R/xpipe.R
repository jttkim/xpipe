xpipe <- function(cmd, input, type = "lines")
{
  if (!is.character(cmd))
  {
    error("cmd must be a character");
  }
  if (length(cmd) != 1)
  {
    error("cmd must be exactly one string");
  }
  flush(stdout());
  flush(stderr());
  retval <- .Call("Rxpipe_xpipe", cmd = cmd, input = as.character(input), type = as.character(type), PACKAGE = "xpipe");
  return(retval);
}

