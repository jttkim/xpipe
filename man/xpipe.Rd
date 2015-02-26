\name{xpipe}
\alias{xpipe}
\title{Read and Write to Piped Process}
\description{Process data from R through a subprocess. The subprocess
  receives its input from a child, and the main R process can read
  the output produced by the subprocess.
}
\usage{
xpipe(cmd, input, type = "lines")
}
\arguments{
  \item{cmd}{the command to start the subprocess.}
  \item{input}{a variable or function providing the input for the
    subprocess.}
  \item{type}{the type of data to be returned}
}
\value{the output produced by the subprocess, as a character vector}

\details{
  A subprocess is started using \code{cmd} and an OS-level pipe is
  set up and connected to the standard output of the subprocess.
  Then, a child is forked from the main R process to provide input
  to the subprocess.

  An OS-level pipe is different from what the \code{pipe} function
  in the base backage of R provides. The Unix/Linux equivalent the base
  package's \code{pipe} function is \code{popen}.

  The argument \code{input} must be a variable that can be coerced to
  character. Each element, coerced to character, is written as a line
  to the standard output of the subprocess.

  As an idea for the future, the input argument could also be a
  function which would be called to obtain line after line, until
  it returns a \code{NA} value to indicate EOF.

  Each line is automatically appended by a newline character. FIXME:
  This could be controlled by a defaulted parameter in the future.

  FIXME:
  The \code{type} parameter will allow setting a type like
  \code{data.frame}, \code{raw} (for receiving the output in one string,
  and more in the future, but currently, only the default \code{"lines"}
  is supported.
}

\note{
  The \code{xpipe} function for type \code{"lines"} will not work
  properly if the output strings contain nullbytes. Use the \code{"raw"}
  type (in the future) if you expect nullbytes.
}

\examples{
# start another R process and get the output of some
# familiar commands:
library(xpipe);
r.results <- xpipe("R --vanilla", c("version", "set.seed(4711)", "x <- runif(10)", "x", "which(x > 0.5)"));
print(r.results);

\dontrun{
# this produces a funny, crumbly "R" logo using netpbm:
# (requires netpbm and ImageMagick for ``display'')
library(xpipe);
dotmatrixR <- c(
  "P2",
  "7 9",
  "256",
  "256 256 256 256 256 256 256",
  "256   0   0   0   0 256 256",
  "256   0 256 256 256   0 256",
  "256   0 256 256 256   0 256",
  "256   0   0   0   0 256 256",
  "256   0 256   0 256 256 256",
  "256   0 256 256   0 256 256",
  "256   0 256 256 256   0 256",
  "256 256 256 256 256 256 256");
crumblyR <- xpipe("pnmenlarge 20 | ppmspread 26 | pamedge | pnminvert | pnmnoraw", dotmatrixR);
# This uses an ``R-level pipe'' to display the image:
p <- pipe("display", "w");
cat(crumblyR, file = p);
close(p);
}
}
\keyword{programming}
\keyword{IO}
\keyword{utilities}

