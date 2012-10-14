\name{McmcLongStan}
\alias{McmcLongStan}
\title{Read STAN output}
\usage{
  McmcLongStan(files, chain = seq_along(files))
}
\arguments{
  \item{file}{\code{character} name of output file produced
  by a STAN model.}
}
\value{
  \code{McmcStan} object.
}
\description{
  Read STAN output
}

