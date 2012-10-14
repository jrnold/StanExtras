\name{read_stan_csv}
\alias{read_stan_csv}
\title{Read STAN output}
\usage{
  read_stan_csv(files, chain_id = seq_along(files))
}
\arguments{
  \item{file}{\code{character} name of output file produced
  by a STAN model.}

  \item{chain_id}{\code{integer} Values of \code{chain_id}
  to use for each chain. These are used instead of the
  values in the header of the csv are ignored.}
}
\value{
  \code{\link[mcmc4]{McmcLong}} object.
}
\description{
  Read csv files produced by Stan.
}
\details{
  This returns both the data in the csv, as well as all the
  metadata in the header of the file.
}

