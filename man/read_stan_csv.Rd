\name{read_stan_csv}
\alias{read_stan_csv}
\title{Read STAN output}
\usage{
  read_stan_csv(file)
}
\arguments{
  \item{file}{\code{character} name of an output file
  produced by a STAN model.}
}
\value{
  A \code{data.frame} with attributes \describe{
  \item{\code{header}}{A \code{list} containing header
  information} \item{\code{rejected}}{A \code{logical}
  vector indicating whether the iteration was a rejected
  sample (for samples only)} }
}
\description{
  Read csv files produced by Stan. This returns both the
  sample values and the metadata in the comments of the
  file.
}

