\docType{methods}
\name{write_rdump-methods}
\alias{write_rdump}
\alias{write_rdump,character-method}
\alias{write_rdump,environment-method}
\alias{write_rdump,list-method}
\alias{write_rdump-methods}
\title{Write Stan-compatible R-dump}
\arguments{
  \item{list}{Objects}

  \item{...}{Pass to \code{\link[rstan]{stan_rdump}}.}
}
\description{
  Generic method that wraps \code{\link[rstan]{stan_rdump}}
  to accept \code{list}, \code{environment}, and
  \code{character}.
}
\seealso{
  \code{\link[rstan]{stan_rdump}}.
}
\keyword{methods}

