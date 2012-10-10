\name{check_stan_names}
\alias{check_stan_names}
\title{Check validity of a STAN name}
\usage{
  check_stan_names(x)
}
\value{
  A \code{logical} vector indicating whether the element is
  a valid Stan variable name.

  A Stan variable must

  \itemize{ \item Start with \code{[A-Za-z]} \item Contain
  only \code{[A-Za-z0-9_]} \item Not conflict with C++ or
  Stan reserved keywords \item Not end with \code{__}.
  \item Not be a reserved word in Stan or C++ }
}
\description{
  Check validity of a STAN name
}
\keyword{internal}

