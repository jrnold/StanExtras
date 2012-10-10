\name{run_stan_model}
\alias{run_stan_model}
\title{Run a compiled STAN model.}
\usage{
  run_stan_model(model, data = NULL, init = NULL,
    samples = NULL, data.file = NULL, init.file = NULL,
    return.samples = TRUE, ...)
}
\arguments{
  \item{model}{\code{character} The path to the compiled
  STAN model.}

  \item{data}{\code{list}. A list with data.}

  \item{data.file}{\code{characer}. Name of file with data,
  or file to save \code{data} to.}

  \item{init}{\code{character}. A list with initial
  values.}

  \item{data.file}{\code{characer}. Name of file with
  initial values, or file to save \code{init} to.}

  \item{samples}{\code{character} Path in which to save the
  samples. The default is a temporary file.}

  \item{return.samples}{\code{logical}. Return samples to}

  \item{data.file}{\code{character}. Name of file with data
  values.}

  \item{...}{Options to be passed to the command line. The
  arguments names and values of the arguments in \code{...}
  are converted to command line arguments. E.g. the
  argument \code{seed=1234} produces the command line
  option \code{--seed=1234}.  To specify a boolean option,
  set the argument to \code{TRUE}. E.g.
  \code{test_grad=TRUE}.}
}
\description{
  Run a compiled STAN model.
}

