\name{run_stan_model}
\alias{run_stan_model}
\title{Run a compiled STAN model.}
\usage{
  run_stan_model(model, data = NULL, init = NULL,
    samples = NULL, scalars = character(), ...)
}
\arguments{
  \item{model}{\code{character} The path to the compiled
  STAN model.}

  \item{data}{\code{list} with data or \code{character}
  specifying the file name. If \code{NULL}, do not pass any
  data.}

  \item{init}{\code{list} with data; \code{character}
  specifying the file name; If \code{NULL}, do not set
  initial values.}

  \item{samples}{\code{character} Path in which to save the
  samples. The default is a temporary file.}

  \item{scalars}{\code{character} vector with the names of
  variables (in both data and init) which are scalars. This
  is needed so that scalar values are correctly written to
  the dump file.}

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
  This is a simple wrapper for running a compiled Stan
  model via the commmand line. It does not handle multiple
  chains. However, it will convert R data to files.
}

