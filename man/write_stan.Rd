\name{write_stan}
\alias{write_stan}
\alias{write_stan-methods}
\alias{write_stan,character-method}
\alias{write_stan,list-method}
\title{Methods for function \code{write.stan}}
\arguments{
  \item{list}{Data objects to write to file}

  \item{file}{\code{character} output file name}

  \item{append}{\code{logical} If \code{TRUE}, then append
  to the file.}

  \item{scalars}{\code{character} Names of scalar
  variables.}

  \item{envir}{\code{environment} In which to look for
  varaibles.}

  \item{precheck}{\code{logical} Check for errors before
  writing to file.}

  \item{width}{\code{numeric} Width of lines in the output
  file.}
}
\description{
  This writes dump files, like \code{\link{dump}}, but with
  a few tweaks to ensure that they will works as input for
  Stan.
}

