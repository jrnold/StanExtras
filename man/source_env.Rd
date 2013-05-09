\name{source_env}
\alias{source_env}
\alias{source_list}
\title{Source file into a new environment or list}
\usage{
  source_env(file, ...,
    env.opts = list(parent = parent.frame()))

  source_list(...)
}
\arguments{
  \item{file}{File name or connection to source}

  \item{env.opts}{Options passed to \code{\link{new.env}}.}

  \item{...}{In \code{source_env}, arguments passed to
  \code{\link{sys.source}}. In \code{source_list},
  arguments passed to \code{source_env}.}
}
\value{
  \describe{ \item{\code{source_env}}{A new
  \code{environment} object.} \item{\code{source_list}}{A
  \code{list} object.} }
}
\description{
  Combines \code{\link{sys.source}} and
  \code{\link{new.env}}.
}

