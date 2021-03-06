#' @include package.R
#' @export run_stan_model
NULL

#' Run a compiled STAN model.
#'
#' This is a simple wrapper for running an already compiled Stan model via the
#' commmand line. 
#" 
#' It does not handle multiple chains; this could be handled with
#' a loop. 
#'
#' @param model \code{character} The path to the compiled STAN model.
#' @param data \code{list} with data or \code{character} specifying
#' the file name. If \code{NULL}, do not pass any data.
#' @param init \code{list} with data; \code{character} specifying the
#' file name; If \code{NULL}, do not set initial values.
#' @param samples \code{character} Path in which to save the samples. The default is a
#' temporary file.
#' @param stdout Passed to \code{\link{system2}}.
#' @param stderr Passed to \code{\link{system2}}.
#' @param ... Options to be passed to the command line. The arguments
#' names and values of the arguments in \code{...} are converted to
#' command line arguments. E.g. the argument \code{seed=1234}
#' produces the command line option \code{--seed=1234}.  To specify a
#' boolean option, set the argument to \code{TRUE}. E.g. \code{test_grad=TRUE}
#' becomes \code{--test_grad}.
#'
#' @return Invisibly returns a \code{character} vector with the name of the output file.
run_stan_model <- function(model, data=NULL, init=NULL, samples=NULL,
                           stdout="", stderr="", ...) {
  opts <- list(...)
  optstring <- c()
  ## Set data
  if (!is.null(data)) {
    if (is.list(data)) {
      if (is.list(data)) {
        data <- as.environment(data)
      }
      data.file <- tempfile(fileext=".R")
      stan_rdump(list=ls(data), file=data.file, envir=data)
    } else if (is.character(data)) {
      if (length(data) > 1) {
        warning(sprintf("%s has length > 1; only first element used",
                        sQuote("data")))
        data <- data[1]
      }
      if (!file.exists(data)) {
        stop(sprintf("file %s does not exist", dQuote(data)))
      }
      data.file <- data
    } else {
      stop("Argument 'data' must be an object of class: list, character, or NULL")
    }
    optstring <- c(optstring, sprintf("--data=%s", shQuote(data.file)))
  }
  ## Set initial values
  if (!is.null(init)) {
    if (is.list(init)) {
      init <- as.environment(init)
      init.file <- tempfile(fileext=".R")
      stan_rdump(ls(init), file=init.file, envir=init)
    } else if (is.character(init)) {
      if (length(init) > 1) {
        warning(sprintf("%s has length > 1; only first element used",
                        sQuote("init")))
        init <- init[1]
      }
      if (!file.exists(init)) {
        stop(sprintf("file %s does not exist", dQuote(init)))
      }
      init.file <- init
    } else {
      stop("Argument 'init' must be an object of class: list, character, or NULL")
    }
    optstring <- c(optstring, sprintf("--init=%s", init.file))
  }
  ## Samples
  if (is.null(samples)) {
    samples <- tempfile(fileext=".csv")
  }
  optstring <- c(optstring, sprintf("--samples=%s", samples))
  for (i in seq_along(opts)) {
    optname <- names(opts)[[i]]
    optvalue <- opts[[i]]
    if (is.logical(optvalue) && isTRUE(optvalue)) {
      optstring <- c(optstring, sprintf("--%s", optname))
    } else {
      optstring <- c(optstring, sprintf("--%s=%s", optname, as.character(optvalue)))
    }
  }
  ## TODO: capture stdout/stderr
  system2(sprintf("%s", model), args=optstring, stdout=stdout, stderr=stderr)
  invisible(samples)
}
