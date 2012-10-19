##' Run a compiled STAN model.
##'
##' This is a simple wrapper for running a compiled Stan model via the
##' commmand line. It does not handle multiple chains. However, it
##' will convert R data to files.
##'
##' @param model \code{character} The path to the compiled STAN model.
##' @param data \code{list} with data or \code{character} specifying
##' the file name. If \code{NULL}, do not pass any data.
##' @param init \code{list} with data; \code{character} specifying the
##' file name; If \code{NULL}, do not set initial values.
##' @param samples \code{character} Path in which to save the samples. The default is a
##' temporary file.
##' @param scalars \code{character} vector with the names of variables
##' (in both data and init) which are scalars. This is needed so that
##' scalar values are correctly written to the dump file.
##' @param ... Options to be passed to the command line. The arguments
##' names and values of the arguments in \code{...} are converted to
##' command line arguments. E.g. the argument \code{seed=1234}
##' produces the command line option \code{--seed=1234}.  To specify a
##' boolean option, set the argument to \code{TRUE}. E.g. \code{test_grad=TRUE}.
##'
##' @export
run_stan_model <- function(model, data=NULL,
                           init=NULL, samples=NULL,
                           scalars = character(),
                           ...) {
    opts <- list(...)
    optstring <- c()
    ## Set data
    if (!is.null(data)) {
        if (is.list(data)) {
            data.file <- tempfile(fileext=".R")
            writeStan(list=data, file=data.file, scalars=scalars)
        } else if (is.character(data)) {
            data.file <- data
        } else {
            stop("Argument 'data' must be an object of class: list, character, or NULL")
        }
        optstring <- c(optstring, sprintf("--data=%s", shQuote(data.file)))
    }
    ## Set initial values
    if (!is.null(init)) {
        if (is.list(init)) {
            init.file <- tempfile(fileext=".R")
            writeStan(list=init, file=init.file, scalars=scalars)
        } else if (is.character(init)) {
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
    system2(sprintf("%s", model), args=optstring)
}


