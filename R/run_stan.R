##' Run a compiled STAN model.
##'
##' @param model \code{character} The path to the compiled STAN model.
##' @param data \code{list}. A list with data.
##' @param data.file \code{characer}. Name of file with data, or file
##' to save \code{data} to.
##' @param init \code{character}. A list with initial values.
##' @param data.file \code{characer}. Name of file with initial values, or file
##' to save \code{init} to.
##' @param samples \code{character} Path in which to save the samples. The default is a
##' temporary file.
##' @param return.samples \code{logical}. Return samples to
##' @param data.file \code{character}. Name of file with data values.
##' @param ... Options to be passed to the command line. The arguments
##' names and values of the arguments in \code{...} are converted to
##' command line arguments. E.g. the argument \code{seed=1234}
##' produces the command line option \code{--seed=1234}.  To specify a
##' boolean option, set the argument to \code{TRUE}. E.g. \code{test_grad=TRUE}.
##'
##' @export
run_stan_model <- function(model, data=NULL,
                           init=NULL, samples=NULL,
                           data.file=NULL, init.file=NULL,
                           return.samples=TRUE, ...) {
    opts <- list(...)
    optstring <- c()
    if (!is.null(data)) {
        if (is.null(data.file)) {
            data.file <- tempfile(fileext=".R")
        }
        write.stan(data, file=data.file)
    }
    optstring <- c(optstring, sprintf("--data=%s", shQuote(data.file)))
    if (!is.null(init)) {
        if (is.null(init.file)) {
            init.file <- tempfile(fileext=".R")
        }
        init.file <- tempfile(fileext=".R")
        write.stan(init, file=init.file)
    }
    if (!is.null(init.file)) {
        optstring <- c(optstring, sprintf("--init=%s", init.file))
    }
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


