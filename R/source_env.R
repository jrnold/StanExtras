#' @include package.R
#' @export source_env source_list
NULL

#' Source file into a new environment or list
#'
#' Combines \code{\link{sys.source}} and \code{\link{new.env}}.
#'
#' @param file File name or connection to source
#' @param env.opts Options passed to \code{\link{new.env}}.
#' @param ... In \code{source_env}, arguments passed to \code{\link{sys.source}}.
#' In \code{source_list}, arguments passed to \code{source_env}.
#' @return
#' \describe{
#' \item{\code{source_env}}{A new \code{environment} object.}
#' \item{\code{source_list}}{A \code{list} object.}
#' }
source_env <- function(file, ..., env.opts = list(parent=parent.frame()) ) {
    e <- do.call(new.env, env.opts)
    sys.source(file, envir=e, ...)
    e
}

#' @rdname source_env
source_list <- function(...) {
    as.list(source_env(...))
}
