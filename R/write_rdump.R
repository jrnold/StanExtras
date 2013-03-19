##' Write Stan-compatible R-dump
##'
##' Generic method that wraps \code{\link[rstan]{stan_rdump}} to
##' accept \code{list}, \code{environment}, and \code{character}.
##'
##' @param list Objects
##' @param ... Pass to \code{\link[rstan]{stan_rdump}}.
##'
##' @name write_rdump-methods
##' @rdname write_rdump-methods
##' @aliases write_rdump
##' @aliases write_rdump-methods
##' @aliases write_rdump,character-method
##' @aliases write_rdump,environment-method
##' @aliases write_rdump,list-method
##' @keywords methods
##' @docType methods
##'
##' @seealso \code{\link[rstan]{stan_rdump}}.
##' @export
setGeneric("write_rdump",
           function(list, ...) {
               standardGeneric("write_rdump")
           })

write_rdump_character <- function(list, envir=parent.frame(), ...) {
    stan_rdump(list, envir=envir, ...)
}

setMethod("write_rdump", "character", write_rdump_character)

setMethod("write_rdump", "environment",
          function(list, ...) {
              callGeneric(list=ls(list), envir=list, ...)              
          })

setMethod("write_rdump", "list",
          function(list, ...) {
              callGeneric(list=as.environment(list), ...)
          })
