cat0 <- function(..., sep="") cat(..., sep=sep)

## List of C++ keywords from http://en.cppreference.com/w/cpp/keyword
CPP_RESERVED <-
    c('alignas', 'alignof', 'and', 'and_eq', 'asm', 'auto', 'bitand', 'bitor',
      'bool', 'break', 'case', 'catch', 'char', 'char16_t', 'char32_t', 'class',
      'compl', 'const', 'constexpr', 'const_cast', 'continue', 'decltype',
      'default', 'delete', 'do', 'double', 'dynamic_cast', 'else', 'enum',
      'explicit', 'export', 'extern', 'false', 'float', 'for', 'friend', 'goto',
      'if', 'inline', 'int', 'long', 'mutable', 'namespace', 'new', 'noexcept',
      'not', 'not_eq', 'nullptr', 'operator', 'or', 'or_eq', 'private', 'protected',
      'public', 'register', 'reinterpret_cast', 'return', 'short', 'signed',
      'sizeof', 'static', 'static_assert', 'static_cast', 'struct', 'switch',
      'template', 'this', 'thread_local', 'throw', 'true', 'try', 'typedef',
      'typeid', 'typename', 'union', 'unsigned', 'using', 'virtual', 'void',
      'volatile', 'wchar_t', 'while', 'xor', 'xor_eq', 'override', 'final')
## List of Stan reserved keywords
## See 16.2 of Stan Manual
STAN_RESERVED <-
    c('for', 'in', 'while', 'repeat', 'until', 'if', 'then', 'else',
      'true', 'false', 'int', 'real', 'vector', 'simplex', 'ordered',
      'row_vector', 'matrix', 'corr_matrix', 'cov_matrix')

##' Check validity of a STAN name
##'
##' @return A \code{logical} vector indicating whether the element is
##' a valid Stan variable name.
##'
##' A Stan variable must
##'
##' \itemize{
##' \item Start with \code{[A-Za-z]}
##' \item Contain only \code{[A-Za-z0-9_]}
##' \item Not conflict with C++ or Stan reserved keywords
##' \item Not end with \code{__}.
##' \item Not be a reserved word in Stan or C++
##' }
##'
##' @keywords internal
check_stan_names <- function(x) {
    (grepl("^[A-Za-z][A-Za-z0-9_]*$", x)
     & !grepl("__$", x)
     & ! x %in% union(CPP_RESERVED, STAN_RESERVED))
}

##' Truncate to non-infinity
##'
##' @param x \code{numeric} vector
##' @return \code{numeric} vector with \code{Inf} and \code{-Inf} replaced
##' with the smallest and largest floating point number.
##' @keywords internal
trunc_inf <- function(x) {
    x[x == Inf] <- .Machine$double.xmax
    x[x == -Inf] <- -.Machine$double.xmax
    x
}

.write.stan <- function(list, file = "standata.R", append = FALSE, envir = parent.frame(),
                        precheck=TRUE, width = options("width")$width,
                        ...)
{
    ## copied from base::dump
    if (is.character(file)) {
        ex <- sapply(list, exists, envir = envir)
        if (!any(ex))
            return(invisible(character()))
        if (nzchar(file)) {
            file <- file(file, ifelse(append, "a", "w"))
            on.exit(close(file), add = TRUE)
        }
        else {
            file <- stdout()
        }
    }

    ## Check that objects have valid names and exist before attempting
    ## to save them. The idea and some code taken from base::save
    if (precheck) {
        oknames <- check_stan_names(list)
        if (!all(oknames)) {
            stop(sprintf("Invalid names: %s",
                         paste(sQuote(list[!oknames]), collapse = ", ")))
        }
        ok <- unlist(lapply(list, exists, envir = envir, mode="numeric"))
        if (!all(ok)) {
            n <- sum(!ok)
            stop(sprintf("object(s) %s not found or not numeric",
                         paste(sQuote(list[!ok]), collapse = ", ")))
        }
    }

    for (v in list) {
        ## will throw error if not found
        vv <- get(v, envir, mode="numeric")
        ## Check for bad values
        if (any(is.na(vv)) || any(is.nan(vv))) {
            stop(sprintf("%s contains NA or NaN", v))
        }
        if (is.integer(vv)) {
            ## Stan treats scientific notation as real
            charv <- format(as.vector(vv), scientific=FALSE)
        } else { # double
            ## Stan cannot handle Inf
            charv <- trunc_inf(as.vector(vv))
        }
        charv <- str_c(charv, collapse=", ")
        cat(v, "<-\n", file=file)
        ## There are three cases
        ## 1. vector: length 1
        ## 2. vector: length > 1
        ## 3. array / matrix
        if (is.null(dim(vv))) {
            if (length(vv) == 1) {
                cat0(charv, "\n", file=file)
            } else {
                cat0(str_wrap(sprintf("c(%s)", charv), width=width), "\n",
                     file=file)
            }
        } else {
            cat0(str_wrap(sprintf("structure(c(%s), .Dim=c(%s))",
                                 charv, str_c(dim(vv), collapse=", ")),
                          width=width), "\n",
                 file=file)
        }
    }
}

##' @export
setGeneric("write.stan", function(list, ...) { standardGeneric("write.stan")})
##' @export
setMethod("write.stan", "character", .write.stan)
##' @export
setMethod("write.stan", "list",
          function(list, ...) {
              write.stan(names(list), envir=as.environment(list), ...)
          })

##' Make valid Stan variable names
##'
##' @param x \code{character} vector
##' @return \code{character} vector.
##' @export
make_stan_names <- function(x) {
    ## Ensure all variables only include "A-Za-z0-9_"
    y <- gsub("\\.", "_", make.names(x, allow_=TRUE))
    ## Remove leading non-letters
    y <- gsub("^[^A-Za-z]+", "", y)
    ## Conflicts with Keywords
    badname <- y %in% union(CPP_RESERVED, STAN_RESERVED)
    y[badname] <- paste0(y[badname], "_")
    ## Remove any trailing __
    y <- gsub("__+$", "_", y)
    ## Ensure uniqueness
    y <- make.unique(y, sep="_")
    y
}

