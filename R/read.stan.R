setClass("McmcStanChains", contains="data.frame")
## ## By Chain
## stan_version_major = "integer",
## stan_version_minor = "integer",
## stan_version_patch = "integer",
## iter = "integer",
## datasrc = "character",
## init = "character",
## append_samples = "integer",
## save_warmup = "integer",
## seed = "character",  # integer, but can be larger than max R integer value
## chain_id = "integer",
## warmup = "integer",
## equal_step_sizes = "integer",
## leapfrog_steps = "integer",
## max_treedepth = "integer",
## epsilon = "numeric",
## epsilon_pm = "integer",
## delta = "numeric",
## gamma = "numeric",
## step_size = "numeric",

setClass("McmcStanParChains", contains="data.frame")
## ## By iteration
## stepsize = "numeric",
## treedepth = "integer",
## rejected = "logical",

setClass("McmcStanChainIters", contains="data.frame")
## ## By chain-parameter
## step_size_multipliers = "numeric",

##' MCMC samples from Stan (Long shape)
##'
##' @name McmcStan-class
##' @rdname McmcStan-class
##' @docType class
##' @keywords classes
##' @export
setClass("McmcLongStan", contains="McmcLong",
         representation(
             metadata = "McmcParameterMeta",
             chains = "McmcStanChains",
             par_chains = "McmcStanParChains",
             chain_iters = "McmcStanChainIters"))

mcmc_stan_validate <- function(object) {
    TRUE
}

setValidity("McmcLongStan", mcmc_stan_validate)

parse_stan_header_lines <- function(lines) {
    header_lines <- lines[str_detect(lines, "^# ")]
    header_stuff <- na.omit(str_match(header_lines, "# +(.*?)=(\\S+)"))
    header <- as.list(header_stuff[ , 3])
    names(header) <- gsub(" ", "_", header_stuff[ , 2])
    header_int <- c("stan_version_major",
                    "stan_version_minor",
                    "stan_version_patch",
                    "append_samples",
                    "save_warmup",
                    # "seed", can be larger than integer that R supports
                    "chain_id",
                    "iter",
                    "warmup",
                    "thin",
                    "epsilon_pm",
                    "leapfrog_steps",
                    "equal_step_sizes",
                    "max_treedepth")
    header_numeric <- c("epsilon",
                        "delta",
                        "gamma",
                        "step_size")
    for (i in header_int) {
        header[[i]] <- as.integer(header[[i]])
    }
    for (i in header_numeric) {
        header[[i]] <- as.numeric(header[[i]])
    }
    parameter_mass_pat <- "parameter step size multipliers:"
    parameter_mass_line <-
        lines[which(str_detect(lines, parameter_mass_pat)) + 1]
    header$step_size_multipliers <-
        as.numeric(str_split(str_sub(parameter_mass_line, 3), ",")[[1]])
    header
}

parse_stan_header_file <- function(file) {
    parse_stan_header_lines(readLines(file, 30))
}

##' Read STAN output
##'
##' @param file \code{character} name of output file produced by a STAN model.
##' @return \code{McmcStan} object.
##' @export
##'
McmcLongStan <- function(file, chain=1) {
    header <- parse_stan_header_file(file)
    step_size_multipliers <- header$step_size_multipliers
    header$step_size_multipliers <- NULL
        
    ## use prior knowledge to speed up reading files
    ## lpp, stepsize, treedepth, ...
    npar <- ncol(read.csv(file, comment.char="#", nrows=1)) - 3
    colClasses <- c("numeric", "integer", "numeric",
                    rep("numeric", npar))
    x <- read.csv(file, header=TRUE, comment.char="#", colClasses=colClasses)

    ## Treedepth, stepsize not considered parameters
    treedepth <- x$treedepth
    stepsize <- x$stepsize
    x <- x[ , setdiff(names(x), c("treedepth", "stepsize"))]

    ## Parse parameters
    parnames <- colnames(x)
    parsed_par <- parse_parameter_names_stan(parnames)
    metadata <- McmcParameterMeta(parsed_par)
    
    ## Rejected rows
    ## Make sure that treedepth, stepsize already removed,
    ## and iteration not yet added
    n <- nrow(x)
    rejected <- c(FALSE, apply(x[2:n, ] == x[1:(n-1), ], 1, all))

    ## Iterations
    iteration <- seq_len(nrow(x))

    mcmc_stan_par_chains <-
        new("McmcStanParChains",
            data.frame(parname = parnames,
                       chain_num = chain,
                       step_size_multiplier = step_size_multipliers))
    mcmc_stan_chain_iter <-
        new("McmcStanChainIters",
            data.frame(chain_num = chain,
                       iter = iteration,
                       rejected = rejected,
                       treedepth = treedepth,
                       stepsize = stepsize))
    mcmc_stan_chain <-
        new("McmcStanChains",
            transform(as.data.frame(header), chain=chain))

    ## Only part that differs for long/list
    x <- transform(x,
                   chain = chain,
                   iteration = iteration)
    x <- melt(x, id.vars=c("chain", "iteration"),
              variable.name="parameter",
              value.name="value")
    new("McmcLongStan",
        McmcLong(x),
        metadata = metadata,
        chains = mcmc_stan_chain,
        par_chains = mcmc_stan_par_chains,
        chain_iters = mcmc_stan_chain_iter)
}

c_mcmc_long_stan <- function(x) {
    
}
