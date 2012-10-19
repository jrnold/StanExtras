parse_stan_header_lines <- function(lines) {
    header_classes <-
        c(stan_version_major = "integer",
          stan_version_minor = "integer",
          stan_version_patch = "integer",
          data = "character",
          init = "character",
          append_samples = "integer",
          save_warmup = "integer",
          # the seed can be larger than .Machine$integer.max
          seed = "character", 
          chain_id = "integer",
          iter = "integer",
          warmup = "integer",
          thin = "integer",
          equal_step_sizes = "integer",
          leapfrog_steps = "integer",
          max_treedepth = "integer",
          epsilon = "integer",
          epsilon_pm = "integer",
          delta = "numeric",
          gamma = "numeric",
          step_size = "numeric")
    header_lines <- lines[str_detect(lines, "^# ")]
    header_stuff <- na.omit(str_match(header_lines, "# +(.*?)=(\\S+)"))
    chains <- as.list(header_stuff[ , 3])
    names(chains) <- gsub(" ", "_", header_stuff[ , 2])
    for (i in names(header_classes)) {
        chains[[i]] <- as(chains[[i]], header_classes[i])
    }
    chains <- data.frame(chains)

    ## Saved parameters can be longer than step size multipliers because
    ## they include generated quantities.
    parln <- lines[which(str_detect(lines, "^lp__,"))]
    parameters <- str_split(parln, ",")[[1]]
    parameters <- parameters[4:length(parameters)]
    npar <- length(parameters)

    parameter_mass_pat <- "parameter step size multipliers:"
    parameter_mass_line <-
        lines[which(str_detect(lines, parameter_mass_pat)) + 1]
    step_size_multipliers <-
        as.numeric(str_split(str_sub(parameter_mass_line, 3), ",")[[1]])
    nmass <- length(step_size_multipliers)
    step_size_multipliers <- c(step_size_multipliers, rep(NA, npar - nmass))

    par_chains <- data.frame(chain_id = chains$chain_id,
                             parname = parameters,
                             step_size_multipliers=step_size_multipliers)
    list(chains=chains, par_chains=par_chains)
}

parse_stan_header_file <- function(file) {
    parse_stan_header_lines(readLines(file, 30))
}

read_stan_csv_one <- function(file, chain_id=NULL, metadata=list()) {
    header <- parse_stan_header_file(file)
    npar <- nrow(header$par_chains)
    if (is.null(chain_id)) {
        chain_id <- as.integer(header$chains$chain_id)
    } else {
        header$chains$chain_id <- as.integer(chain_id)
        header$par_chains$chain_id <- as.integer(chain_id)        
    }
    chains <- within(header$chains, {
         start <- as.integer(ifelse(warmup, warmup + 1, 1))
         end <- as.integer(iter)
         thin <- as.integer(thin)
         iter <- NULL
    })
    ## use prior knowledge to speed up reading files
    ## lpp, stepsize, treedepth, ...
    ## npar <- ncol(read.csv(file, comment.char="#", nrows=1)) - 3
    colClasses <- c("numeric", "integer", "numeric",
                    rep("numeric", npar))
    x <- read.csv(file, header=TRUE, comment.char="#", colClasses=colClasses)

    ## Treedepth, stepsize not considered parameters
    treedepth <- x$treedepth
    stepsize <- x$stepsize
    x <- x[ , setdiff(names(x), c("treedepth__", "stepsize__"))]

    ## Parse parameters
    parnames <- mcmc_parse_parname_stan(colnames(x))
    
    ## Rejected rows
    ## Make sure that treedepth, stepsize already removed,
    ## and iteration not yet added
    ## This uses the same method as coda, calculates it for all
    ## parameters since HMC samples the entire vector of parameters.
    niter <- nrow(x)
    rejected <- c(FALSE, apply(x[2:niter, ] == x[1:(niter-1), ], 1, all))

    x$iter <- seq_len(nrow(x))
    x <- melt(x, id.vars="iter",
              variable.name="parname", value.name="val")
    x$chain_id <- chain_id
    chains$niter <- niter
    chain_iters <- data.frame(chain_id = chain_id,
                              iter = seq_len(niter),
                              treedepth = treedepth,
                              stepsize = stepsize,
                              rejected = rejected)
    McmcLong(x,
             parnames=parnames,
             chains=chains,
             par_chains=header$par_chains,
             chain_iters=chain_iters,
             metadata=metadata)
}

##' Read STAN output
##'
##' Read csv files produced by Stan.
##'
##' This returns both the data in the csv, as well as all the metadata
##' in the header of the file.
##'
##' @param file \code{character} names of output file produced by a STAN model.
##' @param chain_id \code{integer} Values of \code{chain_id} to use
##' for each chain. These are used instead of the values in the header
##' of the csv are ignored.
##' @return \code{\link[mcmc4]{McmcLong}} object.
##' @export
read_stan_csv <- function(file, chain_id=seq_along(files)) {
    do.call(c, mapply(function(x, i) read_stan_csv_one(x, i),
                      files, chain_id, SIMPLIFY=FALSE, USE.NAMES=FALSE))

}

