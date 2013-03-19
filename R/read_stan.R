#' @include package.R
#' @export read_stan_csv
NULL

# For Stan <= 1.2.0 
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
      nondiag_mass = "integer",
      equal_step_sizes = "integer",
      leapfrog_steps = "integer",
      max_treedepth = "integer",
      epsilon = "integer",
      epsilon_pm = "integer",
      delta = "numeric",
      gamma = "numeric",
      step_size = "numeric")
  
  comments <- lines[str_detect(lines, "^# ")]

  header <- list()
  ## select lines where key=value
  eq_lines <- na.omit(str_match(header_lines, "# +(.*?)=(\\S+)"))
  for (i in 1:nrow(eq_lines)) {
    keyname <- gsub(" ", "_", eq_lines[i, 2])
    value <- as(eq_lines[i, 3], header_classes[keyname])
    header[[keyname]] <- value
  }
  
  ## Saved parameters can be longer than step size multipliers because
  ## they include generated quantities.
  parln <- lines[which(str_detect(lines, "^lp__,"))]
  header[["colnames"]] <- str_split(parln, ",")[[1]]

  ## Sample or Optimization
  if (!is.na(str_match(lines[1], "Samples"))) {
    header[["point_estimate"]] <- FALSE
  } else if (!is.na(str_match(lines[1], "Point Estimate"))) {
    header[["point_estimate"]] <- TRUE
  } else {
    print(sprintf("First line noes not appear to be sampling or point estimate:\n%s",
                  lines[1]))
    header[["point_estimate"]] <- NA
  }

  ## Adaption method
  adaptation <- na.omit(str_match(comments,
                                  "\\((.*?)\\) adaptation finished"))[ , 2]
  if (length(adaptation)) {
    header[["adaptation_type"]] <- adaptation
    if (adaptation == "mcmc::nuts_nondiag") {
      cov_line <- which(str_detect(lines, "estimated covariance matrix:"))
      cov_sz <- str_count(lines[cov_line + 1], ",")
      cov_matrix <- matrix(NA_real_, cov_sz, cov_sz)
      for (i in seq_len(cov_sz)) {
        row <- str_sub(lines[cov_line + i], 2L, -2L)
        cov_matrix[i, ] <- as.numeric(str_split(row, ",")[[1]])
      }
      header[["covariance_matrix"]] <- cov_matrix
    } else if (adaptation == "mcmc::nuts_diag") {
      parameter_mass_pat <- "parameter step size multipliers:"
      parameter_mass_line <-
        lines[which(str_detect(lines, parameter_mass_pat)) + 1]
      step_size_multipliers <-
        as.numeric(str_split(str_sub(parameter_mass_line, 3), ",")[[1]])
      header[["step_size_multipliers"]] <- step_size_multipliers
    }
  } else {
    header[["adaptation_type"]] <- NA_character_
  }

  header
}

parse_stan_header_file <- function(file) {
  parse_stan_header_lines(readLines(file, 30))
}

read_stan_csv_one <- function(file, chain_id=NULL, metadata=list()) {
  header <- parse_stan_header_file(file)
  ncolumns <- length(header[["colnames"]])
  pointest <- header[["point_estimate"]]
  if (!pointest) {
    colClasses <- c("numeric", "integer", "numeric", rep("numeric", ncolumns - 3))
  } else {
    colClasses <- rep("numeric", ncolumns)
  }
  x <- read.csv(file, header=TRUE, comment.char="#", colClasses=colClasses)
  niter <- nrow(x)
  if (!pointest) {
    attr(x, "rejected") <- unname(c(FALSE, apply(x[2:niter, ] == x[1:(niter-1), ], 1, all)))
  }
  attr(x, "header") <- header
  x
}

#' @title Read STAN output
#'
#' @description Read csv files produced by Stan. This returns both the sample values and the metadata in the comments
#' of the file.
#'
#' @param file \code{character} name of an output file produced by a STAN model.
#' 
#' @return A \code{data.frame} with attributes
#' \describe{
#' \item{\code{header}}{A \code{list} containing header information}
#' \item{\code{rejected}}{A \code{logical} vector indicating whether the iteration was a rejected sample (for samples only)}
#' }
read_stan_csv <- function(file) {
  read_stan_csv_one(file)
}

