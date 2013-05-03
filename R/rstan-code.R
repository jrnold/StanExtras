# Functions from rstan
stan_rdump <- function(list, file = "", append = FALSE, 
                       envir = parent.frame(),
                       width = options("width")$width, quiet = FALSE) {
  if (is.character(file)) {
    ex <- sapply(list, exists, envir = envir)
    if (!all(ex)) {
      notfound_list <- list[!ex] 
      if (!quiet) 
        warning(paste("objects not found: ", paste(notfound_list, collapse = ', '), sep = '')) 
    } 
    list <- list[ex] 
    if (!any(ex)) 
      return(invisible(character()))

    if (nzchar(file)) {
      file <- file(file, ifelse(append, "a", "w"))
      on.exit(close(file), add = TRUE)
    } else {
      file <- stdout()
    }
  }

  for (x in list) { 
    if (!is_legal_stan_vname(x) & !quiet)
      warning(paste("variable name ", x, " is not allowed in Stan", sep = ''))
  } 

  l2 <- NULL
  addnlpat <- paste0("(.{1,", width, "})(\\s|$)")
  for (v in list) {
    vv <- get(v, envir) 

    if (is.data.frame(vv)) {
      vv <- data.matrix(vv) 
    } else if (is.list(vv)) {
      vv <- data_list2array(vv)
    } else if (is.logical(vv)) {
      mode(vv) <- "integer"
    } else if (is.factor(vv)) {
      vv <- as.integer(vv)
    } 
    
    if (!is.numeric(vv))  {
      if (!quiet) 
        warning(paste0("variable ", v, " is not supported for dumping."))
      next
    } 

    if (is.vector(vv)) {
      if (length(vv) == 1) {
        cat(v, " <- ", vv, "\n", file = file, sep = '')
        next
      }
      str <- paste0(v, " <- \nc(", paste(vv, collapse = ', '), ")") 
      str <-  gsub(addnlpat, '\\1\n', str)
      cat(str, file = file) 
      l2 <- c(l2, v) 
      next
    }    

    if (is.matrix(vv) || is.array(vv)) { 
      l2 <- c(l2, v) 
      vvdim <- dim(vv)
      cat(v, " <- \n", file = file, sep = '')
      str <- paste0("structure(c(", paste(as.vector(vv), collapse = ', '), "),") 
      str <- gsub(addnlpat, '\\1\n', str)
      cat(str, 
          ".Dim = c(", paste(vvdim, collapse = ', '), "))\n", file = file, sep = '')
      next
    }
  }
  invisible(l2) 
} 
