# Functions from rstan
data_list2array <- function(x) {
  # Turn a list of array to an array whose first dimension is the list
  # and other dimensions being the dimensions of the array element.
  # So this would allow data in Stan coded as `vector[J] y[I]` 
  # to read data in form a list that has I elements of vector of length J, say 
  # 
  # # I <- 4; J <- 5
  # # y <- lapply(1:I, function(i) rnorm(J))
  # 
  # Args:
  #   x: A list of numeric array with the same dimensions 
  # Returns:
  #   An array with the first dimension indexes the list;
  #   other dimensions being the dimensions of the list element (an array)
  # 
  len <- length(x) 
  if (len == 0L)  return(NULL)

  dimx1 <- dim(x[[1]])

  if (any(sapply(x, function(xi) !is.numeric(xi))))  
    stop("all elements of the list should be numeric")
  if (is.null(dimx1)) dimx1 <- length(x[[1]]) 
  lendimx1 <- length(dimx1)

  if (len > 1) { 
    d <- sapply(x[-1], 
                function(xi) {
                  dimxi <- dim(xi)
                  if (is.null(dimxi)) dimxi <- length(xi)
                  identical(dimxi, dimx1) 
                })
    if (!all(d)) stop("the dimensions for all elements (array) of the list are not same")
  }

  # TODO(?): check if x is numeric or array. 
  x <- do.call(c, x)
  dim(x) <- c(dimx1, len)
  aperm(x, c(lendimx1 + 1L, seq_len(lendimx1)))
} 

stan_kw1 <- c('for', 'in', 'while', 'repeat', 'until', 'if', 'then', 'else',
              'true', 'false') 
stan_kw2 <- c('int', 'real', 'vector', 'simplex', 'ordered', 'positive_ordered', 
              'row_vector', 'matrix', 'corr_matrix', 'cov_matrix', 'lower', 'upper') 
stan_kw3 <- c('model', 'data', 'parameters', 'quantities', 'transformed', 'generated') 

cpp_kw <- c("alignas", "alignof", "and", "and_eq", "asm", "auto", "bitand", "bitor", "bool", 
            "break", "case", "catch", "char", "char16_t", "char32_t", "class", "compl",
            "const", "constexpr", "const_cast", "continue", "decltype", "default", "delete",
            "do", "double", "dynamic_cast", "else", "enum", "explicit", "export", "extern",
            "false", "float", "for", "friend", "goto", "if", "inline", "int", "long", "mutable",
            "namespace", "new", "noexcept", "not", "not_eq", "nullptr", "operator", "or", "or_eq",
            "private", "protected", "public", "register", "reinterpret_cast", "return",
            "short", "signed", "sizeof", "static", "static_assert", "static_cast", "struct",
            "switch", "template", "this", "thread_local", "throw", "true", "try", "typedef",
            "typeid", "typename", "union", "unsigned", "using", "virtual", "void", "volatile",
            "wchar_t", "while", "xor", "xor_eq")


is_legal_stan_vname <- function(name) {
  # Return:
  #   FALSE: not a lega variable name in Stan 
  #   TRUE: maybe it is valid, but 100% sure 
  if (grepl('\\.',  name)) return(FALSE) 
  if (grepl('^\\d', name)) return(FALSE)
  if (grepl('__$',  name)) return(FALSE)
  if (name %in% stan_kw1) return(FALSE)
  if (name %in% stan_kw2) return(FALSE)
  if (name %in% stan_kw3) return(FALSE)
  !name %in% cpp_kw
} 

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
