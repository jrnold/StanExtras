expand.paste <- function (..., sep = "") {
    apply(expand.grid(...), 1, paste, collapse = sep)
}
