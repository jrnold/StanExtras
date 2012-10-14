expand.paste <- function (..., sep = "") {
    apply(expand.grid(...), 1, paste, collapse = sep)
}

## This is needed to get rbind2 to work correctly for
## the classes inheriting from data.frame
setMethod("rbind2", c(x="data.frame", y="data.frame"),
          function(x, y, ...) {
              base::rbind.data.frame(x, y)
          })

