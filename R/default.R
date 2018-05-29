default <- function (x, ...)
    UseMethod('default', x)

first <- function(x)
    x[1]

#' @export
default.logical <- first

#' @export
default.integer <- first

#' @export
default.character <- first

#' @export
default.factor <- first

#' @export
default.double <- function(x)
    mean(x, na.rm=TRUE)

#' @export
default.data.frame <- identity

#' @export
default.file <- identity

#' @export
default.list <- function(x)
    `if`(is.data.frame(x), default.data.frame(x),
         lapply(x, default))

#' @export
default.default <- identity
