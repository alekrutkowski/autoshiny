getArgName <- function(x)
    attr(x, 'ArgName')

#' An obligatory wrapper for file names (paths)
#' 
#' This function \strong{must} be used
#' \itemize{
#' \item in the \strong{arguments} of function \code{fun}
#' (passed to \code{\link{makeApp}} or \code{\link{makeFiles}}) and/or
#' \item in the \strong{value returned } by \code{fun}
#' }
#' to wrap the character string
#' indicating a path respectively
#' \itemize{
#' \item to an input file ("consumed" by \code{fun} or
#' \item to an output file ("produced" by \code{fun} as a
#' \href{https://en.wikipedia.org/wiki/Side_effect_(computer_science)}{side effect}).
#' }
#' Otherwise \pkg{autoshiny} cannot distinguish file paths from character strings.
#' 
#' @param x A string, i.e. character vector of length 1, indicating a file path to
#' an existing file.
#' @return \code{x} with an S3 class attribute "\code{file}".
#' @export
File <- function(x) {
    if (!is.character(x) || length(x)!=1)
        stop('Function `File` expects a one-element character vector\n',
             'which is a path to a single file, but it received\n',
             'an object of class ',bq(class(x)),' with length=',length(x),'.',
             call.=FALSE)
    if (!file.exists(x))
        stop('File "',x,'" does not exist.')
    structure(x, class="file")
}


.NULL <-  # "NULL" with class
    structure(function() {},
              class="NULL")

setArgName <- function(x, y) {
    x <- if (is.null(x)) .NULL else x
    attr(x, 'ArgName') <- y
    x
}

withArgNames <- function(List) {
    List.ArgName <- getArgName(List)
    if (hasBrokenNames(List))
        stop('All list elements have to be uniquelly named.\n',
             'The list that caused the error was:\n',
             paste(simpleDeparse(List), collapse='\n'),
             call.=FALSE)
    List.names <- names(List)
    mapply(setArgName,
           x = List,
           y = `if`(is.null(List.ArgName),
                    List.names,
                    paste(List.ArgName, List.names)),
           SIMPLIFY = FALSE)
}

preservingArgName <- function(x, f)
    setArgName(f(x), getArgName(x))

outputArgNameToName <- function(OutputArgName)
    sub('^Output ',"",OutputArgName)

simpleDeparse <- function(expr)
    deparse(expr,
            backtick=TRUE,
            control=c())

hasBrokenNames <- function(vec) {
    Names <- names(vec)
    is.null(Names) %|%
        any(Names=="") |
        (length(Names)!=length(unique(Names)))
}

`%|%` <- function(x, y) # lazy logical "or"
    if (x) x else y

plotId <- function(x)
    paste('Plot',x)

downloadId <- function(x)
    paste('Download',x)

tableId <- function(x)
    paste('Table',x)
