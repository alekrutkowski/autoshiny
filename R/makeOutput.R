makeOutput <- function (x, ...)
	UseMethod('makeOutput', x)

#' @export
makeOutput.character <- function(x)
	bquote(verbatimTextOutput(outputId = .(getArgName(x))))

#' @export
makeOutput.integer <- function(x)
	makeOutput.data.frame(x)

#' @export
makeOutput.double <- function(x)
	makeOutput.data.frame(x)

#' @export
makeOutput.factor <- function(x)
	makeOutput.data.frame(x)

#' @export
makeOutput.logical <- function(x)
	makeOutput.data.frame(x)

#' @export
makeOutput.matrix <- function(x)
	makeOutput.data.frame(x)

#' @export
makeOutput.data.frame <- function(x)
	bquote(list(downloadLink(outputId = .(downloadId(getArgName(x))),
							 label = 'Download the table displayed below'),
				dataTableOutput(outputId = .(tableId(getArgName(x))))))

#' @export
makeOutput.file <- function(x)
    bquote(downloadLink(outputId = .(downloadId(getArgName(x))),
                             label = 'Download the file'))

#' @export
makeOutput.list <- function(x)
	`if`(is.data.frame(x), makeOutput.data.frame(x),
		 lapply(withArgNames(x),
		 	   function(y)
		 	   	bquote(list(br(),
		 	   	            h4(.(outputArgNameToName(getArgName(y)))),
		 	   				.(makeOutput(y))))))

#' @export
makeOutput.NULL <- function(x) # for plots generated with generic `plot`
	bquote(list(downloadLink(outputId = .(downloadId(getArgName(x))),
							 label = 'Download the plot displayed below'),
				plotOutput(outputId = .(plotId(getArgName(x))))))

#' @export
makeOutput.ggplot <- function(x) # for plots generated with `ggplot2` package
	makeOutput.NULL(x)
