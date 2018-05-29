makeInput <- function (x, ...)
	UseMethod('makeInput', x)

#' @export
makeInput.logical <- function(x)
	makeRadioButtons(x)

#' @export
makeInput.integer <- function(x)
	makeRadioButtons(x)

#' @export
makeInput.character <- function(x)
	makeRadioButtons(x)

#' @export
makeInput.factor <- function(x)
	makeRadioButtons(x)

#' @export
makeInput.double <- function(x) {
	.min <- round(min(x, na.rm=TRUE), 2)
	.max <- round(max(x, na.rm=TRUE), 2)
	bquote(sliderInput(inputId = .(getArgName(x)),
					   label = .(getArgName(x)),
					   min = .(.min),
					   max = .(.max),
					   value = .(mean(x, na.rm=TRUE)),
					   step = .(round((.max-.min)/200, 2)),
					   ticks=FALSE))
}

#' @export
makeInput.data.frame <- function(x)
    bquote(fileInput(inputId = .(getArgName(x)),
                     label = .(getArgName(x)),
                     multiple = FALSE,
                     accept = c('text/csv',
                                'text/comma-separated-values,text/plain',
                                '.csv')))
#' @export
makeInput.file  <- function(x)
    bquote(fileInput(inputId = .(getArgName(x)),
                     label = .(getArgName(x)),
                     multiple = FALSE))

#' @export
makeInput.list <- function(x)
	`if`(is.data.frame(x), makeInput.data.frame(x),
		 lapply(withArgNames(x),
		 	   makeInput))

# Helpers

makeRadioButtons <- function(x)
	bquote(.(`if`(length(x)<=5,
				  quote(radioButtons),
				  quote(selectInput)))(inputId = .(getArgName(x)),
				  					 label = .(getArgName(x)),
				  					 choices = .(x),
				  					 selected = .(x[1])))

