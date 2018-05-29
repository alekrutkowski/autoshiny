library(shiny)
function(input, output) {
  "Histogram for normal distribution" <- function(
      `Number of observations` = c(100, 10, 1000))
    plot(hist(rnorm(`Number of observations`)))
  ..coerceInputs <- function(input)
    list({
           InputVal <- input[["Number of observations"]]
           if (class(InputVal) == "integer" && !..isFileInput(InputVal))
             InputVal
           else if (is.null(InputVal))
             100
           else if (..isFileInput(InputVal))
             InputVal
           else
             .Primitive("as.integer")(InputVal)
         })
  ..isFileInput <- function(x)
    is.data.frame(x) &&
    all(c("name", "size", "type", "datapath") %in% colnames(x))
  File <- function(x) {
    if (!is.character(x) || length(x) != 1)
      stop("Function `File` expects a one-element character vector\n",
           "which is a path to a single file, but it received\n",
           "an object of class ", bq(class(x)), " with length=", length(x), ".",
           call. = FALSE)
    if (!file.exists(x)) stop("File \"", x, "\" does not exist.")
    structure(x, class = "file")
  }
  ..value <- function() do.call(`Histogram for normal distribution`,
                                ..coerceInputs(reactiveValuesToList(input)))
  list(list(output[["Download Output"]] <- downloadHandler(
                filename = "Plot Output.png", content = function(file) {
                                                png(file)
                                                v <- ..value()
                                                if ("ggplot" %in% class(v))
                                                  print(v)
                                                dev.off()
                                              }, contentType = "image/png"),
            output[["Plot Output"]] <- renderPlot(..value())))
}
