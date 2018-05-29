library(shiny)
function(input, output) {
  "Table of sin and cos values" <- function(
      `Upload CSV file with column "x"` = list(
          c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75, 3, 3.25,
            3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75, 6, 6.25))) {
    dta <- `Upload CSV file with column "x"`
    data.frame(X = dta$x, `Sin of X` = sin(dta$x), `Cos of X` = cos(dta$x),
               check.names = FALSE)
  }
  ..coerceInputs <- function(input)
    list({
           InputVal <- input[["Upload CSV file with column \"x\""]]
           if (class(InputVal) == "data.frame" && !..isFileInput(InputVal))
             InputVal
           else if (is.null(InputVal))
             list(c(0, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2, 2.25, 2.5, 2.75,
                    3, 3.25, 3.5, 3.75, 4, 4.25, 4.5, 4.75, 5, 5.25, 5.5, 5.75,
                    6, 6.25))
           else if (..isFileInput(InputVal))
             read.csv(InputVal$datapath, check.names = FALSE)
           else
             (function(x, row.names = NULL, optional = FALSE, ...) {
                if (is.null(x)) return(as.data.frame(list()))
                UseMethod("as.data.frame")
              })(InputVal)
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
  ..value <- function() do.call(`Table of sin and cos values`,
                                ..coerceInputs(reactiveValuesToList(input)))
  list(list(output[["Download Output"]] <- downloadHandler(
                filename = "Table Output.csv", content = function(file)
                                                 write.csv(..value(), file),
                contentType = "text/csv"),
            output[["Table Output"]] <- renderDataTable(
                {
                  .dframe <- ..value()
                  cbind(data.frame(
                            `<sub>Row</sub>` = {
                              .rn <- row.names(.dframe)
                              paste0("<sub>",
                                     as.character(if (is.null(.rn))
                                                    seq_len(nrow(.dframe))
                                                  else
                                                    .rn), "</sub>")
                            }, check.names = FALSE),
                        if (is.matrix(.dframe))
                          as.data.frame(.dframe, check.names = FALSE)
                        else
                          .dframe)
                }, escape = -1)))
}
