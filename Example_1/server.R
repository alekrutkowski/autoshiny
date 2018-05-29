library(shiny)
function(input, output) {
  "function(x = 1:3, y = 5:9) x + y" <- function(x = 1:3, y = 5:9) x + y
  ..coerceInputs <- function(input)
    list({
           InputVal <- input[["x"]]
           if (class(InputVal) == "integer" && !..isFileInput(InputVal))
             InputVal
           else if (is.null(InputVal))
             1
           else if (..isFileInput(InputVal))
             InputVal
           else
             .Primitive("as.integer")(InputVal)
         }, {
              InputVal <- input[["y"]]
              if (class(InputVal) == "integer" && !..isFileInput(InputVal))
                InputVal
              else if (is.null(InputVal))
                5
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
  ..value <- function() do.call(`function(x = 1:3, y = 5:9) x + y`,
                                ..coerceInputs(reactiveValuesToList(input)))
  list(list(output[["Download Output"]] <- downloadHandler(
                filename = "Table Output.csv",
                content = function(
                    file) write.csv(structure(as.data.frame(..value()),
                                              .Names = "Output"), file),
                contentType = "text/csv"),
            output[["Table Output"]] <- renderDataTable(
                {
                  .dframe <- structure(
                      as.data.frame(..value()), .Names = "Output")
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
