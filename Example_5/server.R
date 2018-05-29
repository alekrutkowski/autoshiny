library(shiny)
function(input, output) {
  "Get \"GDP and main components\" from Eurostat" <- function() {
    x <- eurodata::importData("nama_10_gdp")
    head(x, 10)
  }

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
  ..value <- eventReactive(
      input[["..go"]],
      do.call(`Get "GDP and main components" from Eurostat`, list()))
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
