library(shiny)
function(input, output) {
  "A function with lists everywhere" <- function(
      `First argument group,` = list(1:3, c("a", "b", "c")),
      `2nd arg group,` = list(11:14, c("A", "B", "C", "D", "E")))
    list(
        `Some text` = as.character(`First argument group,`$`number two`,
                                   `2nd arg group,`$`second arg.`),
        `Some numbers` = `First argument group,`$`number one` + `2nd arg group,`$`1st argument`,
        `Even a ggplot2 chart` = ggplot2::qplot(
            a, b, data = data.frame(a = 1:20, b = log(1:20))))
  ..coerceInputs <- function(input)
    list(list({
                InputVal <- input[["First argument group, number one"]]
                if (class(InputVal) == "integer" && !..isFileInput(InputVal))
                  InputVal
                else if (is.null(InputVal))
                  1
                else if (..isFileInput(InputVal))
                  InputVal
                else
                  .Primitive("as.integer")(InputVal)
              }, {
                   InputVal <- input[["First argument group, number two"]]
                   if (class(InputVal) == "character" &&
                       !..isFileInput(InputVal))
                     InputVal
                   else if (is.null(InputVal))
                     "a"
                   else if (..isFileInput(InputVal))
                     InputVal
                   else
                     .Primitive("as.character")(InputVal)
                 }),
         list({
                InputVal <- input[["2nd arg group, 1st argument"]]
                if (class(InputVal) == "integer" && !..isFileInput(InputVal))
                  InputVal
                else if (is.null(InputVal))
                  11
                else if (..isFileInput(InputVal))
                  InputVal
                else
                  .Primitive("as.integer")(InputVal)
              }, {
                   InputVal <- input[["2nd arg group, second arg."]]
                   if (class(InputVal) == "character" &&
                       !..isFileInput(InputVal))
                     InputVal
                   else if (is.null(InputVal))
                     "A"
                   else if (..isFileInput(InputVal))
                     InputVal
                   else
                     .Primitive("as.character")(InputVal)
                 }))
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
  ..value <- function() do.call(`A function with lists everywhere`,
                                ..coerceInputs(reactiveValuesToList(input)))
  list(
      list(output[["Output Some text"]] <- renderText(
               paste(..value()[["Some text"]], collapse = "\n")),
           list(output[["Download Output Some numbers"]] <- downloadHandler(
                    filename = "Table Output Some numbers.csv",
                    content = function(file)
                      write.csv(
                          structure(as.data.frame(..value()[["Some numbers"]]),
                                    .Names = "Some numbers"), file),
                    contentType = "text/csv"),
                output[["Table Output Some numbers"]] <- renderDataTable(
                    {
                      .dframe <- structure(
                          as.data.frame(..value()[["Some numbers"]]),
                          .Names = "Some numbers")
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
                    }, escape = -1)),
           list(output[[
                    "Download Output Even a ggplot2 chart"]] <- downloadHandler(
                    filename = "Plot Output Even a ggplot2 chart.png",
                    content = function(file) {
                      png(file)
                      v <- ..value()[["Even a ggplot2 chart"]]
                      if ("ggplot" %in% class(v)) print(v)
                      dev.off()
                    }, contentType = "image/png"),
                output[["Plot Output Even a ggplot2 chart"]] <- renderPlot(
                    ..value()[["Even a ggplot2 chart"]]))))
}
