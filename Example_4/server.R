library(shiny)
function(input, output) {
  "Excel file in and out" <- function(
      `Input Excel file` = "my_test_file.xlsx") {
    my.data <- openxlsx::read.xlsx(`Input Excel file`)
    my.data2 <- within(my.data, z <- x + y)
    openxlsx::write.xlsx(my.data2, "my_test_file_2.xlsx")
    File("my_test_file_2.xlsx")
  }
  ..coerceInputs <- function(input)
    list({
           InputVal <- input[["Input Excel file"]]
           if (class(InputVal) == "character" && !..isFileInput(InputVal))
             InputVal
           else if (is.null(InputVal))
             "my_test_file.xlsx"
           else if (..isFileInput(InputVal))
             InputVal
           else
             .Primitive("as.character")(InputVal)
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
  ..value <- function() do.call(`Excel file in and out`,
                                ..coerceInputs(reactiveValuesToList(input)))
  list(output[["Download Output"]] <- downloadHandler(
           filename = function() ..value(), content = function(file)
                                              file.copy(..value(), file)))
}
