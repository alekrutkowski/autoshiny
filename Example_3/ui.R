library(shiny)
fluidPage(
    titlePanel("Table of sin and cos values"),
    sidebarLayout(
        sidebarPanel(
            bquote(list(h4("Inputs"),
                        list(fileInput(
                                 inputId = "Upload CSV file with column \"x\"",
                                 label = "Upload CSV file with column \"x\"",
                                 multiple = FALSE,
                                 accept = c(
                                     "text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv")))))),
        mainPanel(
            NULL,
            conditionalPanel(
                condition = "input['..go'] != 0 && !($('html').hasClass('shiny-busy'))",
                list(
                    list(br(), h4("Output"),
                         list(downloadLink(
                                  outputId = "Download Output",
                                  label = "Download the table displayed below"),
                              dataTableOutput(outputId = "Table Output"))))),
            conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                tags$div(br(), "Please wait... ",
                         tags$img(src = "https://media.giphy.com/media/3o7TKtnuHOHHUjR38Y/giphy.gif",
                                  alt = "loader",
                                  style = "width: 4em; height: 4em;"))))))
