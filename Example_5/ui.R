library(shiny)
fluidPage(
    titlePanel("Get \"GDP and main components\" from Eurostat"),
    sidebarLayout(
        NULL,
        mainPanel(
            actionButton("..go", "Go, (re)calculate!"),
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
