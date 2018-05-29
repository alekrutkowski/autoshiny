library(shiny)
fluidPage(
    titlePanel("function(x = 1:3, y = 5:9) x + y"),
    sidebarLayout(
        sidebarPanel(list(h4("Inputs"),
                          list(radioButtons(inputId = "x", label = "x",
                                            choices = 1:3, selected = 1),
                               radioButtons(inputId = "y", label = "y",
                                            choices = 5:9, selected = 5)))),
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
