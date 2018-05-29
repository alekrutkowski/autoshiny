library(shiny)
fluidPage(
    titlePanel("Excel file in and out"),
    sidebarLayout(
        sidebarPanel(
            list(h4("Inputs"), list(fileInput(inputId = "Input Excel file",
                                              label = "Input Excel file",
                                              multiple = FALSE)))),
        mainPanel(
            NULL,
            conditionalPanel(
                condition = "input['..go'] != 0 && !($('html').hasClass('shiny-busy'))",
                list(list(br(), h4("Output"),
                          downloadLink(outputId = "Download Output",
                                       label = "Download the file")))),
            conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                tags$div(br(), "Please wait... ",
                         tags$img(src = "https://media.giphy.com/media/3o7TKtnuHOHHUjR38Y/giphy.gif",
                                  alt = "loader",
                                  style = "width: 4em; height: 4em;"))))))
