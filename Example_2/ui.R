library(shiny)
fluidPage(
    titlePanel("Histogram for normal distribution"),
    sidebarLayout(
        sidebarPanel(bquote(list(h4("Inputs"),
                                 list(radioButtons(
                                          inputId = "Number of observations",
                                          label = "Number of observations",
                                          choices = c(100, 10, 1000),
                                          selected = 100))))),
        mainPanel(
            NULL,
            conditionalPanel(
                condition = "input['..go'] != 0 && !($('html').hasClass('shiny-busy'))",
                list(list(br(), h4("Output"),
                          list(downloadLink(
                                   outputId = "Download Output",
                                   label = "Download the plot displayed below"),
                               plotOutput(outputId = "Plot Output"))))),
            conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                tags$div(br(), "Please wait... ",
                         tags$img(src = "https://media.giphy.com/media/3o7TKtnuHOHHUjR38Y/giphy.gif",
                                  alt = "loader",
                                  style = "width: 4em; height: 4em;"))))))
