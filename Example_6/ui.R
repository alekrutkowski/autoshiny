library(shiny)
fluidPage(
    titlePanel("A function with lists everywhere"),
    sidebarLayout(
        sidebarPanel(
            bquote(
                list(h4("Inputs"),
                     list(list(radioButtons(
                                   inputId = "First argument group, number one",
                                   label = "First argument group, number one",
                                   choices = 1:3, selected = 1),
                               radioButtons(
                                   inputId = "First argument group, number two",
                                   label = "First argument group, number two",
                                   choices = c("a", "b", "c"), selected = "a")),
                          list(radioButtons(
                                   inputId = "2nd arg group, 1st argument",
                                   label = "2nd arg group, 1st argument",
                                   choices = 11:14, selected = 11),
                               radioButtons(
                                   inputId = "2nd arg group, second arg.",
                                   label = "2nd arg group, second arg.",
                                   choices = c("A", "B", "C", "D", "E"),
                                   selected = "A")))))),
        mainPanel(
            NULL,
            conditionalPanel(
                condition = "input['..go'] != 0 && !($('html').hasClass('shiny-busy'))",
                list(
                    list(
                        br(), h4("Output"),
                        list(
                            list(br(), h4("Some text"),
                                 verbatimTextOutput(
                                     outputId = "Output Some text")),
                            list(
                                br(), h4("Some numbers"),
                                list(
                                    downloadLink(
                                        outputId = "Download Output Some numbers",
                                        label = "Download the table displayed below"),
                                    dataTableOutput(
                                        outputId = "Table Output Some numbers"))),
                            list(
                                br(), h4("Even a ggplot2 chart"),
                                list(
                                    downloadLink(
                                        outputId = "Download Output Even a ggplot2 chart",
                                        label = "Download the plot displayed below"),
                                    plotOutput(
                                        outputId = "Plot Output Even a ggplot2 chart"))))))),
            conditionalPanel(
                condition = "$('html').hasClass('shiny-busy')",
                tags$div(br(), "Please wait... ",
                         tags$img(src = "https://media.giphy.com/media/3o7TKtnuHOHHUjR38Y/giphy.gif",
                                  alt = "loader",
                                  style = "width: 4em; height: 4em;"))))))
