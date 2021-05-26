library(shiny)
library(ggplot2)
library(plotly)

ui <- function(request) {
    fluidPage(
        uiOutput("title")
    )
}

server <- function(input, output, session) {
    output$title <- renderUI({
        return(shiny::titlePanel("Income Statistics"))
    })
}

shinyApp(ui = ui, server = server)
