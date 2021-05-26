library(shiny)
library(ggplot2)
library(plotly)

config <- jsonlite::read_json("../config.json")
metadata_api <- paste(config$data_api, "/", config$metadata_path, sep = "")


ui <- function(request) {
    fluidPage(
        uiOutput("title"),
        sidebarLayout(
            sidebarPanel(
                tags$div(
                    id = "dimension",
                    uiOutput("first_dimension"),
                    uiOutput("second_dimension")
                ),
            ),
            mainPanel(
                includeHTML("./app.html"),
                plotlyOutput("plot"),
                width = 10,
            )
        )
    )
}

set_title <- function(output, metadata) {
    output$title <- renderUI({
        return(shiny::titlePanel(as.character(metadata$title)))
    })
}

set_first_dimension <- function(output, dimensions) {
    output$first_dimension <- renderUI({
        selectInput("dimension",
            label = "Dimension:",
            choices = dimensions
        )
    })
}

set_second_dimension <- function(input, output, dimensions) {
    dimensions <- dimensions[dimensions != input$dimension]
    output$second_dimension <- renderUI({
        selectInput("second_dimension",
            label = "Second Dimension:",
            choices = dimensions
        )
    })
}


server <- function(input, output, session) {
    dimensions <- list()
    observe({
        query <- shiny::parseQueryString(session$clientData$url_search)
        request <- list("variable" = query$variable)
        data <- httr::POST(
            metadata_api,
            body = request,
            encode = "json"
        )
        metadata <- jsonlite::fromJSON(httr::content(data, "text"))

        set_title(output, metadata)
        dimensions <<- transfer.server::get_dimensions(metadata)
        set_first_dimension(output, dimensions)
    })

    observeEvent(input$dimension,
        set_second_dimension(input, output, dimensions),
        ignoreInit = TRUE,
        ignoreNULL = TRUE
    )
}

shinyApp(ui = ui, server = server)
