library(shiny)
library(ggplot2)
library(plotly)

config <- jsonlite::read_json("../config.json")
metadata_api <- paste(config$data_api, "/", config$metadata_path, sep = "")


ui <- function(request) {
    fluidPage(
        uiOutput("title"),
        tags$div(
            id = "dimension",
            uiOutput("first_dimension")
        ),
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


server <- function(input, output, session) {
    observe({
        query <- shiny::parseQueryString(session$clientData$url_search)
        request <- list("variable" = query$variable)
        data <- httr::POST(
            metadata_api,
            body = request,
            encode = "json"
        )
        metadata <- jsonlite::fromJSON(httr::content(data, "text"))
        cat(file = stderr(), paste("data: ", metadata, "\n"))


        set_title(output, metadata)
        dimensions <- transfer.server::get_dimensions(metadata)
        set_first_dimension(output, dimensions)
    })
}

shinyApp(ui = ui, server = server)
