library(shiny)
library(ggplot2)
library(plotly)
library(soep.plots)

config <- jsonlite::read_json("../config.json")
metadata_api <- paste(config$data_api, "/", config$metadata_path, sep = "")
data_api <- paste(config$data_api, "/", config$data_path, sep = "")


ui <- function(request) {
    fluidPage(
        uiOutput("title"),
        sidebarLayout(
            sidebarPanel(
                uiOutput("year_range"),
                tags$div(
                    id = "dimension",
                    uiOutput("first_dimension"),
                    conditionalPanel(
                        condition = "input.first_dimension != 'none'",
                        uiOutput("second_dimension")
                    )
                ),
                checkboxInput(
                    "confidence_interval",
                    "Konfidenzintervall",
                    value = TRUE
                ),
                downloadButton("download_data", "Download CSV"),
                width = 2,
            ),
            mainPanel(
                includeHTML("./app.html"),
                fluidRow(
                    plotlyOutput("plot"),
                ),
                width = 10,
            )
        )
    )
}

set_title <- function(output, metadata) {
    output$title <- shiny::renderUI({
        return(shiny::titlePanel(as.character(metadata$title)))
    })
}

set_first_dimension <- function(output, dimensions) {
    output$first_dimension <- shiny::renderUI({
        selectInput("first_dimension",
            label = "Dimension:",
            choices = dimensions,
            selected = "none"
        )
    })
}


set_second_dimension <- function(input, output, dimensions) {
    dimensions <- dimensions[dimensions != input$first_dimension]
    selected <- dimensions[dimensions == input$second_dimension]
    if (length(selected) == 0 || selected == input$first_dimension) {
        selected <- "none"
    }
    dimensions["Keine"] <- "none"
    output$second_dimension <- shiny::renderUI({
        shiny::selectInput("second_dimension",
            label = "Zweite Dimension:",
            choices = dimensions,
            selected = selected
        )
    })
}

set_year_range <- function(output, metadata) {
    output$year_range <- shiny::renderUI({
        shiny::sliderInput(
            "year_range",
            label = "Jahre",
            min = metadata$start_year,
            max = metadata$end_year,
            value = c(metadata$start_year, metadata$end_year),
            sep = "",
            step = 1
        )
    })
}

get_metadata <- function(session, api_url) {
    query <- shiny::parseQueryString(session$clientData$url_search)
    request <- list("variable" = query$variable)
    response <- httr::POST(
        api_url,
        body = request,
        encode = "json"
    )
    return(jsonlite::fromJSON(httr::content(response, "text")))
}

get_data <- function(metadata, input) {
    dimensions <- NULL
    cat(file = stderr(), "In get_data", "\n")
    if (input$first_dimension != "none") {
        dimensions <- input$first_dimension
    }
    if (input[["second_dimension"]] != "none") {
        dimensions <- c(dimensions, input$second_dimension)
    }
    if (is.null(dimensions)) {
        request <- list(
            "variable" = metadata$variable,
            "dimensions" = list()
        )
    } else {
        request <- list(
            "variable" = metadata$variable,
            "dimensions" = as.list(c(dimensions))
        )
    }
    response <- httr::POST(
        data_api,
        body = request,
        encode = "json"
    )
    return(read.csv(text = httr::content(response, "text")))
}

render_plot <- function(input, output, data_plot) {
    output$plot <- renderPlotly({
        if (input$confidence_interval) {
            data_plot$enable_confidence_interval()
        } else {
            data_plot$disable_confidence_interval()
        }
        data_plot$set_year_range(year_range = input$year_range)

        return(data_plot$plot())
    })
}

server <- function(input, output, session) {
    plot_data <- data.frame()
    data_plot <- NULL
    metadata <- eventReactive("", {
        cat(file = stderr(), "In metadata", "\n")
        return(get_metadata(session = session, api_url = metadata_api))
    })
    observeEvent(metadata(), {
        cat(file = stderr(), "In dimension", "\n")
        dimensions <- transfer.server::get_dimensions(metadata())
        dimensions["Keine"] <- "none"
        set_first_dimension(output, dimensions)
        return(dimensions)
    })
    observeEvent(input$first_dimension, {
        set_second_dimension(
            input,
            output,
            transfer.server::get_dimensions(metadata())
        )
    })
    plot_data <- eventReactive(
        {
            input$first_dimension
            input$second_dimension
        },
        {
            cat(file = stderr(), "In plot data", "\n")
            get_data(metadata(), input)
        },
        ignoreNULL = TRUE
    )

    group_by <- eventReactive(
        {
            input$first_dimension
            input$second_dimension
            plot_data()
        },
        {
            cat(file = stderr(), "In group", "\n")
            if (input$first_dimension == "none") {
                return(vector())
            }
            if (input$second_dimension != "none") {
                return(c(input$first_dimension, input$second_dimension))
            }
            return(input$first_dimension)
        }
    )

    set_title(output, metadata())
    set_year_range(output, metadata())

    observeEvent(
        {
            plot_data()
            input
        },
        {
            data_plot <- soep.plots::numeric_plot(
                fields = list(
                    "year" = list("label" = "Erhebungsjahr"),
                    "weighted_mean" = list("label" = "Durchschnittsgehalt")
                ),
                data = plot_data(),
                x_axis = "year",
                y_axis = "mean",
                group_by = group_by(),
            )
            output$download_data <- downloadHandler(
                filename = "data.csv",
                content = function(file) {
                    write.csv(data_plot$get_data(), file, row.names = FALSE)
                }
            )


            output$plot <- renderPlotly({
                if (input$confidence_interval) {
                    data_plot$enable_confidence_interval()
                } else {
                    data_plot$disable_confidence_interval()
                }
                data_plot$set_year_range(year_range = input$year_range)

                return(data_plot$plot())
            })
        }
    )
}


shinyApp(ui = ui, server = server)
