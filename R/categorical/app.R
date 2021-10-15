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
                uiOutput("start_year"),
                uiOutput("end_year"),
                tags$div(
                    id = "dimension",
                    uiOutput("first_dimension"),
                    tags$div(
                        class = "value-dropdown",
                        conditionalPanel(
                            condition = "input.first_dimension != 'none'",
                            uiOutput("first_value")
                        )
                    ),
                    conditionalPanel(
                        condition = "input.first_value != 'none'",
                        uiOutput("second_dimension"),
                        tags$div(
                            class = "value-dropdown",
                            conditionalPanel(
                                condition = "input.second_dimension != 'none'",
                                uiOutput("second_value")
                            )
                        )
                    )
                ),
                checkboxInput(
                    "confidence_interval",
                    "Konfidenzintervall",
                    value = TRUE
                ),
                checkboxInput(
                    "bar_chart",
                    "Säulendiagramm",
                    value = FALSE
                ),
                checkboxInput(
                    "hide_legend",
                    "Legende ausblenden",
                    value = FALSE
                ),
                downloadButton("download_data", "Download CSV"),
                width = 2,
            ),
            mainPanel(
                includeHTML("../app.html"),
                fluidRow(
                    plotlyOutput("plot"),
                ),
                width = 10,
            )
        )
    )
}

set_title <- function(output, metadata, query) {
    if (!is.null(query["no-title"])) {
        if (query["no-title"] == "TRUE") {
            return()
        }
    }

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

set_first_value_label <- function(output, dimension, value_labels) {
    selected_labels <- value_labels[[dimension]]
    names(selected_labels) <- selected_labels

    if (dimension == "none") {
        output$first_value <- shiny::renderUI({
            selectInput("first_value",
                label = "Ausprägung",
                choices = list("None" = "none"),
                selected = "none"
            )
        })
        return()
    }

    output$first_value <- shiny::renderUI({
        selectInput("first_value",
            label = "Ausprägung",
            choices = selected_labels,
            selected = selected_labels[[1]]
        )
    })
}

set_second_value_label <- function(output, dimension, value_labels) {
    selected_labels <- value_labels[[dimension]]
    names(selected_labels) <- selected_labels

    if (dimension == "none") {
        output$second_value <- shiny::renderUI({
            selectInput("second_value",
                label = "Ausprägung",
                choices = list("None" = "none"),
                selected = "none"
            )
        })
        return()
    }

    output$second_value <- shiny::renderUI({
        selectInput("second_value",
            label = "Ausprägung",
            choices = selected_labels,
            selected = selected_labels[[1]]
        )
    })
}


set_second_dimension <- function(input, output, dimensions) {
    dimensions <- dimensions[dimensions != input$first_dimension]
    selected <- dimensions[dimensions == input$second_dimension]
    if (
        length(selected) == 0 ||
            selected == input$first_dimension ||
            input$first_dimension == "none"
    ) {
        selected <- "none"
        output$second_value <- shiny::renderUI({
            selectInput("second_value",
                label = "Ausprägung",
                choices = list("None" = "none"),
                selected = "none"
            )
        })
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

set_year_text_input <- function(output, start_year, end_year){
    output$start_year <- shiny::renderUI({
        shiny::textInput("start_year", "", value=start_year)
    })
    output$end_year <- shiny::renderUI({
        shiny::textInput("end_year", "", value=end_year)
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

get_metadata <- function(query, api_url) {
    request <- list("variable" = query$variable)
    response <- httr::POST(
        api_url,
        body = request,
        encode = "json"
    )
    return(jsonlite::fromJSON(httr::content(response, "text")))
}

get_dimensions <- function(metadata) {
    output <- metadata$dimensions$variable
    names(output) <- metadata$dimensions$label
    return(as.list(output))
}

get_value_labels <- function(metadata) {
    output <- as.list(metadata$dimensions$values)
    names(output) <- metadata$dimensions$variable
    return(as.list(output))
}

get_data <- function(metadata, input) {
    dimensions <- NULL
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

        return(ggplotly(data_plot$plot(), tooltip = "text"))
    })
}

server <- function(input, output, session) {
    query <- eventReactive("", {
        return(shiny::parseQueryString(session$clientData$url_search))
    })

    metadata <- eventReactive(query(), {
        return(get_metadata(query = query(), api_url = metadata_api))
    })
    plot_data <- eventReactive(
        {
            input$first_dimension
            input$second_dimension
        },
        {
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
            if (input$first_dimension == "none") {
                return(vector())
            }
            if (input$second_dimension != "none") {
                return(
                    sort(c(input$first_dimension, input$second_dimension))
                )
            }
            return(input$first_dimension)
        }
    )

    observeEvent(metadata(), {
        set_title(output, metadata(), query())
    })
    observeEvent(metadata(), {
        query_ <- unlist(query())
        if (
            !is.na(
                as.numeric(
                    query_["start-year"]
                )
            ) &&
                !is.na(
                    as.numeric(
                        query_["end-year"]
                    )
                )
        ) {
            set_year_text_input(output, query_["start-year"], query_["end-year"])
            return()
        }
        set_year_range(output, metadata())
    })

    year_range <- eventReactive(list(input$year_range, query(), input$start_year, input$end_year), {
        query_ <- unlist(query())
        cat(file=stderr(), '"', paste(typeof(input$start_year)), '"', "\n")
        query_range <- c(input$start_year, input$end_year)
        if (!is.null(query_range)) {
            cat(file = stderr(), paste(query_range), "\n")
            return(as.numeric(query_range))
        }
        cat(file = stderr(), paste(input$year_range), "\n")
        return(input$year_range)
    })


    observeEvent(metadata(), {
        dimensions <- get_dimensions(metadata())
        dimensions["Keine"] <- "none"
        set_first_dimension(output, dimensions)
        return(dimensions)
    })
    observeEvent(input$first_dimension, {
        value_labels <- get_value_labels(metadata())
        set_first_value_label(
            output = output,
            dimension = input$first_dimension,
            value_labels = value_labels
        )
        set_second_dimension(
            input,
            output,
            transfer.server::get_dimensions(metadata())
        )
    })
    observeEvent(input$second_dimension, {
        value_labels <- get_value_labels(metadata())
        set_second_value_label(
            output = output,
            dimension = input$second_dimension,
            value_labels = value_labels
        )
    })
    reaction_list <- reactive({
        list(input$hide_legend, input$first_value, input$second_value, input$confidence_interval, input$bar_chart, year_range())
    })
    observeEvent(
        {
            reaction_list()
        },
        {
            filename <- paste0(
                gsub(" ", "_", metadata()$title),
                "_",
                paste(group_by(), collapse = "_")
            )
            output$download_data <- downloadHandler(
                filename = paste0(
                    filename,
                    ".csv"
                ),
                content = function(file) {
                    write.csv(plot_data(), file, row.names = FALSE)
                }
            )

            arguments <- list(
                fields = list(
                    "year" = list("label" = "Erhebungsjahr"),
                    "percent" = list("label" = "Anteil in Prozent")
                ),
                data = plot_data(),
                x_axis = "year",
                y_axis = "percent"
            )
            arguments[["group_axis"]] <- metadata()[["variable"]]
            dimension_filter <- list()
            if (
                input$first_dimension != "none" &
                    input$first_value != "none"
            ) {
                dimension_filter[input$first_dimension] <- input$first_value
                if (
                    input$second_dimension != "none" &
                        input$second_value != "none"
                ) {
                    dimension_filter[
                        input$second_dimension
                    ] <- input$second_value
                }
            }
            arguments[["dimension_metadata"]] <- dimension_filter
            cat(file = stderr(), paste(arguments[["group_axis"]]), "\n")
            cat(file = stderr(), paste(arguments[["dimension_metadata"]]), "\n")
            data_plot <- do.call(soep.plots::categorical_plot, arguments)


            if (input$bar_chart) {
                data_plot$set_to_bar()
            } else {
                data_plot$set_to_line()
            }


            output$plot <- renderPlotly({
                if (input$confidence_interval) {
                    data_plot$enable_confidence_interval()
                } else {
                    data_plot$disable_confidence_interval()
                }
                data_plot$set_year_range(year_range = year_range())

                plotly_plot <- ggplotly(data_plot$plot(), tooltip = "text")
                
                if(input$hide_legend){
                    plotly_plot <- plotly_plot %>% layout(showlegend = FALSE)
                } 

                return(plotly_plot)
            })
        }
    )
}


shinyApp(ui = ui, server = server)
