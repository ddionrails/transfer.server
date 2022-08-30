library(shiny)
library(plotly)
library(soep.plots)


ui <- function(request) {
    fluidPage(
        shinyjs::useShinyjs(),
        uiOutput("title"),
        sidebarLayout(
            div( id = "menu", 
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
                downloadButton("download_data", "Download CSV")
            )
            ),
            mainPanel(
                includeHTML("../app.html"),
                fluidRow(
                    actionButton("toggle_sidebar", "Menu Aus-/Einblenden"),
                    plotlyOutput("plot"),
                )
            )
        )
    )
}

set_second_dimension <- function(input, output, dimensions) {
    dimensions <- dimensions[dimensions != input$first_dimension]
    selected <- dimensions[dimensions == input$second_dimension]
    if (length(selected) == 0 || selected == input$first_dimension ||
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

get_value_labels <- function(metadata) {
    output <- as.list(metadata$dimensions$labels)
    names(output) <- metadata$dimensions$variable
    return(as.list(output))
}

server <- function(input, output, session) {
    query <- eventReactive("", {
        return(shiny::parseQueryString(session$clientData$url_search))
    })

    metadata <- eventReactive(query(), {
        return(transfer.server::get_metadata(query = query()))
    })
    plot_data <- eventReactive(
        {
            input$first_dimension
            input$second_dimension
        },
        {
            transfer.server::get_data(metadata(), input, "categorical")
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
        transfer.server::set_title(output, metadata(), query())
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
            transfer.server::set_year_text_input(output, query_["start-year"], query_["end-year"])
            return()
        }
        transfer.server::set_year_range(output, metadata())
    })

    year_range <- eventReactive(list(input$year_range, query(), input$start_year, input$end_year), {
        query_ <- unlist(query())
        query_range <- c(input$start_year, input$end_year)
        if (!is.null(query_range)) {
            return(as.numeric(query_range))
        }
        return(input$year_range)
    })


    observeEvent(metadata(), {
        dimensions <- transfer.server::get_dimensions(metadata())
        dimensions["Keine"] <- "none"
        transfer.server::set_first_dimension(output, dimensions)
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


    observeEvent(input$toggle_sidebar, {
        transfer.server::toggle_menu()
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
                    "proportion" = list("label" = "Anteil in Prozent")
                ),
                data = plot_data(),
                x_axis = "year",
                y_axis = "proportion"
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

                plotly_plot <- data_plot$plot()

                if (input$hide_legend) {
                    plotly_plot <- plotly_plot %>% layout(showlegend = FALSE)
                }

                return(plotly_plot)
            })
        }
    )
}

shinyApp(ui = ui, server = server)
