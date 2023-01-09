library(shiny)
library(plotly)
library(soep.plots)

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
                conditionalPanel(
                    condition = "input.first_dimension != 'none'",
                    checkboxInput(
                        "hide_legend",
                        "Legende ausblenden",
                        value = FALSE
                    )
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

server <- function(input, output, session) {
    query <- eventReactive("", {
        return(shiny::parseQueryString(session$clientData$url_search))
    })

    metadata <- eventReactive(query(), {
        return(
            transfer.server::get_metadata(
                query = query()
            )
        )
    })
    y_scale_limits <- eventReactive(query(), {
        query_ <- unlist(query())
        if(all(c("y-min", "y-max") %in% names(query_))){
        return( c(query_["y-min"], query_["y-max"]))
        }
        return(
            vector()
        )
    })
    plot_data <- eventReactive(
        {
            input$first_dimension
            input$second_dimension
        },
        {
            if (
                input$first_dimension == input$second_dimension ||
                    input$first_dimension == "none"
            ) {
                shiny::updateSelectInput(
                    session,
                    "second_dimension",
                    selected = "none"
                )
            }
            transfer.server::get_data(metadata(), input, "numerical")
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
                    c(input$first_dimension, input$second_dimension)
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
        set_second_dimension(
            input,
            output,
            transfer.server::get_dimensions(metadata())
        )
    })
    observeEvent(
        {
            plot_data()
            input
        },
        {
            arguments <- list(
                fields = list(
                    "year" = list("label" = "Erhebungsjahr"),
                    "mean" = list("label" = "Mittelwert")
                ),
                data = plot_data(),
                x_axis = "year",
                y_axis = "mean"
            )
            group_axis <- group_by()
            if (length(group_axis) > 0) {
                arguments[["group_axis"]] <- group_axis
            }
            data_plot <- do.call(soep.plots::numeric_plot, arguments)


            filename <- paste0(
                gsub(" ", "_", metadata()$title),
                "_",
                paste(group_by(), collapse = "_")
            )
            filename <- gsub("\\_$", "", filename)
            output$download_data <- downloadHandler(
                filename = paste0(
                    filename,
                    ".csv"
                ),
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
                if (length(y_scale_limits()) == 2){
                    cat(file=stderr(), paste0(data_plot$y_scale_limits, "\n"))
                    data_plot$set_y_scale_limits(y_scale_limits =y_scale_limits())
                    cat(file=stderr(), paste0(length(data_plot$y_scale_limits), "\n"))
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
