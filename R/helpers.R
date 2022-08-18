library(ggplot2)
library(plotly)


config <- jsonlite::read_json("./extdata/config.json")
metadata_api <- paste(config$data_api, config$metadata_path, sep = "")
data_api <- paste(config$data_api, config$data_path, sep = "")

#' @export 
get_dimensions <- function(metadata) {
    output <- metadata$dimensions$variable
    names(output) <- metadata$dimensions$label
    return(as.list(output))
}

#' @export set_title
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

#' @export set_first_dimension
set_first_dimension <- function(output, dimensions) {
    output$first_dimension <- shiny::renderUI({
        selectInput("first_dimension",
            label = "Dimension:",
            choices = dimensions,
            selected = "none"
        )
    })
}

#' @export set_year_text_input
set_year_text_input <- function(output, start_year, end_year){
    output$start_year <- shiny::renderUI({
        shiny::textInput("start_year", "", value=start_year)
    })
    output$end_year <- shiny::renderUI({
        shiny::textInput("end_year", "", value=end_year)
    })
}

#' @export set_year_range
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

#' @export get_metadata 
get_metadata <- function(query) {
    request <- list("variable" = query$variable)
    response <- http_request(metadata_api, request)
    return(jsonlite::fromJSON(txt = httr::content(response, "text", encoding="utf-8")))
}

#' @export get_data
get_data <- function(metadata, input, type) {
    dimensions <- NULL
    if (input$first_dimension != "none") {
        dimensions <- input$first_dimension
    }
    if (input[["second_dimension"]] != "none") {
        dimensions <- c(dimensions, input$second_dimension)
    }
    if (is.null(dimensions)) {
        request <- list(
            "variable" = metadata$id,
            "dimensions" = "",
           "type" = type
        )
    } else {
        request <- list(
            "variable" = metadata$id,
            "dimensions" = paste(c(dimensions), collapse=","),
           "type" = type
        )
    }

    response <- http_request(data_api, request)

    return(read.csv(text = httr::content(response, "text")))
}

http_request <- function(url, query){
    if(config$user != "" & config$password != ""){
        return (httr::GET(
            url,
            query = query,
            httr::authenticate(config$user, config$password)
            )
        )
    }
    return(
        response <- httr::GET(
            url,
            query = query,
            )
    )
    

}
