
#' @export get_dimensions
get_dimensions <- function(metadata) {
    output <- metadata$dimensions$column
    names(output) <- metadata$dimensions$label
    return(as.list(output))
}
