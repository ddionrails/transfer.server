
#' @export get_dimensions
get_dimensions <- function(metadata) {
    output <- metadata$dimensions$variable
    names(output) <- metadata$dimensions$label
    return(as.list(output))
}
