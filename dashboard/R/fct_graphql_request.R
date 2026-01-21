#' graphql_request
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
graphql_request <- function(query, variables) {
    response <- httr::POST(
        url,
        body = list(query = query, variables = variables),
        encode = "json",
        httr::add_headers(Authorization = paste("JWT", token))
    )

    content_text <- httr::content(response, as = "text")
    return(
        jsonlite::fromJSON(content_text, flatten = TRUE)
    )
}