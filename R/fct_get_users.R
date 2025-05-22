#' get_users
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @export
get_users <- function(conn, url, project_id, date) {
    message(sprintf("Fetching users for %s", date))
    # Construct end date by adding time component and one day
    end_date <- as.Date(date) + 1
    
    # Original users query
    usersQuery <- '
        query BugsMatterNightlyUsersQuery($projectId: Int!, $date: String!, $endDate: String!){
            project(id: $projectId) {
                memberships(where: { 
                    createdAt: { 
                        gte: $date,
                        lt: $endDate
                    } 
                }) {
                    user {
                        id
                        username
                        createdAt
                    }
                }
            }
        }
    '

    response <- httr::POST(
        url,
        body = list(
            query = usersQuery,
            variables = list(
                projectId = project_id,
                date = date,
                endDate = end_date
            )
        ),
        encode = "json",
        httr::add_headers(Authorization = paste("JWT", token))
    )

    content_text <- httr::content(response, as = "text")

    new_users <- jsonlite::fromJSON(content_text, flatten = TRUE) %>%
        as.data.frame()

    print(new_users)

    if (nrow(new_users) == 0) {
        return(new_users)
    }

    new_users <- new_users %>%
        tibble::tibble() %>%
        dplyr::rename_all(
            function(col) stringr::str_replace_all(col, "data.project.memberships.", "")
        ) %>%
        dplyr::rename_all(
            function(col) stringr::str_replace_all(col, stringr::fixed("user."), "")
        ) %>%
        dplyr::rename_all(
            snakecase::to_snake_case
        ) %>%
        dplyr::mutate(
            sign_up_date = as.Date(created_at)
        ) %>%
        dplyr::select(-created_at)

    DBI::dbAppendTable(conn, DBI::Id("op", "users"), new_users)

    message(sprintf("Added %s users.", nrow(new_users)))
    return(new_users)
}
