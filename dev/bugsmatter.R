library(httr)
library(jsonlite)
library(dplyr)

url <- "https://api.coreo.io/graphql"
token <- Sys.getenv("BUGS_MATTER_API_TOKEN")
projectId <- 343

# Generic GraphQL request function
graphql_request <- function(query, variables) {
  response <- POST(
    url,
    body = list(query = query, variables = variables),
    encode = "json",
    add_headers(Authorization = paste("JWT", token))
  )

  content_text <- content(response, as = "text")
  fromJSON(content_text, flatten = TRUE)
}

print_users <- function(){
    usersQuery <- '
        query bugsMatterDashboardNightlyUsersQuery($projectId: Int!){
            project(id: $projectId) {
                memberships(where: { createdAt: { gt: "2024-04-15" } }){
                    user {
                        id
                        displayName
                        username
                    }
                }
            }
        }
    '
    data <- graphql_request(usersQuery, list(projectId = projectId))
    print(data)
}

print_journeys <- function(){
    journeysQuery <- '
        query bugsMatterDashboardNightlyJourneysQuery($projectId: Int!){
            records(where: { projectId: $projectId }, limit: 100){
                id
                geometry {
                    type
                    coordinates
                }
                data
                userId
            }
        }
    '
    data <- graphql_request(journeysQuery, list(projectId = projectId)) %>%
        as.data.frame() %>%
        tibble()
    print(data)
}

#print_users()
print_journeys()
