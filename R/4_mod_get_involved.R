#' get_involved UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_get_involved_ui <- function(id) {
  ns <- NS(id)
  bslib::page_fillable(
    shiny::div(
      class = "overview-page",
      shiny::div(
        class = "overview-page-content",
        # shiny::div(
        #   class = "overview-header",
        #   shiny::h1(
        #     "Get Involved"
        #   ),
        #   shiny::tags$br(),
        #   shiny::p(
        #     class = "overview-lead",
        #     "Here, you can keep up-to-date with the findings of Bugs Matter, the global citizen science
        #     survey of ‘bug splats’ on vehicle number plates to monitor flying insect abundance. You can explore
        #     where and how many journeys have been recorded, and see information about the lengths of journeys,
        #     and the types of vehicles used. You can explore trends in the number of bugs splatted by location and over different time periods.
        #     You can also find information on the numbers of users in different parts of the world and how many journeys have been recorded by our top recorders!"
        #   )
        # ),
        # shiny::tags$br(),
        shiny::div(
          class = "overview-body",
          shiny::p(
            "Habitat loss and fragmentation has caused declines in
biodiversity across the world. The conversion of natural
habitats into agriculture, urban areas, and infrastructure
development leads to the loss of suitable habitats for insects,
making it difficult for them to feed and reproduce. The
widespread use of chemical pesticides, including insecticides,
herbicides, and fungicides, can have detrimental effects on
insect populations. Climate change alters weather patterns,
affecting insect life cycles, behaviour, and distribution. Some
insects may struggle to adapt to rapidly changing conditions
or may lose suitable habitats due to shifting climate zones."
          ),
          shiny::p(
            "Using alternatives to peat fertilizer can help reduce CO2 emissions and slow the impact
of climate change on insects and our environment. By eliminating or reducing our use of pesticides,
we can stop the decline of thousands of insects in an instant. You can help the insects in your garden
by letting the grass grow longer,  sowing wildflowers, and generally being less tidy. Climate change is
a growing threat to a wide range of wildlife, including insects. The two best things you can do is eat
less meat and make fewer vehicle journeys. Join an organisation such as your local Wildlife Trust or
Buglife. Charities like these do vital work to protect and restore our most important wildlife sites,
restore lost habitats at scale and reconnect our countryside. Most importantly, make every journey count,
      by taking part in Bugs Matter!"
          )
        )
      )
    )
  )
}

#' get_involved Server Functions
#'
#' @noRd
mod_get_involved_server <- function(id, conn) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
  })
}

## To be copied in the UI
# mod_get_involved_ui("get_involved_1")

## To be copied in the server
# mod_get_involved_server("get_involved_1")
