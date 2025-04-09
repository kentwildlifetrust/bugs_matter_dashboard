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
      class = "welcome-page",
      shiny::div(
        class = "welcome-page-content",
        shiny::div(
          class = "welcome-header",
          shiny::h1(
            "Get Involved"
          ),
          shiny::br(),
          shiny::p(
            class = "welcome-lead",
            "Here, you can keep up-to-date with the findings of Bugs Matter, the global citizen science
            survey of ‘bug splats’ on vehicle number plates to monitor flying insect abundance. You can explore
            where and how many journeys have been recorded, and see information about the lengths of journeys,
            and the types of vehicles used. You can explore trends in the number of bugs splatted by location and over different time periods.
            You can also find information on the numbers of users in different parts of the world and how many journeys have been recorded by our top recorders!"
          )
        ),
        shiny::br(),
        shiny::div(
          class = "welcome-body",
          shiny::h3("At a Glance"),
          shiny::div(
            class = "welcome-figures",
            shiny::div(
              class = "row welcome-value-row",
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "welcome-value-box",
                  title = "Distance sampled",
                  value = shiny::textOutput(ns("distance")),
                  showcase = shiny::icon("fa fa-route fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#3C91E6",
                    fg = "#FFF"
                  ),
                  showcase_layout = "top right"
                )
              ),
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "welcome-value-box",
                  title = "Splats counted",
                  value = shiny::textOutput(ns("splats")),
                  showcase = shiny::icon("fa fa-mosquito fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#58A732",
                    fg = "#FFF"
                  ),
                  showcase_layout = "top right"
                )
              ),
              shiny::div(
                class = "col-sm",
                bslib::value_box(
                  class = "welcome-value-box",
                  title = "Change in splat rate 2021 - 2024",
                  value = shiny::textOutput(ns("trend")),
                  showcase = shiny::icon("fa fa-chart-line-down fa-solid"),
                  theme = bslib::value_box_theme(
                    bg = "#F46036",
                    fg = "#FFF"
                  ),
                  showcase_layout = "top right"
                )
              )
            ),
            br(),
            bslib::card(
              min_height = 600,
              bslib::card_header(
                "Modelled change in splat rate, 2021 to 2024"
              ),
              bslib::card_body(
                class = "p-0",
                leaflet::leafletOutput(ns("map"), height = "100%")
              ),
              full_screen = TRUE
            ),
          ),
          shiny::br(),
          shiny::h3("About the Project"),
          shiny::p(
            "The Bugs Matter citizen science survey uses an innovative
method for large-scale indiscriminate monitoring of flying
insect populations. Citizen scientists record the number
of insect splats on their vehicle number plates following
a journey, having first removed any residual insects from
previous journeys. It has the potential to provide an efficient,
standardised and scalable approach to monitor trends in
insect abundance across local, regional and global scales. The sampling technique is based on the ‘windscreen
phenomenon’, a term given to the anecdotal observation that fewer insect splats appear on
the windscreens of cars now compared to a decade or
several decades ago. These observations, which have also
been reported from empirical data",
            bslib::tooltip(shiny::a("[1]", class = "ref-link ref-link-nospace"), "Møller, 2019", placement = "top"),
            ", have been
interpreted as an indicator of major global declines in insect
abundance."
          ),
          shiny::p(
            "A growing body of evidence",
            bslib::tooltip(shiny::a("[2]", class = "ref-link"), "Fox et al., 2013; Hallmann et al.,
2017; Goulson, D. 2019; Sánchez-Bayo et al., 2019; Thomas
et al., 2019; van der Sluijs, 2020; Macadam et al., 2020;
Outhwaite, McCann and Newbold, 2022", placement = "top"),
            "highlights population
declines in insects and other invertebrates at UK and global
scales. These declines, which are evident across all functional
groups of insects (herbivores, detritivores, parasitoids,
predators and pollinators), could have catastrophic impacts
on the Earth’s natural systems and human survivability
on our planet. Invertebrates are functionally of greater
importance than large-bodied fauna, and in terms of biomass,
bioabundance and species diversity, they make up the
greatest proportion of life on Earth."
          ),
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
          )
        )
      )
    )
  )
}

#' get_involved Server Functions
#'
#' @noRd
mod_get_involved_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_get_involved_ui("get_involved_1")

## To be copied in the server
# mod_get_involved_server("get_involved_1")
