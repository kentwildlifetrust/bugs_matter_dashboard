#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tags$html(
    lang = "en",
    shiny::tagList(
      # Leave this function for adding external resources
      golem_add_external_resources(),
      # Your application UI logic
      bslib::page_navbar(
        id = "page_navbar",
        title = shiny::tagList(
          tags$img(
            src = "www/bm_bug_logo.png",
            height = "28px",
            alt = "Bugs Matter Logo"
          ),
          "Bugs",
          shiny::span("Matter")
        ),
        bslib::nav_panel(
          title = "Overview",
          value = 1,
          mod_overview_ui("overview_1")
        ),
        bslib::nav_panel(
          title = "Journeys",
          value = 2,
          mod_explore_journeys_ui("explore_journeys_1")
        ),
        bslib::nav_panel(
          title = "Trends",
          value = 3,
          mod_trends_ui("trends_1")
        ),
        bslib::nav_panel(
          title = "Participation",
          value = 4,
          mod_participation_ui("participation_1")
        ),
        bslib::nav_item(
          actionButton(
            "log_in",
            "Log in",
            class = "btn-primary",
            style = "flex-grow: 0; height: min-content; margin: 0 !important; font-weight: 400;",
            `aria-label` = "Log in to view organization-specific data"
          )
        ),
        theme = theme()
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  api_key <- Sys.getenv("BUGS_MATTER_COOKIE_CONTROL_API_KEY")

  golem::add_resource_path(
    "www",
    app_sys("app/www")
  )

  shiny::tags$head(
    tags$html(HTML(
      '<script async src="https://www.googletagmanager.com/gtag/js?id=G-JM7YQ5GYG1"></script>'
    )),
    tags$script(
      src = "https://cc.cdn.civiccomputing.com/9/cookieControl-9.x.min.js",
      type = "text/javascript"
    ),
    #
    tags$script(HTML(paste0(
      "
    function getQueryStringValue(key) {
      const urlParams = new URLSearchParams(window.location.search);
      return urlParams.get(key);
    }
    const config = {
      apiKey: '",
      api_key,
      "',
      product: 'COMMUNITY',
      text : {
        thirdPartyTitle : 'Warning: Some cookies require your attention',
        thirdPartyDescription : 'Consent for some third party cookies can not be automatically revoked. Please follow the link below if you want to opt out of them.'
      },
      'necessaryCookies': ['rscid', 'rscid-legacy', 'rsconnect', 'rsconnect-legacy', 'RSC-XSRF', 'RSC-XSRF-legacy'],
      'optionalCookies': [{
        'name': 'analytics',
        'label': 'Analytical Cookies',
        'description': 'Analytical cookies help us to improve our website by collecting and reporting information on its usage.',
                      cookies: ['_ga', 'ar_debug', '_gid', '_gat', '__utma', '__utmt', '__utmb', '__utmc', '__utmz', '__utmv'],
              onAccept : function(){
                  // Add Google Analytics
                    window.dataLayer = window.dataLayer || [];
                    function gtag(){dataLayer.push(arguments);}
                    gtag('js', new Date());

                    gtag('config', 'G-7TP3WRM9QZ');

                    $(document).on('click', '.bookmark-button', function(e) {
                      gtag('event', 'bookmark_url', {
                        tab: getQueryStringValue('tab')
                      });
                    });

                  // End Google Analytics
              },
              onRevoke: function(){
                  // Disable Google Analytics
                  window['ga-disable-G-7TP3WRM9QZ'] = true;
                  // End Google Analytics
              }
      }],
      'text': {
          'intro': 'These cookies are optional.'
      },
      'consentCookieExpiry': 365,
      'theme': 'light',
      'toggleType': 'checkbox',
      'closeStyle': 'button',
      'iabCMP': false
    };
    CookieControl.load( config );"
    ))),
    golem::favicon(),
    shiny::HTML(
      '<script crossorigin="anonymous" src="https://kit.fontawesome.com/82bb623444.js"></script>'
    ),
    shiny::HTML(
      '<script src="https://unpkg.com/leaflet.vectorgrid@latest/dist/Leaflet.VectorGrid.bundled.js"></script>'
    ),
    shinyjs::useShinyjs(),
    golem::bundle_resources(
      path = app_sys("app/www"),
      app_title = "Bugs Matter Dashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
