ui <- shiny::shinyUI(
    fluidPage(
        HTML("<div id=\"blackbaud-donation-form_6501479b-1ecd-4eee-a70e-430fe035e683\"></div>
<script src=\"https://sky.blackbaudcdn.net/static/donor-form-loader/2/main.js\"></script>
<script>BBDonorFormLoader.newBlackbaudDonationFormZoned('renxt', 'p-pi2R9adPg0CeWlO8qniPkw', '6501479b-1ecd-4eee-a70e-430fe035e683', 'eur')</script>")

    )
)
    

server <- function(input, output) {}

shiny::shinyApp(ui, server)