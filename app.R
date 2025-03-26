# Load Shiny
library(shiny)

# Source Sidebar and Graph Modules
source("R/mod_roleNeutral.R")

# Define UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .param-row {
        display: flex;
        align-items: center;
        gap: 20px;
        width: 100%;
      }
      .param-label {
        width: 180px;
        font-weight: bold;
        flex-shrink: 0;
      }
      .param-inputs {
        display: flex;
        align-items: center;
        gap: 10px;
        flex-grow: 1;
      }
      .param-row .shiny-input-container {
        flex-grow: 1;
        min-width: 100px;
        max-width: 250px;
      }
      .param-row .shiny-slider-container {
        flex-grow: 2;
      }
    "))
  ),
  
  navbarPage(
    tags$img(src = "imgs/ROLE-logo.png", height = "40px", style = "margin-top: -10px;"),
    mod_roleNeutral_ui("roleNeutral_1")
  )
)

# Define Server
server <- function(input, output, session) {
  mod_roleNeutral_server("roleNeutral_1")
}

# Run the App
shinyApp(ui = ui, server = server)