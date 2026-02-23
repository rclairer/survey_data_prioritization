library(shiny)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("Age structure tally"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::actionButton("toggle_all", "Select/Deselect All"),
      
      shiny::uiOutput("species_ui"),
      
      shiny::uiOutput("year_ui")
      
    ),
      
    shiny::mainPanel(
      shiny::tableOutput("table")
    )
  )
)