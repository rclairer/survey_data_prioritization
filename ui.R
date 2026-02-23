library(shiny)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("Age structure tally"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::uiOutput("year_ui"),
      
      shiny::actionButton("toggle_all", "Select/deselect all species"),
      
      shiny::uiOutput("species_ui")
      
    ),
      
    shiny::mainPanel(
      shiny::tableOutput("table")
    )
  )
)