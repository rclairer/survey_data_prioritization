library(shiny)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("Age structure tally"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::uiOutput("species_ui"),
      
      shiny::uiOutput("year_ui")
      
    ),
      
    shiny::mainPanel(
      shiny::tableOutput("table")
    )
  )
)