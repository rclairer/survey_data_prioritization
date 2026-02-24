library(shiny)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("Length and age structure tallies"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::uiOutput("year_ui"),
      
      shiny::actionButton("toggle_all", "Select/deselect all species"),
      
      shiny::uiOutput("species_ui")
      
    ),
      
    shiny::mainPanel(
          h4("Length tally"),
          shiny::tableOutput("length_tally"),
          hr(),
          h4("Age structure tally"),
          shiny::tableOutput("age_structure_tally")
    )
  )
)