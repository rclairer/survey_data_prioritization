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
          shiny::fluidRow(
            column(width = 6,
                   h4("Length tally"),
                   shiny::tableOutput("length_tally"))
          ),
            column(width = 6,
                   h4("Age structure tally"),
                   tableOutput("age_structure_tally"))
    )
  )
)