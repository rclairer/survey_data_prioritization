library(shiny)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("Length and age structure tallies"),
  
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::checkboxGroupInput(
        "table_type",
        "Display:",
        choices = c("Length" = "length",
                    "Age" = "age"),
        selected = c("length", "age")
      ),
      
      shiny::uiOutput("year_ui"),
      
      shiny::actionButton("toggle_all", "Select/deselect all species"),
      
      shiny::uiOutput("species_ui")
      
    ),
      
    shiny::mainPanel(
          shiny::uiOutput("dynamic_content")
    )
  )
)