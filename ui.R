library(shiny)
library(gt)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("FRS survey sampling protocol"),
  
  shiny::tabsetPanel(
    
    shiny::tabPanel(
      "Length and age structure tallies",
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
),
    
    shiny::tabPanel(
        "Targets and catch per tow tallies",
        gt::gt_output("tows_targets")
    )
  )
)