library(shiny)
library(gt)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("FRS survey sampling protocol review"),
  
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
        shiny::sidebarLayout(
          shiny::sidebarPanel(
            shiny::selectInput(
              inputId = "table_choice",
              label = "Display:",
              choices = c("2025 catch per tow tallies" = "tows_targets",
                          "Average (across all years) catch per tow tallies" = "tows_targets_average"),
              selected = "tows_targets"
            )
          ),
          shiny::mainPanel(
        gt::gt_output("tows_targets_table")
    )
  )
)
)
)