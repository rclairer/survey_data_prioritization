library(shiny)
library(gt)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("FRS survey sampling protocol review"),
  
  shiny::tabsetPanel(
    
    shiny::tabPanel(
      "WCGBTS length and age structure tallies",
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
        "WCGBTS targets and catch per tow tallies",
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
),

shiny::tabPanel(
  "HKLS length and age structure tallies",
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      
      shiny::checkboxGroupInput(
        "hkls_table_type",
        "Display:",
        choices = c("Length" = "hkls_length",
                    "Age" = "hkls_age"),
        selected = c("hkls_length", "hkls_age")
      ),
      
      shiny::uiOutput("hkls_year_ui"),
      
      shiny::actionButton("hkls_toggle_all", "Select/deselect all species"),
      
      shiny::uiOutput("hkls_species_ui")
      
    ),
    
    shiny::mainPanel(
      shiny::uiOutput("hkls_dynamic_content")
    )
  )
)

)
)