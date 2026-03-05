library(shiny)
library(gt)

ui <- shiny::fluidPage(
  
  shiny::titlePanel("FRS survey sampling protocol review"),
  
  shiny::tabsetPanel(
    
    tabPanel("Home",
      # Add text using HTML formatting functions
       h1("About the tool and data"), # Creates a primary header
          p("This tool facilitates collaborative review of the NOAA NWFSC FRAM FRS survey sampling protocols and data collection targets designed to support stock assessments and research. The data are from the NOAA NWFSC West Coast Groundfish Bottomtrawl Survey (WCGBTS) and NOAA NWFSC Hook and Line Survey (HKLS). The data are filtered to include the species and stock areas currently included (or considered for inclusion) in the PFMC Fishery Managment Plan."), # Creates a paragraph of text
          p("*This is a work-in-process, results may be updated."),
            "To view the code and to download the data tables, visit the",
            a("GitHub repository.", href = "https://github.com/rclairer/survey_data_prioritization/") # Inserts a hyperlink
      ), 
    
    shiny::tabPanel(
      "WCGBTS length and age structure tallies",
        shiny::sidebarLayout(
        shiny::sidebarPanel(
          
      p("Select the data type (length or age structure tallies, year range, and species to display."),
      
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
            ),
            p("The highlighted cells correspond to the set length targets.")
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

      p("Select the data type (length or age structure tallies, year range, and species to display."),
      
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