library(here)
library(shiny)
library(dplyr)

server <- function(input, output, session) {
  
  data <- read.csv(
    here::here("2026", "age_structure_tally_table_2026.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  #extract year columns
  year_cols <- names(data)[-1]
  years <- as.numeric(year_cols)
  year_lookup <- setNames(year_cols, years)
  
#species checkboxes
  
  output$species_ui <- renderUI({
    checkboxGroupInput(
      "species",
      "Select species:",
      choices = data$species,
      selected = data$species #default
    )
    
  })
  
#dynamically create year slider
  
  output$year_ui <- shiny::renderUI({
    

  shiny::sliderInput(
    "year_range",
    "Select year range:",
    min = min(years),
    max = max(years),
    value = c(min(years), max(years)), #default
    sep = ""
  )
  
  })
  
  filtered_data <- shiny::reactive({
    
    shiny::req(input$species, input$year_range)
    
    selected_years <- seq(input$year_range[1], input$year_range[2])
    
    actual_cols <- na.omit(year_lookup[as.character(selected_years)])
    
  data %>%
    dplyr::filter(species %in% input$species) %>%
    dplyr::select(species, all_of(actual_cols))
    
  })
  
  output$table <- renderTable({
    filtered_data()
  })
}