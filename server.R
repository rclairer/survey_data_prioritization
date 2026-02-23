library(here)
library(shiny)
library(dplyr)

server <- function(input, output, session) {
  
  length_data <- read.csv(
    here::here("2026", "length_tally_table_2026.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  age_structure_data <- read.csv(
    here::here("2026", "age_structure_tally_table_2026.csv"),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  
  #extract year columns
  year_cols <- names(length_data)[-1]
  years <- as.numeric(year_cols)
  year_lookup <- setNames(year_cols, years)
  
#species checkboxes
  
  output$species_ui <- shiny::renderUI({
    shiny::checkboxGroupInput(
      "species",
      "Select species:",
      choices = length_data$species,
      selected = length_data$species
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
  
  shiny::observeEvent(input$toggle_all, {
    shiny::req(input$species)
    if (length(input$species) < length(length_data$species)){
      shiny::updateCheckboxGroupInput(session, "species", selected = length_data$species)
    } else {
      shiny::updateCheckboxGroupInput(session, "species", selected = character(0))
    }
  })
  

  length_tally_data <- shiny::reactive({
    
    shiny::req(input$species, input$year_range)
    
    selected_years <- seq(input$year_range[1], input$year_range[2])
    
    actual_cols <- na.omit(year_lookup[as.character(selected_years)])
    
  length_data %>%
    dplyr::filter(species %in% input$species) %>%
    dplyr::select(species, all_of(actual_cols))
    
  })
  
  age_structure_tally_data <- shiny::reactive({
    
    shiny::req(input$species, input$year_range)
    
    selected_years <- seq(input$year_range[1], input$year_range[2])
    
    actual_cols <- na.omit(year_lookup[as.character(selected_years)])
    
    age_structure_data %>%
      dplyr::filter(species %in% input$species) %>%
      dplyr::select(species, all_of(actual_cols))
    
  })
  
  output$length_tally <- shiny::renderTable({
    length_tally_data()
  })
  
  output$age_structure_tally <- shiny::renderTable({
    age_structure_tally_data()
  })
  
}