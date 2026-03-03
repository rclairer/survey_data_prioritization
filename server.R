library(here)
library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(DT)

server <- function(input, output, session) {
  
  length_data <- readr::read_csv(
    here::here("2026", "length_tally_table_2026.csv"),
    #check.names = FALSE,
    #stringsAsFactors = FALSE
  )
  
  #extract year columns
  year_cols <- names(length_data)[-1]
  years <- as.numeric(year_cols)
  year_lookup <- setNames(year_cols, years)
  
  age_structure_data <- readr::read_csv(
    here::here("2026", "age_structure_tally_table_2026.csv"),
    #check.names = FALSE,
    #stringsAsFactors = FALSE
  )
  
  tows_targets_year <- readr::read_csv(
    here::here("2026", "tows_targets_2026.csv")
  )
    
  tows_targets_average <- readr::read_csv(
    here::here("2026", "average_across_years_tows_targets_2026.csv")
  )
  
#Species checkboxes
  
  output$species_ui <- shiny::renderUI({
    shiny::checkboxGroupInput(
      "Species",
      "Select species:",
      choices = length_data$Species,
      selected = length_data$Species
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
    shiny::req(input$Species)
    if (length(input$Species) < length(length_data$Species)){
      shiny::updateCheckboxGroupInput(session, "Species", selected = length_data$Species)
    } else {
      shiny::updateCheckboxGroupInput(session, "Species", selected = character(0))
    }
  })
  

  length_tally_data <- shiny::reactive({
    
    shiny::req(input$Species, input$year_range)
    
    selected_years <- seq(input$year_range[1], input$year_range[2])
    
    actual_cols <- na.omit(year_lookup[as.character(selected_years)])
    
  length_data %>%
    dplyr::filter(Species %in% input$Species) %>%
    dplyr::select(Species, all_of(actual_cols))
    
  })
  
  age_structure_tally_data <- shiny::reactive({
    
    shiny::req(input$Species, input$year_range)
    
    selected_years <- seq(input$year_range[1], input$year_range[2])
    
    actual_cols <- na.omit(year_lookup[as.character(selected_years)])
    
    age_structure_data %>%
      dplyr::filter(Species %in% input$Species) %>%
      dplyr::select(Species, all_of(actual_cols))
    
  })
  
  output$length_tally <- shiny::renderTable({
    length_tally_data()
  })
  
  output$age_structure_tally <- shiny::renderTable({
    age_structure_tally_data()
  })

  output$length_plot <- shiny::renderPlot({
    shiny::req(length_tally_data(), input$year_range)
    
    df <- length_tally_data()
    
    df_long <- df |> 
      tidyr::pivot_longer(
        cols = -Species,
        names_to = "year",
        values_to = "value"
      ) |>
      dplyr::mutate(year = as.numeric(year))
    
    ggplot2::ggplot(df_long,
                    ggplot2::aes(x = year,
                                 y = value,
                                 color = Species,
                                 group = Species)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        title = "Length tally by species",
        x = "Year",
        y = "Length tally",
        color = "Species"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "right"
      )

  })
  
  output$age_structure_plot <- shiny::renderPlot({
    shiny::req(age_structure_tally_data(), input$year_range)
    
    df <- age_structure_tally_data()
    
    df_long <- df |> 
      tidyr::pivot_longer(
        cols = -Species,
        names_to = "year",
        values_to = "value"
      ) |>
      dplyr::mutate(year = as.numeric(year))
    
    ggplot2::ggplot(df_long,
                    ggplot2::aes(x = year,
                                 y = value,
                                 color = Species,
                                 group = Species)) +
      ggplot2::geom_line(linewidth = 1.2) +
      ggplot2::geom_point(size = 2) +
      ggplot2::labs(
        title = "Age structure tally by species",
        x = "Year",
        y = "Age strcuture tally",
        color = "Species"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        plot.title = ggplot2::element_text(face = "bold"),
        legend.position = "right"
      )
    
  })  
  
  output$dynamic_content <- renderUI({
    shiny::req(input$table_type)
    
    content <- list()
    
    if("length" %in% input$table_type) {
      content <- append(content, list(
        h4("Length tally"),
        shiny::tableOutput("length_tally"),
        shiny::plotOutput("length_plot"),
        hr()
      ))
      
    }
    
    if("age" %in% input$table_type) {
      content <- append(content, list(
        h4("Age structure tally"),
        shiny::tableOutput("age_structure_tally"),
        shiny::plotOutput("age_structure_plot")
      ))
    }
    
    do.call(tagList, content)
  })
  

      
}