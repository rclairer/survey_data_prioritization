#packages
library(here)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(nwfscSurvey)
library(readr)
library(flextable)

dir <- here::here("2026", "hkls_data")

#source functions
source(here::here("R", "get_species_list.R"))
source(here::here("R","clean_nwfsc_hkl.R"))

#run functions
data <- readr::read_csv(here::here("2026", "hkls_data", "nwfsc_hkl_DWarehouse_version_abbreviated_02032026.csv"))

species <- get_species_list()

hkls_catch <- clean_nwfsc_hkl(dir = dir, data = data, species = species)

####################
year <- as.numeric(format(Sys.Date(), "%Y"))
data_year <- as.numeric(format(Sys.Date(), "%Y"))-1

col_names <- c("Species", as.character(2004:2019), as.character(2021:data_year))
all_years <- c(as.character(2004:2019), as.character(2021:data_year))

#blank length_tally table (before for loop)
length_tally_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
colnames(length_tally_table) <- col_names

#blank age_structure_tally table (before for loop)
age_structure_tally_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
colnames(age_structure_tally_table) <- col_names

##########################################################
catch_species <- sort(unique(hkls_catch$Common_name))

species_list <- catch_species
for (i in 1:length(species_list)) {
#  for (i in 1:5) {
  species <- species_list[i]
  species_name <- gsub(" ", "_", species)
  
  #here create folder for each species
  folder_name <- paste0("Data_", species_name)
  folder_path <- file.path(dir, folder_name)
  #create the folder if it does not exist
  if(!dir.exists(folder_path)){
    dir.create(folder_path, recursive = TRUE)
  }
  #add everything here
  
  setwd(folder_path)
  
  catch <- hkls_catch %>%
    dplyr::filter(Common_name == species)
  
  #TABLE 2
  table2_satisfactory <- catch %>%
#    dplyr::filter(Performance == "Satisfactory") %>% #check if there are any unsatisfactory tows
    group_by(as.character(Year)) %>%
    summarise(count_of_length_cm = sum(!is.na(Length_cm)),
              count_of_otosag_id = sum(!is.na(otolith_number))) %>%
    rename(year = "as.character(Year)")
  
  table2_satisfactory <- merge(data.frame("year" = all_years), table2_satisfactory, by = "year", all.x = TRUE)
  
  #optional to have 0 instead of NA
  table2_satisfactory[is.na(table2_satisfactory)] <- 0
  
  readr::write_csv(table2_satisfactory, paste0(species_name, "_table2_satisfactory.csv"))
  
  formatted_table2_satisfactory <- flextable::flextable(table2_satisfactory)
  formatted_table2_satisfactory <- flextable::theme_vanilla(formatted_table2_satisfactory)
  formatted_table2_satisfactory <- flextable::set_header_labels(formatted_table2_satisfactory,
                                                                values = list("year" = "Year",
                                                                              "count_of_length_cm" = "Count of length_cm",
                                                                              "count_of_otosag_id" = "Count of otosag_id"))
  
  flextable::save_as_image(formatted_table2_satisfactory, path = paste0(species_name, "_formatted_table2_satisfactory.png"))
  
  #add to main table
  #vector from tables for length
  length_tally <- dplyr::pull(table2_satisfactory, count_of_length_cm)
  length_tally <- as.data.frame(t(length_tally))
  length_tally <- cbind(Species = species_list[i], length_tally) #cbind one column with species name in it!!!!!
  colnames(length_tally) <- col_names
  
  #vector from tables for age structures
  age_structure_tally <- dplyr::pull(table2_satisfactory, count_of_otosag_id)
  age_structure_tally <- as.data.frame(t(age_structure_tally))
  age_structure_tally <- cbind(Species = species_list[i], age_structure_tally) #cbind one column with species name in it!!!!!
  colnames(age_structure_tally) <- col_names
  
  #bind_rows length
  length_tally_table <- rbind(length_tally_table, length_tally)
  
  #bind_rows age structure
  age_structure_tally_table <- rbind(age_structure_tally_table, age_structure_tally)
  
  print(species)
  
}

#outside for loop, write tables
#have to go back to the main directory
setwd(here::here("2026"))

readr::write_csv(length_tally_table, paste0("hkls_length_tally_table_", year, ".csv"))

readr::write_csv(age_structure_tally_table, paste0("hkls_age_structure_tally_table_", year, ".csv"))

#make main tables pretty
formatted_length <- flextable::flextable(length_tally_table)
formatted_length <- flextable::theme_vanilla(formatted_length)

#formatted_length
flextable::save_as_image(formatted_length, path = paste0("hkls_formatted_length_tally_table_", year, ".png"))

formatted_age_structure <- flextable::flextable(age_structure_tally_table)
formatted_age_structure <- flextable::theme_vanilla(formatted_age_structure)

#formatted_age_structure
flextable::save_as_image(formatted_age_structure, path = paste0("hkls_formatted_age_structure_tally_table_", year, ".png"))

#and truncate main tables and make them pretty
last_five_years <- tail(colnames(length_tally_table), 5)

last_five_years_length_tally_table <- length_tally_table[,c("Species", last_five_years)]

readr::write_csv(last_five_years_length_tally_table, paste0("hkls_last_five_years_length_tally_table_", year, ".csv"))

last_five_years_age_structure_tally_table <- age_structure_tally_table[,c("Species", last_five_years)]

readr::write_csv(last_five_years_age_structure_tally_table, paste0("hkls_last_five_years_age_structure_tally_table_", year, ".csv"))

formatted_last_five_length <- flextable::flextable(last_five_years_length_tally_table)
formatted_last_five_length <- flextable::theme_vanilla(formatted_last_five_length)

#formatted_last_five_length
flextable::save_as_image(formatted_last_five_length, path = paste0("hkls_formatted_last_five_years_length_tally_table_", year, ".png"))

formatted_last_five_age_structure <- flextable::flextable(last_five_years_age_structure_tally_table)
formatted_last_five_age_structure <- flextable::theme_vanilla(formatted_last_five_age_structure)

#formatted_last_five_age_structure
flextable::save_as_image(formatted_last_five_age_structure, path = paste0("hkls_formatted_last_five_years_age_structure_tally_table_", year, ".png"))
