#packages
library(here)
library(dplyr)
library(ggplot2)
library(cowplot)
library(stringr)
library(nwfscSurvey)
library(readr)
library(flextable)

dir <- here::here("2026", "data")

#source functions
source(here::here("R","pull_wcgbts.R"))
source(here::here("R", "get_species_list.R"))
source(here::here("R", "rename_wcgbts_species.R"))
source(here::here("R", "clean_wcgbts_catch.R"))
source(here::here("R", "clean_wcgbts_bio.R"))

#run functions
data <- pull_wcgbts(dir = dir, species = species, load = TRUE)

species <- get_species_list()

wcgbts_catch <- clean_wcgbts_catch(dir = dir, species = species, data = data)
wcgbts_bio <- clean_wcgbts_bio(dir = dir, species = species, data = data)

####################
year <- as.numeric(format(Sys.Date(), "%Y"))
data_year <- as.numeric(format(Sys.Date(), "%Y"))-1

col_names <- c("species", as.character(2003:2019), as.character(2021:data_year))
all_years <- c(as.character(2003:2019), as.character(2021:data_year))

#blank length_width_tally table (before for loop)
length_width_tally_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
colnames(length_width_tally_table) <- col_names

#blank age_structure_tally table (before for loop)
age_structure_tally_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
colnames(age_structure_tally_table) <- col_names
##########################

catch_species <- sort(unique(wcgbts_catch$Common_name)) #is blue and deacon an issue here? no positive catch and no bio
#bio_species <- sort(unique(wcgbts_bio$Common_name))

species_list <- catch_species
#for (i in 1:nrow(species_list)) {
for (i in 1:5) {
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
  
  catch <- wcgbts_catch %>%
    dplyr::filter(Common_name == species)
  
  #TABLE 0    
  table0_satisfactory <- catch %>%
    dplyr::filter(Performance == "Satisfactory") %>% #check if there are any unsatisfactory tows
    dplyr::group_by(as.character(Year)) %>%
    dplyr::summarise(satisfactory_tows = length(Trawl_id)) %>%
    dplyr::rename(year = "as.character(Year)")
  
  write.csv(table0_satisfactory, paste0(species_name, "_table0_satisfactory.csv"), row.names = FALSE)
  
  formatted_table0_satisfactory <- flextable::flextable(table0_satisfactory)
  formatted_table0_satisfactory <- flextable::theme_vanilla(formatted_table0_satisfactory)
  formatted_table0_satisfactory <- flextable::set_header_labels(formatted_table0_satisfactory,
                                                     values = list("year" = "Year",
                                                                   "satisfactory_tows" = "Sastisfactory tows"))
  
  flextable::save_as_image(formatted_table0_satisfactory, path = paste0(species_name, "_formatted_table0_satisfactory.png"))
  
  #TABLE 1   
  table1_satisfactory <- catch %>%
    dplyr::filter(Performance == "Satisfactory")%>% #check if there are any unsatisfactory tows
    dplyr::group_by(as.character(Year)) %>%
    dplyr::summarise(zero = sum(total_catch_numbers == 0),
              greater_than_zero = sum(total_catch_numbers > 0),
              one_to_five = sum(total_catch_numbers > 0 & total_catch_numbers < 6),
              six_to_ten = sum(total_catch_numbers > 5 & total_catch_numbers < 11),
              eleven_to_twenty = sum(total_catch_numbers > 10 & total_catch_numbers < 21),
              twentyone_to_thirtyfive = sum(total_catch_numbers > 20 & total_catch_numbers < 36),
              thirtysix_to_fiftyfive = sum(total_catch_numbers > 35 & total_catch_numbers < 56),
              fiftysix_to_eightyfive = sum(total_catch_numbers > 55 & total_catch_numbers < 86),
              eightysix_to_onehundredfifteen = sum(total_catch_numbers > 65 & total_catch_numbers < 116),
              greater_than_onehundredfifteen = sum(total_catch_numbers > 115)) %>%
    dplyr::rename(year = "as.character(Year)")
  
  write.csv(table1_satisfactory, paste0(species_name, "_table1_satisfactory.csv"), row.names = FALSE)
  
  formatted_table1_satisfactory <- flextable::flextable(table1_satisfactory)
  formatted_table1_satisfactory <- flextable::theme_vanilla(formatted_table1_satisfactory)
  formatted_table1_satisfactory <- flextable::set_header_labels(formatted_table1_satisfactory,
                                                     values = list("year" = "Year",
                                                                   "zero" = "Total no catch",
                                                                   "greater_than_zero" = "Total > 0",
                                                                   "one_to_five" = "1 to 5",
                                                                   "six_to_ten" = "6 to 10",
                                                                   "eleven_to_twenty" = "11 to 20",
                                                                   "twentyone_to_thirtyfive" = "21 to 35",
                                                                   "thirtysix_to_fiftyfive" = "36 to 55",
                                                                   "fiftysix_to_eightyfive" = "56 to 85",
                                                                   "eightysix_to_onehundredfifteen" = "86 to 115",
                                                                   "greater_than_onehundredfifteen" = "> 115"))
  
  flextable::save_as_image(formatted_table1_satisfactory, path = paste0(species_name, "_formatted_table1_satisfactory.png"))

bio <- wcgbts_bio %>%
  dplyr::filter(Common_name == species)

#TABLE 2
table2_satisfactory <- bio %>%
  dplyr::filter(Performance == "Satisfactory") %>% #check if there are any unsatisfactory tows
  group_by(as.character(Year)) %>%
  summarise(count_of_length_cm = sum(!is.na(Length_cm)),
            count_of_otosag_id = sum(!is.na(Otosag_id))) %>%
  rename(year = "as.character(Year)")

table2_satisfactory <- merge(data.frame("year" = all_years), table2_satisfactory, by = "year", all.x = TRUE)

#optional to have 0 instead of NA
table2_satisfactory[is.na(table2_satisfactory)] <- 0

write.csv(table2_satisfactory, paste0(species_name, "_table2_satisfactory.csv"), row.names = FALSE)

formatted_table2_satisfactory <- flextable::flextable(table2_satisfactory)
formatted_table2_satisfactory <- flextable::theme_vanilla(formatted_table2_satisfactory)
formatted_table2_satisfactory <- flextable::set_header_labels(formatted_table2_satisfactory,
                                                   values = list("year" = "Year",
                                                                 "count_of_length_cm" = "Count of length_cm",
                                                                 "count_of_otosag_id" = "Count of otosag_id"))

flextable::save_as_image(formatted_table2_satisfactory, path = paste0(species_name, "_formatted_table2_satisfactory.png"))

#add to main table
#vector from tables for length weight
length_width_tally <- dplyr::pull(table2_satisfactory, count_of_length_cm)
length_width_tally <- as.data.frame(t(length_width_tally))
length_width_tally <- cbind(species = species_list[i], length_width_tally) #cbind one column with species name in it!!!!!
colnames(length_width_tally) <- col_names

#vector from tables for age structures
age_structure_tally <- dplyr::pull(table2_satisfactory, count_of_otosag_id)
age_structure_tally <- as.data.frame(t(age_structure_tally))
age_structure_tally <- cbind(species = species_list[i], age_structure_tally) #cbind one column with species name in it!!!!!
colnames(age_structure_tally) <- col_names

#bind_rows length weight
length_width_tally_table <- rbind(length_width_tally_table, length_width_tally)

#bind_rows age structure
age_structure_tally_table <- rbind(age_structure_tally_table, age_structure_tally)

print(i)
    
}

#outside for loop, write tables
#have to go back to the main directory
setwd(base_dir)

write.csv(length_width_tally_table, paste0("length_width_tally_table_", year, ".csv"), row.names = FALSE)

write.csv(age_structure_tally_table, paste0("age_structure_tally_table_", year, ".csv"), row.names = FALSE)

#make main tables pretty
formatted_length_width <- flextable::flextable(length_width_tally_table)
formatted_length_width <- theme_vanilla(formatted_length_width)
formatted_length_width <- set_header_labels(formatted_length_width,
                                            values = list("species" = "Species"))

#formatted_length_width
save_as_image(formatted_length_width, path = paste0("formatted_length_width_tally_table_", year, ".png"))

formatted_age_structure <- flextable::flextable(age_structure_tally_table)
formatted_age_structure <- theme_vanilla(formatted_age_structure)
formatted_age_structure <- set_header_labels(formatted_age_structure,
                                             values = list("species" = "Species"))

#formatted_age_structure
save_as_image(formatted_age_structure, path = paste0("formatted_age_structure_tally_table_", year, ".png"))

#and truncate main tables and make them pretty
last_five_years <- tail(colnames(length_width_tally_table), 5)

last_five_years_length_width_tally_table <- length_width_tally_table[,c("species", last_five_years)]

write.csv(last_five_years_length_width_tally_table, paste0("last_five_years_length_width_tally_table_", year, ".csv"))

last_five_years_age_structure_tally_table <- age_structure_tally_table[,c("species", last_five_years)]

write.csv(last_five_years_age_structure_tally_table, paste0("last_five_years_age_structure_tally_table_", year, ".csv"))

formatted_last_five_length_width <- flextable::flextable(last_five_years_length_width_tally_table)
formatted_last_five_length_width <- theme_vanilla(formatted_last_five_length_width)
formatted_last_five_length_width <- set_header_labels(formatted_last_five_length_width,
                                                      values = list("species" = "Species"))

#formatted_last_five_length_width
save_as_image(formatted_last_five_length_width, path = paste0("formatted_last_five_years_length_width_tally_table_", year, ".png"))

formatted_last_five_age_structure <- flextable::flextable(last_five_years_age_structure_tally_table)
formatted_last_five_age_structure <- theme_vanilla(formatted_last_five_age_structure)
formatted_last_five_age_structure <- set_header_labels(formatted_last_five_age_structure,
                                                       values = list("species" = "Species"))

#formatted_last_five_age_structure
save_as_image(formatted_last_five_age_structure, path = "formatted_last_five_years_age_structure_tally_table_2025.png")



