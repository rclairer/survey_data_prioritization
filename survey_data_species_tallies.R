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

col_names <- c("Species", as.character(2003:2019), as.character(2021:data_year))
all_years <- c(as.character(2003:2019), as.character(2021:data_year))

#blank length_tally table (before for loop)
length_tally_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
colnames(length_tally_table) <- col_names

#blank age_structure_tally table (before for loop)
age_structure_tally_table <- data.frame(matrix(ncol = length(col_names), nrow = 0))
colnames(age_structure_tally_table) <- col_names

#blank cumulative length tally by number of tows table
tow_tally_table <- data.frame(matrix(ncol = 15, nrow = 0))
tow_tally_table_col_names <- c("Species", "Year", "Total no catch", "Total postive catch", "< 10", "10 to 19", "20 to 29", "30 to 39", "40 to 49", "50 to 59", "60 to 69", "70 to 79", "80 to 89", "90 to 99", "> 100")
colnames(tow_tally_table) <- tow_tally_table_col_names

#black average cumulative length tally by number of rows table
average_tow_tally_table <- data.frame(matrix(ncol = 15, nrow = 0))
colnames(average_tow_tally_table) <- tow_tally_table_col_names
##########################

catch_species <- sort(unique(wcgbts_catch$Common_name)) #is blue and deacon an issue here? no positive catch and no bio
#write.csv(catch_species, file.path(paste0("2026/Data/species_", year, ".csv")))
#bio_species <- sort(unique(wcgbts_bio$Common_name))

species_list <- catch_species
for (i in 1:length(species_list)) {
#for (i in 1:5) {
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
              one_to_nine = sum(total_catch_numbers > 0 & total_catch_numbers < 10),
              ten_to_nineteen = sum(total_catch_numbers > 10 & total_catch_numbers < 20),
              twenty_to_twentynine = sum(total_catch_numbers > 20 & total_catch_numbers < 30),
              thirty_to_thirtynine = sum(total_catch_numbers > 30 & total_catch_numbers < 40),
              forty_to_fortynine = sum(total_catch_numbers > 40 & total_catch_numbers < 50),
              fifty_to_fiftynine = sum(total_catch_numbers > 50 & total_catch_numbers < 60),
              sixty_to_sixtynine = sum(total_catch_numbers > 60 & total_catch_numbers < 70),
              seventy_to_seventynine = sum(total_catch_numbers > 70 & total_catch_numbers < 80),
              eighty_to_eightynine = sum(total_catch_numbers > 80 & total_catch_numbers < 90),
              ninety_to_ninetynine = sum(total_catch_numbers > 90 & total_catch_numbers < 100), #includes 99
              onehundred_and_greater = sum(total_catch_numbers > 99)) %>% #one hundred and greater
    dplyr::rename(year = "as.character(Year)") %>%
    dplyr::rowwise() %>%
    dplyr::mutate(average_across_years = mean(c_across(2:ncol(table1_satisfactory)))) %>%
    dplyr::ungroup()
  
  colnames(table1_satisfactory) <- tow_tally_table_col_names
  
  write.csv(table1_satisfactory, paste0(species_name, "_table1_satisfactory.csv"), row.names = FALSE)
  
  formatted_table1_satisfactory <- flextable::flextable(table1_satisfactory)
  formatted_table1_satisfactory <- flextable::theme_vanilla(formatted_table1_satisfactory)
  formatted_table1_satisfactory <- flextable::set_header_labels(formatted_table1_satisfactory,
                                                                values = list("average_across_years" = "Average across years"))

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

#cumulative length tally by number of tows
tow_tally <- table1_satisfactory %>%
                dplyr::filter(Year == 2025)
tow_tally <- cbind(Species = species_list[i], tow_tally)
colnames(tow_tally) <- tow_tally_table_col_names

average_tow_tally <- table1_satisfactory %>%
  dplyr::filter(Year == "Average across years")
average_tow_tally <- cbind(Species = species_list[i], average_tow_tally)
colnames(average_tow_tally) <- average_tow_tally_table_col_names

#bind_rows length
length_tally_table <- rbind(length_tally_table, length_tally)

#bind_rows age structure
age_structure_tally_table <- rbind(age_structure_tally_table, age_structure_tally)

#bind rows tow
tow_tally_table <- rbind(tow_tally_table, tow_tally)

#bind average rows tow
average_tow_tally_table <- rbind(average_tow_tally_table, average_tow_tally)

print(species)
    
}

#outside for loop, write tables
#have to go back to the main directory
setwd(here::here("2026"))

write.csv(length_tally_table, paste0("length_tally_table_", year, ".csv"), row.names = FALSE)

write.csv(age_structure_tally_table, paste0("age_structure_tally_table_", year, ".csv"), row.names = FALSE)

#make main tables pretty
formatted_length <- flextable::flextable(length_tally_table)
formatted_length <- flextable::theme_vanilla(formatted_length)

#formatted_length
flextable::save_as_image(formatted_length, path = paste0("formatted_length_tally_table_", year, ".png"))

formatted_age_structure <- flextable::flextable(age_structure_tally_table)
formatted_age_structure <- flextable::theme_vanilla(formatted_age_structure)

#formatted_age_structure
flextable::save_as_image(formatted_age_structure, path = paste0("formatted_age_structure_tally_table_", year, ".png"))

#and truncate main tables and make them pretty
last_five_years <- tail(colnames(length_tally_table), 5)

last_five_years_length_tally_table <- length_tally_table[,c("Species", last_five_years)]

write.csv(last_five_years_length_tally_table, paste0("last_five_years_length_tally_table_", year, ".csv"))

last_five_years_age_structure_tally_table <- age_structure_tally_table[,c("Species", last_five_years)]

write.csv(last_five_years_age_structure_tally_table, paste0("last_five_years_age_structure_tally_table_", year, ".csv"))

formatted_last_five_length <- flextable::flextable(last_five_years_length_tally_table)
formatted_last_five_length <- flextable::theme_vanilla(formatted_last_five_length)

#formatted_last_five_length
flextable::save_as_image(formatted_last_five_length, path = paste0("formatted_last_five_years_length_tally_table_", year, ".png"))

formatted_last_five_age_structure <- flextable::flextable(last_five_years_age_structure_tally_table)
formatted_last_five_age_structure <- flextable::theme_vanilla(formatted_last_five_age_structure)

#formatted_last_five_age_structure
flextable::save_as_image(formatted_last_five_age_structure, path = paste0("formatted_last_five_years_age_structure_tally_table", year, ".png"))

tow_tally_table <- tow_tally_table[,-2] #minus year
write.csv(tow_tally_table, paste0("tow_tally_table_", year, ".csv"))

formatted_tow_tally_table <- flextable::flextable(tow_tally_table)
formatted_tow_tally_table <- flextable::theme_vanilla(formatted_tow_tally_table)

#formatted_tow_tally
flextable::save_as_image(formatted_tow_tally_table, path = paste0("formatted_tow_tally_table", year, ".png"))

average_tow_tally_table <- average_tow_tally_table[,-2] #minus year
write.csv(average_tow_tally_table, paste0("average_across_years_tow_tally_table_", year, ".csv"))

formatted_average_tow_tally_table <- flextable::flextable(average_tow_tally_table)
formatted_average_tow_tally_table <- flextable::theme_vanilla(formatted_average_tow_tally_table)

#formatted_tow_tally
flextable::save_as_image(formatted_average_tow_tally_table, path = paste0("formatted_average_across_years_tow_tally_table", year, ".png"))

