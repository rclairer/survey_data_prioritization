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
#didn't seem to filter data by species list... which I guess makes sense, there is no filter in the function

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

catch_species <- sort(unique(wcgbts_catch$Common_name)) #is blue and deacon an issue here? no postive catch and no bio
bio_species <- sort(unique(wcgbts_bio$Common_name))

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
#    dplyr::filter(Performance == "Satisfactory") %>% #check if there are any unsatisfactory tows
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
#    dplyr::filter(Performance == "Satisfactory")%>% #check if there are any unsatisfactory tows
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
  
  write.csv(table1_satisfactory, paste0(species_name, "_table1.csv"), row.names = FALSE)
  
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
 
#  bio <- wcgbts_bio %>%
#    dplyr::filter(Common_name == species_name)  
  
#  bio <- bio[c("Length_cm", "Otosag_id", "Performance", "Scientific_name", "Trawl_id", "Width_cm", "Year")]
#  colnames(bio) <- c("length_cm", "otosag_id", "performance", "scientific_name", "trawl_id", "width_cm", "year") #to match bio samples
  
#  bio_samples = pull_biological_samples(
#    common_name = species_list[i],
#    survey = "NWFSC.Combo",
#    standard_filtering = FALSE)
  
#  bio_samples <- bio_samples[c("left_pectoral_fin_id", "length_cm", "otosag_id", "ovary_id", "performance", "scientific_name", "stomach_id", "tissue_id", "trawl_id", "width_cm", "year")]
  
#  bio_all_samples <- left_join(bio_all, bio_samples, by = c("length_cm", "otosag_id", "performance", "scientific_name", "trawl_id", "width_cm", "year"))
  
  
  
   
}
