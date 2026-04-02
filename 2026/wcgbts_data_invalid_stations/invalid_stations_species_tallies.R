library(dplyr)
library(flextable)

#focus only on satisfactory hauls
bio_data_invalid_satisfactory <- dplyr::filter(bio_data_invalid, Performance == "Satisfactory")

catch_data_invalid_satisfactory <- dplyr::filter(catch_data_invalid, Performance == "Satisfactory")

#why are stations deemed invalid?
reason_invalid <- dplyr::summarise(
  dplyr::group_by(bio_data_invalid_satisfactory, Reason_station_invalid),
  n_tows = dplyr::n_distinct(Trawl_id)
)
#flex table

formatted_reason_invalid <- flextable::flextable(reason_invalid) |>
  flextable::theme_vanilla() |>
  flextable::set_header_labels(values = list("Reason_station_invalid" = "Reason invalid", "n_tows" = "Number of tows")) |>
  flextable::autofit()

flextable::save_as_image(formatted_reason_invalid, path = here::here("2026","wcgbts_data_invalid_stations","reason_invalid.png"))


#how many tows at invalid stations have satisfactory data?
#bio
unique(as.factor(bio_data_invalid_satisfactory$Trawl_id))
#150

invalid_tows_per_year <-  dplyr::summarise(
  dplyr::group_by(bio_data_invalid_satisfactory, Year),
  n_tows = dplyr::n_distinct(Trawl_id)
)
#catch
unique(as.factor(catch_data_invalid_satisfactory$Trawl_id))
#154
dplyr::summarise(
  dplyr::group_by(catch_data_invalid_satisfactory, Year),
  n_tows = dplyr::n_distinct(Trawl_id)
)

invalid_tows_per_year$Year <- as.character(invalid_tows_per_year$Year)
#flextable
formatted_invalid_tows_per_year <- flextable::flextable(invalid_tows_per_year) |>
    flextable::theme_vanilla() |>
    flextable::set_header_labels(values = list("Reason_station_invalid" = "Reason invalid", "n_tows" = "Number of tows at invalid stations")) |>
  flextable::autofit()

flextable::save_as_image(formatted_invalid_tows_per_year, path = here::here("2026","wcgbts_data_invalid_stations","invalid_tows_per_year.png"))

#what data are we missing?

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

##########################

catch_species <- sort(unique(bio_data_invalid_satisfactory$Common_name))

species_list <- catch_species
for (i in 1:length(species_list)) {
  #for (i in 1:5) {
  species <- species_list[i]
  species_name <- gsub(" ", "_", species)
  
  bio <- bio_data_invalid_satisfactory %>%
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
setwd(here::here("2026", "wcgbts_data_invalid_stations"))

readr::write_csv(length_tally_table, "wcgbts_length_tally_table_invalid_stations.csv")

readr::write_csv(age_structure_tally_table, "wcgbts_age_structure_tally_table_invalid_stations.csv")

#make main tables pretty
formatted_length <- flextable::flextable(length_tally_table) |>
  flextable::theme_vanilla()

#formatted_length
flextable::save_as_image(formatted_length, path = here::here("2026","wcgbts_data_invalid_stations","length_tally_table_invalid_stations.png"))

formatted_age_structure <- flextable::flextable(age_structure_tally_table) |>
  flextable::theme_vanilla()

#formatted_age_structure
flextable::save_as_image(formatted_age_structure, path = here::here("2026","wcgbts_data_invalid_stations","age_structure_tally_table_invalid_stations.png"))

