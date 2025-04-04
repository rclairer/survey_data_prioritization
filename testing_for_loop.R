#MAKE AARON'S TABLES IN A FOR LOOP

#to do this for all species, I will need a configuration file for common_name and I think that's it!! and I will need to create a new folder for each species
library(dplyr)
library(nwfscSurvey)
library(flextable)

base_dir <- "~/GitHub/survey_data_prioritization"
setwd(base_dir)

species_list <- read.csv("species_list_2025.csv")

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

##############################

for (i in 1:nrow(species_list)) {

#for (i in 1:10) {
  species_name <- species_list$species[i]
  species_name <- gsub(" ", "_", species_name)
  
  #here create folder for each species
  folder_name <- paste0("Data_", species_name)
  folder_path <- file.path(base_dir, folder_name)
  #create the folder if it does not exist
  if(!dir.exists(folder_path)){
    dir.create(folder_path, recursive = TRUE)
  }
  
  setwd(folder_path)
  
  catch_all_test <- tryCatch({
    
    message("Trying pull_catch() with standard_filtering = FALSE")
    
    catch_all <- pull_catch(
      common_name = species_list$species[i],
      survey = "NWFSC.Combo",
      standard_filtering = FALSE)

    #to test i <- 46 (pink rockfish)    
    
    ############################################# 
    #########    SATISFACTORY   ################# 
    #############################################
    
    #TABLE 0    
    table0_satisfactory <- catch_all %>%
      filter(Performance == "Satisfactory") %>%
      group_by(as.character(Year)) %>%
      summarise(satisfactory_tows = length(Trawl_id)) %>%
      rename(year = "as.character(Year)")
    
    write.csv(table0_satisfactory, paste0(species_name, "_table0_satisfactory.csv"), row.names = FALSE)
    
    formatted_table0_satisfactory <- flextable::flextable(table0_satisfactory)
    formatted_table0_satisfactory <- theme_vanilla(formatted_table0_satisfactory)
    formatted_table0_satisfactory <- set_header_labels(formatted_table0_satisfactory,
                                                       values = list("year" = "Year",
                                                                     "satisfactory_tows" = "Satisfactory tows"))
    
    save_as_image(formatted_table0_satisfactory, path = paste0(species_name, "_formatted_table0_satisfactory.png"))
    
    #TABLE 1   
    table1_satisfactory <- catch_all %>%
      filter(Performance == "Satisfactory")%>%
      group_by(as.character(Year)) %>%
      summarise(zero = sum(total_catch_numbers == 0),
                greater_than_zero = sum(total_catch_numbers > 0),
                one_to_five = sum(total_catch_numbers > 0 & total_catch_numbers < 6),
                six_to_ten = sum(total_catch_numbers > 5 & total_catch_numbers < 11),
                eleven_to_twenty = sum(total_catch_numbers > 10 & total_catch_numbers < 21),
                twentyone_to_thirtyfive = sum(total_catch_numbers > 20 & total_catch_numbers < 36),
                thirtysix_to_fiftyfive = sum(total_catch_numbers > 35 & total_catch_numbers < 56),
                fiftysix_to_eightyfive = sum(total_catch_numbers > 55 & total_catch_numbers < 86),
                eightysix_to_onehundredfifteen = sum(total_catch_numbers > 65 & total_catch_numbers < 116),
                greater_than_onehundredfifteen = sum(total_catch_numbers > 115)) %>%
      rename(year = "as.character(Year)")
    
    write.csv(table1_satisfactory, paste0(species_name, "_table1.csv"), row.names = FALSE)
    
    formatted_table1_satisfactory <- flextable::flextable(table1_satisfactory)
    formatted_table1_satisfactory <- theme_vanilla(formatted_table1_satisfactory)
    formatted_table1_satisfactory <- set_header_labels(formatted_table1_satisfactory,
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
    
    save_as_image(formatted_table1_satisfactory, path = paste0(species_name, "_formatted_table1_satisfactory.png"))
    
    
    ############################################# 
    #########    UNSATISFACTORY   ################# 
    #############################################
    
    #TABLE 0    
    table0_unsatisfactory <- catch_all %>%
      filter(Performance == "Unsatisfactory") %>%
      group_by(as.character(Year)) %>%
      summarise(unsatisfactory_tows = length(Trawl_id)) %>%
      rename(year = "as.character(Year)")
    
    write.csv(table0_unsatisfactory, paste0(species_name, "_table0_unsatisfactory.csv"), row.names = FALSE)
    
    formatted_table0_unsatisfactory <- flextable::flextable(table0_unsatisfactory)
    formatted_table0_unsatisfactory <- theme_vanilla(formatted_table0_unsatisfactory)
    formatted_table0_unsatisfactory <- set_header_labels(formatted_table0_unsatisfactory,
                                                         values = list("year" = "Year",
                                                                       "unsatisfactory_tows" = "Unsatisfactory tows"))
    
    save_as_image(formatted_table0_unsatisfactory, path = paste0(species_name, "_formatted_table0_unsatisfactory.png"))
    
    #TABLE 1   
    table1_unsatisfactory <- catch_all %>%
      filter(Performance == "Unsatisfactory")%>%
      group_by(as.character(Year)) %>%
      summarise(zero = sum(total_catch_numbers == 0),
                greater_than_zero = sum(total_catch_numbers > 0),
                one_to_five = sum(total_catch_numbers > 0 & total_catch_numbers < 6),
                six_to_ten = sum(total_catch_numbers > 5 & total_catch_numbers < 11),
                eleven_to_twenty = sum(total_catch_numbers > 10 & total_catch_numbers < 21),
                twentyone_to_thirtyfive = sum(total_catch_numbers > 20 & total_catch_numbers < 36),
                thirtysix_to_fiftyfive = sum(total_catch_numbers > 35 & total_catch_numbers < 56),
                fiftysix_to_eightyfive = sum(total_catch_numbers > 55 & total_catch_numbers < 86),
                eightysix_to_onehundredfifteen = sum(total_catch_numbers > 65 & total_catch_numbers < 116),
                greater_than_onehundredfifteen = sum(total_catch_numbers > 115)) %>%
      rename(year = "as.character(Year)")
    
    write.csv(table1_unsatisfactory, paste0(species_name, "_table1.csv"), row.names = FALSE)
    
    formatted_table1_unsatisfactory <- flextable::flextable(table1_unsatisfactory)
    formatted_table1_unsatisfactory <- theme_vanilla(formatted_table1_unsatisfactory)
    formatted_table1_unsatisfactory <- set_header_labels(formatted_table1_unsatisfactory,
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
    
    save_as_image(formatted_table1_unsatisfactory, path = paste0(species_name, "_formatted_table1_unsatisfactory.png"))
    
    
    #############################################################
    ############### ONLY EVER CAUGHT ON SATISFACTORY TOWS ########
    ##################################################################
    
  }, error = function(e) {
    message(paste("Pull_catch with standard_filtering = FALSE resulted in an error for i =", i, "so using standard_filtering = TRUE"))
    
    catch_all <- pull_catch(
      common_name = species_list$species[i],
      survey = "NWFSC.Combo",
      standard_filtering = TRUE)
    
    #TABLE 0 
    table0_satisfactory <- catch_all %>%
      filter(Performance == "Satisfactory") %>%
      group_by(as.character(Year)) %>%
      summarise(satisfactory_tows = length(Trawl_id)) %>%
      rename(year = "as.character(Year)")
    
    write.csv(table0_satisfactory, paste0(species_name, "_table0_satisfactory.csv"), row.names = FALSE)
    
    formatted_table0_satisfactory <- flextable::flextable(table0_satisfactory)
    formatted_table0_satisfactory <- theme_vanilla(formatted_table0_satisfactory)
    formatted_table0_satisfactory <- set_header_labels(formatted_table0_satisfactory,
                                                       values = list("year" = "Year",
                                                                     "satisfactory_tows" = "Satisfactory tows"))
    
    save_as_image(formatted_table0_satisfactory, path = paste0(species_name, "_formatted_table0_satisfactory.png"))
    
    #TABLE 1   
    table1_satisfactory <- catch_all %>%
      filter(Performance == "Satisfactory")%>%
      group_by(as.character(Year)) %>%
      summarise(zero = sum(total_catch_numbers == 0),
                greater_than_zero = sum(total_catch_numbers > 0),
                one_to_five = sum(total_catch_numbers > 0 & total_catch_numbers < 6),
                six_to_ten = sum(total_catch_numbers > 5 & total_catch_numbers < 11),
                eleven_to_twenty = sum(total_catch_numbers > 10 & total_catch_numbers < 21),
                twentyone_to_thirtyfive = sum(total_catch_numbers > 20 & total_catch_numbers < 36),
                thirtysix_to_fiftyfive = sum(total_catch_numbers > 35 & total_catch_numbers < 56),
                fiftysix_to_eightyfive = sum(total_catch_numbers > 55 & total_catch_numbers < 86),
                eightysix_to_onehundredfifteen = sum(total_catch_numbers > 65 & total_catch_numbers < 116),
                greater_than_onehundredfifteen = sum(total_catch_numbers > 115)) %>%
      rename(year = "as.character(Year)")
    
    write.csv(table1_satisfactory, paste0(species_name, "_table1.csv"), row.names = FALSE)
    
    formatted_table1_satisfactory <- flextable::flextable(table1_satisfactory)
    formatted_table1_satisfactory <- theme_vanilla(formatted_table1_satisfactory)
    formatted_table1_satisfactory <- set_header_labels(formatted_table1_satisfactory,
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
    
    save_as_image(formatted_table1_satisfactory, path = paste0(species_name, "_formatted_table1_satisfactory.png"))
    
    
    ########################################################
    #   NO CATCH ON UNSATISFACTORY HAULS!!!!!!!!!!!!!!!!!!!!
    #########################################################   
    
    table0_unsatisfactory <- table0_satisfactory
    colnames(table0_unsatisfactory) <- c("year", "unsatisfactory_tows")
    table0_unsatisfactory$unsatisfactory_tows <- NA
    
    table1_unsatisfactory <- table1_satisfactory
    table1_unsatisfactory[,2:ncol(table1_unsatisfactory)] <- NA
    
  })
  
  for (i in 1:nrow(species_list)) { 
  
  bio_all_test <- tryCatch({
    
    bio_all <- pull_bio(
      common_name = species_list$species[i],
      survey = "NWFSC.Combo",
      standard_filtering = FALSE)
    
    bio_all <- bio_all[c("Length_cm", "Otosag_id", "Performance", "Scientific_name", "Trawl_id", "Width_cm", "Year")]
    colnames(bio_all) <- c("length_cm", "otosag_id", "performance", "scientific_name", "trawl_id", "width_cm", "year") #to match bio samples
    
    bio_samples = pull_biological_samples(
      common_name = species_list$species[i],
      survey = "NWFSC.Combo",
      standard_filtering = FALSE)
    
    bio_samples <- bio_samples[c("left_pectoral_fin_id", "length_cm", "otosag_id", "ovary_id", "performance", "scientific_name", "stomach_id", "tissue_id", "trawl_id", "width_cm", "year")]
    
    bio_all_samples <- left_join(bio_all, bio_samples, by = c("length_cm", "otosag_id", "performance", "scientific_name", "trawl_id", "width_cm", "year"))
    
    
    ############################################# 
    #########    SATISFACTORY   ################# 
    #############################################
    
    
    #TABLE 2
    table2_satisfactory <- bio_all_samples %>%
      filter(performance == "Satisfactory") %>%
      group_by(as.character(year)) %>%
      summarise(count_of_length_cm = sum(!is.na(length_cm)),
                count_of_otosag_id = sum(!is.na(otosag_id)),
                count_of_ovary_id = sum(!is.na(ovary_id)),
                count_of_stomach_id = sum(!is.na(stomach_id)),
                count_of_tissue_id = sum(!is.na(tissue_id)),
                count_of_left_pectoral_fin_id = sum(!is.na(left_pectoral_fin_id))) %>%
      rename(year = "as.character(year)")
    
    table2_satisfactory <- merge(data.frame("year" = all_years), table2_satisfactory, by = "year", all.x = TRUE)
    
    #optional to have 0 instead of NA
    #table2[is.na(table2)] <- 0
    
    write.csv(table2_satisfactory, paste0(species_name, "_table2_satisfactory.csv"), row.names = FALSE)
    
    formatted_table2_satisfactory <- flextable::flextable(table2_satisfactory)
    formatted_table2_satisfactory <- theme_vanilla(formatted_table2_satisfactory)
    formatted_table2_satisfactory <- set_header_labels(formatted_table2_satisfactory,
                                                       values = list("year" = "Year",
                                                                     "count_of_length_cm" = "Count of length_cm",
                                                                     "count_of_otosag_id" = "Count of otosag_id",
                                                                     "count_of_ovary_id" = "Count of ovary_id",
                                                                     "count_of_stomach_id" = "Count of stomach_id",
                                                                     "count_of_tissue_id" = "Count of tissue_id",
                                                                     "count_of_left_pectoral_fin_id" = "Count of left_pectoral_fin_id"))
    
    save_as_image(formatted_table2_satisfactory, path = paste0(species_name, "_formatted_table2_satisfactory.png"))
    
    #TABLE 3    
    table3_satisfactory <- bio_all_samples %>%
      filter(performance == "Satisfactory") %>%
      group_by(trawl_id) %>%
      summarise(count_of_length_cm = sum(!is.na(length_cm)),
                count_of_otosag_id = sum(!is.na(otosag_id)),
                count_of_ovary_id = sum(!is.na(ovary_id)),
                count_of_stomach_id = sum(!is.na(stomach_id)),
                count_of_tissue_id = sum(!is.na(tissue_id)),
                count_of_left_pectoral_fin_id = sum(!is.na(left_pectoral_fin_id)))
    
    write.csv(table3_satisfactory, paste0(species_name, "_table3_satisfactory.csv"), row.names = FALSE)
    
    #table is too big to format
    #formatted_table3_satisfactory <- flextable::flextable(table3_satisfactory)
    #formatted_table3_satisfactory <- theme_vanilla(formatted_table3_satisfactory)
    #formatted_table3_satisfactory <- set_header_labels(formatted_table3_satisfactory,
    #                                      values = list("trawl_id" = "Trawl ID",
    #                                                    "count_of_length_cm" = "Count of length_cm",
    #                                                    "count_of_otosag_id" = "Count of otosag_id",
    #                                                    "count_of_ovary_id" = "Count of ovary_id",
    #                                                    "count_of_stomach_id" = "Count of stomach_id",
    #                                                    "count_of_tissue_id" = "Count of tissue_id",
    #                                                    "count_of_left_pectoral_fin_id" = "Count of left_pectoral_fin_id"))
    
    
    #save_as_image(formatted_table3_satisfactory, path = paste0(species_name, "_formatted_table3_satisfactory.png"))
    
    ############################################# 
    #########    UNSATISFACTORY   ################# 
    #############################################
    
    #TABLE 2
    table2_unsatisfactory <- bio_all_samples %>%
      filter(performance == "Unsatisfactory") %>%
      group_by(as.character(year)) %>%
      summarise(count_of_length_cm = sum(!is.na(length_cm)),
                count_of_otosag_id = sum(!is.na(otosag_id)),
                count_of_ovary_id = sum(!is.na(ovary_id)),
                count_of_stomach_id = sum(!is.na(stomach_id)),
                count_of_tissue_id = sum(!is.na(tissue_id)),
                count_of_left_pectoral_fin_id = sum(!is.na(left_pectoral_fin_id))) %>%
      rename(year = "as.character(year)")
    
    table2_unsatisfactory <- merge(data.frame("year" = all_years), table2_unsatisfactory, by = "year", all.x = TRUE)
    
    #optional to have 0 instead of NA
    #table2[is.na(table2)] <- 0
    
    write.csv(table2_unsatisfactory, paste0(species_name, "_table2_unsatisfactory.csv"), row.names = FALSE)
    
    formatted_table2_unsatisfactory <- flextable::flextable(table2_unsatisfactory)
    formatted_table2_unsatisfactory <- theme_vanilla(formatted_table2_unsatisfactory)
    formatted_table2_unsatisfactory <- set_header_labels(formatted_table2_unsatisfactory,
                                                         values = list("year" = "Year",
                                                                       "count_of_length_cm" = "Count of length_cm",
                                                                       "count_of_otosag_id" = "Count of otosag_id",
                                                                       "count_of_ovary_id" = "Count of ovary_id",
                                                                       "count_of_stomach_id" = "Count of stomach_id",
                                                                       "count_of_tissue_id" = "Count of tissue_id",
                                                                       "count_of_left_pectoral_fin_id" = "Count of left_pectoral_fin_id"))
    
    save_as_image(formatted_table2_unsatisfactory, path = paste0(species_name, "_formatted_table2_unsatisfactory.png"))
    
    #TABLE 3    
    table3_unsatisfactory <- bio_all_samples %>%
      filter(performance == "Unsatisfactory") %>%
      group_by(trawl_id) %>%
      summarise(count_of_length_cm = sum(!is.na(length_cm)),
                count_of_otosag_id = sum(!is.na(otosag_id)),
                count_of_ovary_id = sum(!is.na(ovary_id)),
                count_of_stomach_id = sum(!is.na(stomach_id)),
                count_of_tissue_id = sum(!is.na(tissue_id)),
                count_of_left_pectoral_fin_id = sum(!is.na(left_pectoral_fin_id)))
    
    write.csv(table3_unsatisfactory, paste0(species_name, "_table3_unsatisfactory.csv"), row.names = FALSE)
    
    #table is too big to format
    #formatted_table3_unsatisfactory <- flextable::flextable(table3_unsatisfactory)
    #formatted_table3_unsatisfactory <- theme_vanilla(formatted_table3_unsatisfactory)
    #formatted_table3_unsatisfactory <- set_header_labels(formatted_table3_unsatisfactory,
    #                                      values = list("trawl_id" = "Trawl ID",
    #                                                    "count_of_length_cm" = "Count of length_cm",
    #                                                    "count_of_otosag_id" = "Count of otosag_id",
    #                                                    "count_of_ovary_id" = "Count of ovary_id",
    #                                                    "count_of_stomach_id" = "Count of stomach_id",
    #                                                    "count_of_tissue_id" = "Count of tissue_id",
    #                                                    "count_of_left_pectoral_fin_id" = "Count of left_pectoral_fin_id"))
    
    
    #save_as_image(formatted_table3_unsatisfactory, path = paste0(species_name, "_formatted_table3_unsatisfactory.png"))
    
    
    
    
    
    
  }, error = function(e) {
    message(paste("Pull_bio() error for i =", i, "Using filtering = TRUE")) 
    
    bio_all <- pull_bio(
      common_name = species_list$species[i],
      survey = "NWFSC.Combo",
      standard_filtering = TRUE)
    
    bio_all <- bio_all[c("Length_cm", "Otosag_id", "Performance", "Scientific_name", "Trawl_id", "Width_cm", "Year")]
    colnames(bio_all) <- c("length_cm", "otosag_id", "performance", "scientific_name", "trawl_id", "width_cm", "year") #to match bio samples
    
    bio_samples = pull_biological_samples(
      common_name = species_list$species[i],
      survey = "NWFSC.Combo",
      standard_filtering = TRUE)
    
    bio_samples <- bio_samples[c("left_pectoral_fin_id", "length_cm", "otosag_id", "ovary_id", "performance", "scientific_name", "stomach_id", "tissue_id", "trawl_id", "width_cm", "year")]
    
    bio_all_samples <- left_join(bio_all, bio_samples, by = c("length_cm", "otosag_id", "performance", "scientific_name", "trawl_id", "width_cm", "year"))
    
    #TABLE 2
    table2_satisfactory <- bio_all_samples %>%
      filter(performance == "Satisfactory") %>%
      group_by(as.character(year)) %>%
      summarise(count_of_length_cm = sum(!is.na(length_cm)),
                count_of_otosag_id = sum(!is.na(otosag_id)),
                count_of_ovary_id = sum(!is.na(ovary_id)),
                count_of_stomach_id = sum(!is.na(stomach_id)),
                count_of_tissue_id = sum(!is.na(tissue_id)),
                count_of_left_pectoral_fin_id = sum(!is.na(left_pectoral_fin_id))) %>%
      rename(year = "as.character(year)")
    
    table2_satisfactory <- merge(data.frame("year" = all_years), table2_satisfactory, by = "year", all.x = TRUE)
    
    #optional to have 0 instead of NA
    #table2[is.na(table2)] <- 0
    
    write.csv(table2_satisfactory, paste0(species_name, "_table2_satisfactory.csv"), row.names = FALSE)
    
    formatted_table2_satisfactory <- flextable::flextable(table2_satisfactory)
    formatted_table2_satisfactory <- theme_vanilla(formatted_table2_satisfactory)
    formatted_table2_satisfactory <- set_header_labels(formatted_table2_satisfactory,
                                                       values = list("year" = "Year",
                                                                     "count_of_length_cm" = "Count of length_cm",
                                                                     "count_of_otosag_id" = "Count of otosag_id",
                                                                     "count_of_ovary_id" = "Count of ovary_id",
                                                                     "count_of_stomach_id" = "Count of stomach_id",
                                                                     "count_of_tissue_id" = "Count of tissue_id",
                                                                     "count_of_left_pectoral_fin_id" = "Count of left_pectoral_fin_id"))
    
    save_as_image(formatted_table2_satisfactory, path = paste0(species_name, "_formatted_table2_satisfactory.png"))
    
    #TABLE 3    
    table3_satisfactory <- bio_all_samples %>%
      filter(performance == "Satisfactory") %>%
      group_by(trawl_id) %>%
      summarise(count_of_length_cm = sum(!is.na(length_cm)),
                count_of_otosag_id = sum(!is.na(otosag_id)),
                count_of_ovary_id = sum(!is.na(ovary_id)),
                count_of_stomach_id = sum(!is.na(stomach_id)),
                count_of_tissue_id = sum(!is.na(tissue_id)),
                count_of_left_pectoral_fin_id = sum(!is.na(left_pectoral_fin_id)))
    
    write.csv(table3_satisfactory, paste0(species_name, "_table3_satisfactory.csv"), row.names = FALSE)
    
    #table is too big to format
    #formatted_table3_satisfactory <- flextable::flextable(table3_satisfactory)
    #formatted_table3_satisfactory <- theme_vanilla(formatted_table3_satisfactory)
    #formatted_table3_satisfactory <- set_header_labels(formatted_table3_satisfactory,
    #                                      values = list("trawl_id" = "Trawl ID",
    #                                                    "count_of_length_cm" = "Count of length_cm",
    #                                                    "count_of_otosag_id" = "Count of otosag_id",
    #                                                    "count_of_ovary_id" = "Count of ovary_id",
    #                                                    "count_of_stomach_id" = "Count of stomach_id",
    #                                                    "count_of_tissue_id" = "Count of tissue_id",
    #                                                    "count_of_left_pectoral_fin_id" = "Count of left_pectoral_fin_id"))
    
    
    #save_as_image(formatted_table3_satisfactory, path = paste0(species_name, "_formatted_table3_satisfactory.png"))
    
    
    ########################################################
    #   NO CATCH ON UNSATISFACTORY HAULS!!!!!!!!!!!!!!!!!!!!
    #########################################################   
    
    
  })
  
   
  
  
  
  
  print(i)
  
}

