library(dplyr)
library(nwfscSurvey)
library(flextable)
library(ggplot2)

species_list <- read.csv("species_list_2025.csv")

species_name <- species_list$species[i]
species_name <- gsub(" ", "_", species_name)


bio_all <- pull_bio(
  common_name = species_list$species[i],
  #common_name = c("rougheye rockfish", "blackspotted rockfish", "rougheye and blackspotted rockfish"),
  survey = "NWFSC.Combo",
  standard_filtering = FALSE)

# Create histogram with binwidth of 1 cm
p <- ggplot(bio_all, aes(x = Length_cm)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 1, fill = "black", alpha = 0.1) +
  #scale_y_continuous(limits = c(0, 0.05)) +
  #scale_x_continuous(limits = c(0, 90))+
  labs(title = paste0(species_name),
       x = "Length (cm)",
       y = "Frequency") +
  theme_classic()

# Display the plot
print(p)


#standardize them all to one and plot on eachother
#also could tally all observations ever in the length bins and plot that. that would be crazy. and harder to do. this also wouldn't account for the fact that some speceis are just larger than others


# Save as PNG
ggsave("length_histogram.png", plot = p, width = 8, height = 6, dpi = 300)


#blackgill! sablefish! Yelloweye! Widow! need to try rougheye 


length_all_species <- data.frame(matrix(ncol = 2, nrow = 0))
colnames(length_all_species) <- c("Common_name", "Length_cm")

#for (i in 1:nrow(species_list)) {
  
  for (i in 1:10) {
  #species_name <- species_list$species[i]
  #species_name <- gsub(" ", "_", species_name)
  
  bio_all <- pull_bio(
    common_name = species_list$species[i],
    #common_name = c("rougheye rockfish", "blackspotted rockfish", "rougheye and blackspotted rockfish"),
    survey = "NWFSC.Combo",
    standard_filtering = FALSE)
  
  bio_length <- bio_all[,c("Common_name", "Length_cm")]

  length_all_species <- rbind(length_all_species, bio_length)
  
  print(i)
  
}

p <- ggplot(length_all_species, aes(x = Length_cm)) +
  geom_histogram(aes(y = after_stat(density), fill = Common_name), binwidth = 1,
                 alpha = 0.1, position = "identity") +
  scale_fill_manual(values = rep("grey", length(unique(length_all_species$Common_name)))) +
  labs(#title = "Overlaid Length Frequency Histogram",
       x = "Length (cm)",
       y = "Density") +
  theme_classic()

print(p)

###############################

# Bin width
bin_width <- 1

# Compute histogram density data for each species
hist_data <- length_all_species %>%
  ggplot(aes(x = Length_cm, fill = Common_name)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = bin_width, position = "identity", alpha = 0.1, color = NA) +
  scale_fill_manual(values = rep("grey", length(unique(length_all_species$Common_name)))) +
  theme_classic()

# Get the computed data from the ggplot object (density values for each bin)
computed_data <- ggplot_build(hist_data)$data[[1]]

# Find the max density across all groups
max_density <- max(computed_data$density)

# Normalize the densities so the max density is 1
computed_data <- computed_data %>%
  mutate(density_normalized = density / max_density)

# Plot the normalized histograms
p_normalized <- ggplot(computed_data, aes(x = xmin + (xmax - xmin) / 2, y = density_normalized, fill = group)) +
  geom_col(width = bin_width, alpha = 0.5, position = "identity", color = NA) +
  scale_fill_manual(values = rep("grey", length(unique(computed_data$group)))) +
  labs(title = "Normalized Length Frequency Histogram (Max Density = 1)",
       x = "Length (cm)",
       y = "Density (Normalized)") +
  theme_classic()

# Display the normalized plot
print(p_normalized)



density_data_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(density_data_all) <- c("Length_cm", "standardized_density", "Common_name")

######################################
for (i in 1:10) {
  species_name <- species_list$species[i]
  species_name <- gsub(" ", "_", species_name)
  
  bio_all <- pull_bio(
    common_name = species_list$species[i],
    #common_name = c("rougheye rockfish", "blackspotted rockfish", "rougheye and blackspotted rockfish"),
    survey = "NWFSC.Combo",
    standard_filtering = FALSE)
  
  bio_length <- bio_all[,c("Common_name", "Length_cm")]
  
  p <- ggplot(bio_length, aes(x = Length_cm)) +
    geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.1, color = "black", fill = "grey") +
    theme_classic()
  
  # Extract the computed histogram data
  computed_data <- ggplot_build(p)$data[[1]]
  
  # Identify the maximum density for this species
  max_density <- max(computed_data$density)
  
  # Standardize the density for this species by dividing by its max density
  computed_data$density_standardized <- computed_data$density / max_density
  
  density_data <- computed_data[,c("x", "density_standardized")]
  density_data$Common_name <- paste0(species_name)
  
  colnames(density_data) <- c("Length_cm", "standardized_density", "Common_name")
  
  density_data_all <- rbind(density_data_all, density_data)
  
 
  print(i)
  
}

p_overlay <- ggplot(density_data_all, aes(x = Length_cm, y = standardized_density, fill = Common_name)) +
  geom_col(width = bin_width, alpha = 0.5, position = "identity") +
  scale_fill_manual(values = rep("grey", length(unique(density_data_all$Common_name)))) +
  labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 per Species)",
       x = "Length (cm)",
       y = "Density (Standardized)") +
  theme_classic()

print(p_overlay)





p_standardized <- ggplot(density_data_all, aes(x = xmin + (xmax - xmin) / 2, y = density_standardized)) +
  geom_col(width = bin_width, alpha = 0.5, position = "identity", fill = "grey") +
  labs(title = paste("Standardized Length Frequency Histogram (Max Density = 1 for", species_name, ")"),
       x = "Length (cm)",
       y = "Density (Standardized)") +
  theme_classic()



#####################################################
density_data_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(density_data_all) <- c("Length_cm", "standardized_density", "Common_name")

for (i in 1:10) {
  species_name <- species_list$species[i]
  species_name_clean <- gsub(" ", "_", species_name)
  
  bio_all <- pull_bio(
    common_name = species_name,
    survey = "NWFSC.Combo",
    standard_filtering = FALSE
  )
  
  # Only proceed if there are more than 100 observations
  if (nrow(bio_all) > 100) {
    
    bio_length <- bio_all[, c("Common_name", "Length_cm")]
    
    p <- ggplot(bio_length, aes(x = Length_cm)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.1, color = "black", fill = "grey") +
      theme_classic()
    
    computed_data <- ggplot_build(p)$data[[1]]
    
    max_density <- max(computed_data$density)
    computed_data$density_standardized <- computed_data$density / max_density
    
    density_data <- computed_data[, c("x", "density_standardized")]
    density_data$Common_name <- species_name_clean
    
    colnames(density_data) <- c("Length_cm", "standardized_density", "Common_name")
    
    density_data_all <- rbind(density_data_all, density_data)
    
    print(paste("Included:", species_name, "| n =", nrow(bio_all)))
  } else {
    print(paste("Skipped:", species_name, "| n =", nrow(bio_all)))
  }
}

p_overlay <- ggplot(density_data_all, aes(x = Length_cm, y = standardized_density, fill = Common_name)) +
  geom_col(width = 1, alpha = 0.5, position = "identity") +
  scale_fill_manual(values = rep("grey", length(unique(density_data_all$Common_name)))) +
  labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 per Species)",
       x = "Length (cm)",
       y = "Density (Standardized)") +
  theme_classic()

print(p_overlay)

###############################################################
density_data_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(density_data_all) <- c("Length_cm", "standardized_density", "Common_name")

for (i in 1:nrow(species_list)) {
#for (i in 1:10) {
  species_name <- species_list$species[i]
  species_name_clean <- gsub(" ", "_", species_name)
  
  bio_all <- pull_bio(
    common_name = species_name,
    survey = "NWFSC.Combo",
    standard_filtering = FALSE
  )
  
  # Only proceed if >100 obs AND common name contains "rockfish"
  if (nrow(bio_all) > 100 && grepl("rockfish", species_name, ignore.case = TRUE)) {
    
    bio_length <- bio_all[, c("Common_name", "Length_cm")]
    
    p <- ggplot(bio_length, aes(x = Length_cm)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.1, color = "black", fill = "grey") +
      theme_classic()
    
    computed_data <- ggplot_build(p)$data[[1]]
    
    max_density <- max(computed_data$density)
    computed_data$density_standardized <- computed_data$density / max_density
    
    density_data <- computed_data[, c("x", "density_standardized")]
    density_data$Common_name <- species_name_clean
    
    colnames(density_data) <- c("Length_cm", "standardized_density", "Common_name")
    
    density_data_all <- rbind(density_data_all, density_data)
    
    print(paste("Included:", species_name, "| n =", nrow(bio_all)))
  } else {
    print(paste("Skipped:", species_name, "| n =", nrow(bio_all)))
  }
  
  print(i)
}

p_overlay <- ggplot(density_data_all, aes(x = Length_cm, y = standardized_density, fill = Common_name)) +
  geom_col(width = 1, alpha = 0.2, position = "identity") +
  scale_fill_manual(values = rep("grey", length(unique(density_data_all$Common_name)))) +
  labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 per Species)",
       x = "Length (cm)",
       y = "Density (Standardized)") +
  theme_classic()+
  theme(legend.position = "none")

print(p_overlay)

#p_overlay_color <- ggplot(density_data_all, aes(x = Length_cm, y = standardized_density, fill = Common_name)) +
#  geom_col(width = 1, alpha = 0.5, position = "identity") +
#  labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 per Species)",
#       x = "Length (cm)",
#       y = "Density (Standardized)") +
#  theme_classic()+
#  theme(legend.position = "none")

#print(p_overlay_color)












