library(dplyr)
library(nwfscSurvey)
library(flextable)
library(ggplot2)

species_list <- read.csv("species_list_2025.csv")

species_all <- nwfscSurvey::pull_spp()
#sebastes_species <- species_all |> 
#  select(latin) |>
#  contains("Sebastes")

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
#have to add trawl ID

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
    
    #MAKE A FOLDER FOR THESE PLOTS
    
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

#### 
#same thing for depth
#have to have trawl ID in density table also
#then merge or maybe depth is already there
#DEPTH IS THERE, SO JUST DO ONE THAT IS DEPTH FREQUENCY DISTRIBTUION
#and another that is length by depth 


#############################################################################
###############################################################
############################################################
length_depth_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(length_depth_all) <- c("Scientific_name", "Length_cm", "Depth_m")

length_density_data_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(length_density_data_all) <- c("Length_cm", "length_standardized_density", "Scientific_name")

depth_density_data_all <- data.frame(matrix(ncol = 3, nrow = 0))
colnames(depth_density_data_all) <- c("Depth_m", "depth_standardized_density", "Scientific_name")

#to consider: pretty density plots like red hind paper

species_all <- nwfscSurvey::pull_spp()
species_Sebastes <- species_all %>%
  filter(grepl("Sebastes", latin))
#for (i in 1:nrow(species_list)) {
for (i in 1:nrow(species_Sebastes)) {
#  for (i in 1:10) {
  ##species_name <- species_list$species[i]
  species_name <- species_Sebastes$latin[i]
  species_name_write <- species_Sebastes$scientific_name[i]
  #species_name_clean <- gsub(" ", "_", species_name)
  
  bio_all_test <- tryCatch({
    
message("Trying pull_bio() for i =", i, " (",species_name,")") 
    
  bio_all <- pull_bio(
    #common_name = species_name,
    sci_name = species_name,
    survey = "NWFSC.Combo",
    standard_filtering = FALSE
   )
  
  # Only proceed if >100 obs AND common name contains "rockfish"
  ##if (nrow(bio_all) > 100 && grepl("rockfish", species_name, ignore.case = TRUE)) {
  if (nrow(bio_all) > 100 && grepl("Sebastes", species_name, ignore.case = TRUE)) {   
    bio_length_depth <- bio_all[, c("Scientific_name", "Length_cm", "Depth_m")]
    
    #rbind this to a larger dataframe for length by depth analysis
    length_depth_all <- rbind(length_depth_all, bio_length_depth)
    
    #MAKE A FOLDER FOR THESE PLOTS by species
    
    ##########LENGTH
    
    l <- ggplot(bio_length_depth, aes(x = Length_cm)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.1, color = "black", fill = "grey") +
      theme_classic()
    
    length_output_data <- ggplot_build(l)$data[[1]]
    
    length_max_density <- max(length_output_data$density)
    length_output_data$length_density_standardized <- length_output_data$density / length_max_density
    
    length_density_data <- length_output_data[, c("x", "length_density_standardized")]
    length_density_data$Scientific_name <- species_name
    
    colnames(length_density_data) <- c("Length_cm", "length_standardized_density", "Scientific_name")
    
    length_density_data_all <- rbind(length_density_data_all, length_density_data)
    
    ####### DEPTH ##########
    
    d <- ggplot(bio_length_depth, aes(x = Depth_m)) +
      geom_histogram(aes(y = after_stat(density)), binwidth = 10, alpha = 0.1, color = "black", fill = "grey") +
      theme_classic()
    
    depth_output_data <- ggplot_build(d)$data[[1]]
    
    depth_max_density <- max(depth_output_data$density)
    depth_output_data$depth_density_standardized <- depth_output_data$density / depth_max_density
    
    depth_density_data <- depth_output_data[, c("x", "depth_density_standardized")]
    depth_density_data$Scientific_name <- species_name
    
    colnames(depth_density_data) <- c("Depth_m", "depth_standardized_density", "Scientific_name")
    
    depth_density_data_all <- rbind(depth_density_data_all, depth_density_data)
    
    
    print(paste("Included:", species_name, "| n =", nrow(bio_all)))
  } else {
    print(paste("Skipped:", species_name, "| n =", nrow(bio_all)))
  }
  
  
  }, error = function(e) {
    message(paste("Pull_bio() not available for",species_name))
    
  })  
  
  
  print(i)
}

l_overlay <- ggplot(length_density_data_all, aes(x = Length_cm, y = length_standardized_density, fill = Scientific_name)) +
  geom_col(width = 1, alpha = 0.2, position = "identity") +
  scale_fill_manual(values = rep("grey", length(unique(length_density_data_all$Scientific_name)))) +
  labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 each species)",
       x = "Length (cm)",
       y = "Density (standardized)") +
  theme_classic()+
  theme(legend.position = "none")

print(l_overlay)

d_overlay <- ggplot(depth_density_data_all, aes(x = Depth_m, y = depth_standardized_density, fill = Scientific_name)) +
  geom_col(width = 10, alpha = 0.2, position = "identity") +
  scale_fill_manual(values = rep("grey", length(unique(depth_density_data_all$Scientific_name)))) +
  labs(title = "Overlayed Standardized Depth Frequency Histograms (Max Density = 1 each species)",
       x = "Depth (m)",
       y = "Density (standardized)") +
  theme_classic()+
  theme(legend.position = "none")

print(d_overlay)

l_d <- ggplot(length_depth_all, aes(x = Depth_m, y = Length_cm, fill = Scientific_name)) +
  geom_point()
  geom_col(width = 10, alpha = 0.2, position = "identity") +
  scale_fill_manual(values = rep("grey", length(unique(depth_density_data_all$Scientific_name)))) +
  labs(title = "Overlayed Standardized Depth Frequency Histograms (Max Density = 1 each species)",
       x = "Depth (m)",
       y = "Density (standardized)") +
  theme_classic()+
  theme(legend.position = "none")

##############################################
##############################################
###############################################
  
  species_all <- nwfscSurvey::pull_spp()
  species_Sebastes <- species_all %>%
    filter(grepl("Sebastes", latin))
  
  # Initialize combined data frames
  length_depth_all <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(length_depth_all) <- c("Scientific_name", "Length_cm", "Depth_m")
  
  length_density_data_all <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(length_density_data_all) <- c("Length_cm", "length_standardized_density", "Scientific_name")
  
  depth_density_data_all <- data.frame(matrix(ncol = 3, nrow = 0))
  colnames(depth_density_data_all) <- c("Depth_m", "depth_standardized_density", "Scientific_name")
  
  #to consider: pretty density plots like red hind paper!!!!!!!!!!!!!!
  
  for (i in 1:nrow(species_Sebastes)) {
    species_name <- species_Sebastes$latin[i]
    species_name_write <- species_Sebastes$scientific_name[i]
    
    message("Trying pull_bio() for i =", i, " (", species_name, ")")
    
    # Wrap only the pull_bio() call in tryCatch
    bio_all <- tryCatch({
      pull_bio(
        sci_name = species_name,
        survey = "NWFSC.Combo",
        standard_filtering = FALSE
      )
    }, error = function(e) {
      message(paste("pull_bio() not available for", species_name, ":", e$message))
      return(NULL)
    })
    
    # Skip iteration if pull_bio() failed
    if (is.null(bio_all)) {
      next
    }
    
    # Check conditions before processing
    if (nrow(bio_all) > 100 && grepl("Sebastes", species_name, ignore.case = TRUE)) {
      bio_length_depth <- bio_all[, c("Scientific_name", "Length_cm", "Depth_m")]
      length_depth_all <- rbind(length_depth_all, bio_length_depth)
      
      # Length plot
      l <- ggplot(bio_length_depth, aes(x = Length_cm)) +
        geom_histogram(aes(y = after_stat(density)), binwidth = 1, alpha = 0.1, color = "black", fill = "grey") +
        theme_classic()
      length_output_data <- ggplot_build(l)$data[[1]]
      length_output_data$length_density_standardized <- length_output_data$density / max(length_output_data$density)
      length_density_data <- length_output_data[, c("x", "length_density_standardized")]
      length_density_data$Scientific_name <- species_name
      colnames(length_density_data) <- c("Length_cm", "length_standardized_density", "Scientific_name")
      length_density_data_all <- rbind(length_density_data_all, length_density_data)
      
      # Depth plot
      d <- ggplot(bio_length_depth, aes(x = Depth_m)) +
        geom_histogram(aes(y = after_stat(density)), binwidth = 10, alpha = 0.1, color = "black", fill = "grey") +
        theme_classic()
      depth_output_data <- ggplot_build(d)$data[[1]]
      depth_output_data$depth_density_standardized <- depth_output_data$density / max(depth_output_data$density)
      depth_density_data <- depth_output_data[, c("x", "depth_density_standardized")]
      depth_density_data$Scientific_name <- species_name
      colnames(depth_density_data) <- c("Depth_m", "depth_standardized_density", "Scientific_name")
      depth_density_data_all <- rbind(depth_density_data_all, depth_density_data)
      
      print(paste("Included:", species_name, "| n =", nrow(bio_all)))
    } else {
      print(paste("Skipped:", species_name, "| n =", nrow(bio_all)))
    }
    
    print(i)
  }

  #41 rockfish species (plus just "Sebastes")
  l_overlay <- ggplot(length_density_data_all, aes(x = Length_cm, y = length_standardized_density, fill = Scientific_name)) +
    geom_col(width = 1, alpha = 0.2, position = "identity") +
    scale_fill_manual(values = rep("grey", length(unique(length_density_data_all$Scientific_name)))) +
    labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 each species)",
         x = "Length (cm)",
         y = "Density (standardized)") +
    theme_classic()+
    theme(legend.position = "none")
  
  print(l_overlay)
  
  length_density_filtered <- length_density_data_all %>%
    group_by(Length_cm) %>%
    filter(n_distinct(Scientific_name) > 5) %>%
    ungroup()
  
  l_overlay_noless5 <- ggplot(length_density_filtered, aes(x = Length_cm, y = length_standardized_density, fill = Scientific_name)) +
    geom_col(width = 1, alpha = 0.2, position = "identity") +
    scale_fill_manual(values = rep("grey", length(unique(length_density_data_all$Scientific_name)))) +
    labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 each species)",
         x = "Length (cm)",
         y = "Density (standardized)") +
    theme_classic()+
    theme(legend.position = "none")
  
  #we want to select TRAWLS 
  
  length_filtered_depth <- length_depth_all %>%
    filter(Depth_m > 225 & Depth_m < 300)
  
  l_overlay_untrawlable <- ggplot(length_filtered_depth, aes(x = Length_cm)) +
    geom_histogram(binwidth = 1)+
    theme_classic()
    geom_col(width = 1, alpha = 0.2, position = "identity") +
    scale_fill_manual(values = rep("grey", length(unique(length_density_data_all$Scientific_name)))) +
    labs(title = "Overlayed Standardized Length Frequency Histograms (Max Density = 1 each species)",
         x = "Length (cm)",
         y = "Density (standardized)") +
    theme_classic()+
    theme(legend.position = "none")
  
    
  d_overlay <- ggplot(depth_density_data_all, aes(x = Depth_m, y = depth_standardized_density, fill = Scientific_name)) +
    geom_col(width = 10, alpha = 0.2, position = "identity") +
    scale_fill_manual(values = rep("grey", length(unique(depth_density_data_all$Scientific_name)))) +
    labs(title = "Overlayed Standardized Depth Frequency Histograms (Max Density = 1 each species)",
         x = "Depth (m)",
         y = "Density (standardized)") +
    theme_classic()+
    theme(legend.position = "none")
  
  print(d_overlay)
  
  l_d <- ggplot(length_depth_all, aes(x = Depth_m, y = Length_cm)) +
    geom_point(shape = 16, fill = "black", stroke = 0, alpha = 0.005)+ #0.01 not bad
    theme_classic()+
    theme(legend.position = "none")
  




